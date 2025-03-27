{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

{-|
Module     : Lib
Maintainer : James King <james@agentultra.com>

An implementation of /Monadic, Prompt Lazy Assertions for Haskell/:

https://kar.kent.ac.uk/14528/1/monadicAssertions.pdf
-}

module Lib where

import Control.Monad hiding (fail)
import Data.IORef
import Prelude hiding (fail)
import System.IO.Unsafe

data EvalTree = Eval [IORef EvalTree] | Uneval (IORef (IO ()))

type EvalTreeRef = IORef EvalTree

mkEvalTreeCons :: EvalTreeRef -> Int -> IO [EvalTreeRef]
mkEvalTreeCons r n = do refs <- replicateM n emptyUnevalRef
                        Uneval aRef <- readIORef r
                        action <- readIORef aRef
                        writeIORef r (Eval refs)
                        action
                        pure refs

emptyUnevalRef :: IO EvalTreeRef
emptyUnevalRef = do aRef <- newIORef (pure ())
                    newIORef (Uneval aRef)

class Observe a where
  observe :: a -> EvalTreeRef -> a

instance Observe a => Observe [a] where
  observe (x:xs) r = unsafePerformIO $ do [aRef, bRef] <- mkEvalTreeCons r 2
                                          pure (observe x aRef : observe xs bRef)
  observe [] r = unsafePerformIO $ do _ <- mkEvalTreeCons r 0
                                      pure []

mkObserve :: Observe a => a -> IO (EvalTreeRef, a)
mkObserve x = do r <- emptyUnevalRef
                 pure (r, observe x r)

newtype Lazy a = Lazy (EvalTreeRef, a)

data Computation a = Result a | Suspension
  deriving stock (Eq, Show)

instance Applicative Computation where
  pure = Result

  (Result f) <*> Result x   = Result $ f x
  (Result _) <*> Suspension = Suspension
  Suspension <*> _          = Suspension

deriving instance Functor Computation

instance Monad Computation where
  (Result x) >>= f = f x
  Suspension >>= _ = Suspension

parConjunction :: Computation Bool -> Computation Bool -> Computation Bool
(Result True)  `parConjunction` (Result True)  = Result True
(Result False) `parConjunction` _              = Result False
_              `parConjunction` (Result False) = Result False
_              `parConjunction` _              = Suspension

(&|&) :: Computation Bool -> Computation Bool -> Computation Bool
(&|&) = parConjunction

type FailCont = IO ()
type SuccCont a = FailCont -> a -> IO ()

newtype Try a = Try (SuccCont a -> FailCont -> IO ())

deriving instance Functor Try

instance Applicative Try where
  pure x = Try (\sc fc -> sc fc x)

  fs <*> xs = do
    f <- fs
    f <$> xs

instance Monad Try where
  (Try asIO) >>= f = Try (\sc fc -> asIO (\sfc x -> fromTry (f x) sc sfc) fc)

fromTry :: Try a -> SuccCont a -> FailCont -> IO ()
fromTry (Try x) = x

fail :: Try a
fail = Try (\_ fc -> fc)

suspend :: IORef (IO ()) -> Try a -> SuccCont a -> FailCont -> IO ()
suspend aRef try sc fc = do io <- readIORef aRef
                            writeIORef aRef (io >> fromTry try sc fc)

parDisjunction :: Try a -> Try a -> Try a
(Try x) `parDisjunction` (Try y) = Try $ \sc fc -> do
  ref <- newIORef True
  x sc (orIORef ref fc) >> y sc (orIORef ref fc)

orIORef :: IORef Bool -> FailCont -> IO ()
orIORef ref fc = do v <- readIORef ref
                    if v then writeIORef ref False
                      else fc

(|||) :: Try a -> Try a -> Try a
(|||) = parDisjunction

parApply :: Try (a -> b) -> Try a -> Try b
parApply (Try f) (Try x) = Try $ \sc fc -> do
  fRef <- newIORef []
  xRef <- newIORef []
  ref <- newIORef True
  f (\ffc f' -> do modifyIORef fRef ((ffc, f'):)
                   xs <- readIORef xRef
                   mapM_ (\(xfc, x') -> sc (ffc >> xfc) (f' x')) xs)
    (andIORef ref fc)
  x (\xfc x' -> do modifyIORef xRef ((xfc, x'):)
                   fs <- readIORef fRef
                   mapM_ (\(ffc, f') -> sc (ffc >> xfc) (f' x')) fs)
    (andIORef ref fc)

andIORef :: IORef Bool -> IO () -> IO ()
andIORef ref fc = do v <- readIORef ref
                     when v $ writeIORef ref False >> fc

(***) :: Try (a -> b) -> Try a -> Try b
(***) = parApply

type Assert = Try ()

parConj :: Assert -> Assert -> Assert
evalTreeX `parConj` evalTreeY = (pure (\_ _ -> ()) *** evalTreeX) *** evalTreeY

(&&&) :: Assert -> Assert -> Assert
(&&&) = parConj

assert :: Observe a => String -> (Lazy a -> Assert) -> a -> a
assert label p x = unsafePerformIO $ do
  (eT, x') <- mkObserve x
  let Try check = p $ Lazy (eT, x')
  check (\_ _ -> putStrLn $ "Assertion passed: " ++ label)
        (error $ "Assertion failed: " ++ label)
  pure x
