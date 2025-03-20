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
import Data.Text (Text)
import Prelude hiding (fail)
import System.IO.Unsafe

data EvalTree = Eval [EvalTreeRef] | Uneval (IORef (IO ()))

type EvalTreeRef = IORef EvalTree

mkEvalTreeCons :: EvalTreeRef -> Int -> IO [EvalTreeRef]
mkEvalTreeCons r n = do refs <- sequence (replicate n emptyUnevalRef)
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
  observe [] r = unsafePerformIO $ do mkEvalTreeCons r 0
                                      pure []

mkObserve :: Observe a => a -> IO (EvalTreeRef, a)
mkObserve x = do r <- emptyUnevalRef
                 pure (r, observe x r)

newtype Lazy a = Lazy (EvalTree, a)

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

leq :: Lazy Int -> Lazy Int -> Maybe Bool
leq (Lazy (Eval [], x)) (Lazy (Eval [], y)) = Just $ x <= y
leq _ _                                     = Nothing

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

fromTry (Try x) = x

fail :: Try a
fail = Try (\sc fc -> fc)

suspend :: IORef (IO ()) -> Try a -> SuccCont a -> FailCont -> IO ()
suspend aRef try sc fc = do io <- readIORef aRef
                            writeIORef aRef (io >> (fromTry try) sc fc)

parDisjunction :: Try a -> Try a -> Try a
(Try xs) `parDisjunction` (Try ys) = Try (xs ++ ys)

(|||) :: Try a -> Try a -> Try a
(|||) = parDisjunction

nil :: Lazy [a] -> Try ()
nil (Lazy (Eval _, [])) = pure ()
nil (Lazy (Eval _, _:_)) = fail
nil rx@(Lazy (Uneval aRef, v)) = Try (suspend aRef (nil rx))

cons :: Lazy [a] -> Try (Lazy a, Lazy [a])
cons (Lazy (Eval [subTermX, subTermY], x:xs)) = pure (Lazy (subTermX, x), Lazy (subTermY, xs))
cons (Lazy (Eval _, [])) = fail
cons rx@(Lazy (Uneval aRef, v)) = Try $ suspend aRef (cons rx)

val :: Lazy a -> Try a
val (Lazy (subTerm, v)) = condEval subTerm (pure v)
  where
    condEval :: EvalTree -> Try a -> Try a
    condEval (Eval st) termValue = foldr condEval termValue st
    condEval Uneval _ = suspend

parApply :: Try (a -> b) -> Try a -> Try b
parApply (Try fs) (Try xs) = Try [ res
                                 | fRes <- fs, xRes <- xs,
                                   let res = do f <- fRes
                                                f <$> xRes
                                 ]
(***) :: Try (a -> b) -> Try a -> Try b
(***) = parApply

type Assert = Try ()

parConj :: Assert -> Assert -> Assert
evalTreeX `parConj` evalTreeY = (pure (\_ _ -> ()) *** evalTreeX) *** evalTreeY

(&&&) :: Assert -> Assert -> Assert
(&&&) = parConj

assert :: Text -> (Lazy a -> Try ()) -> a -> a
assert = undefined
