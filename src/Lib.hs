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

import Data.Text (Text)

data EvalTree = Eval [EvalTree] | Uneval
  deriving stock Show

newtype Lazy a = Lazy (EvalTree, a)

data Computation a = Result a | Suspension
  deriving stock (Eq, Show)

instance Applicative Computation where
  pure = Result

  (Result f) <*> Result x   = Result $ f x
  (Result _) <*> Suspension = Suspension
  Suspension <*> _          = Suspension

deriving instance Functor Computation

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

newtype Try a = Try [Computation a]
  deriving stock (Eq, Show)

deriving instance Functor Try

instance Applicative Try where
  pure x = Try [Result x]

  fs <*> xs = do
    f <- fs
    f <$> xs

-- TODO (james): simplify in terms of Applicative
instance Monad Try where
  (Try as) >>= f = Try $ concatMap (applyResult (fromTry . f)) as
    where
      fromTry (Try x) = x

      applyResult :: (a -> [Computation b]) -> Computation a -> [Computation b]
      applyResult f (Result x) = f x
      applyResult f Suspension = [Suspension]

fail :: Try ()
fail = Try []

suspend :: Try ()
suspend = Try [Suspension]

parDisjunction :: Try a -> Try a -> Try a
(Try xs) `parDisjunction` (Try ys) = Try (xs ++ ys)

(|||) :: Try a -> Try a -> Try a
(|||) = parDisjunction

nil :: Lazy [a] -> Try ()
nil = undefined

cons :: Lazy [a] -> Try (Lazy a, Lazy [a])
cons = undefined

val :: Lazy a -> Try a
val = undefined

assert :: Text -> (Lazy a -> Try ()) -> a -> a
assert = undefined
