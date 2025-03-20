{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.Text (Text)
import Prelude hiding (fail)

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
      applyResult g (Result x) = g x
      applyResult _ Suspension = [Suspension]

fail :: Try a
fail = Try []

suspend :: Try a
suspend = Try [Suspension]

parDisjunction :: Try a -> Try a -> Try a
(Try xs) `parDisjunction` (Try ys) = Try (xs ++ ys)

(|||) :: Try a -> Try a -> Try a
(|||) = parDisjunction

nil :: Lazy [a] -> Try ()
nil (Lazy (Eval _, v)) = unless (null v) fail
nil (Lazy (Uneval, _)) = suspend

cons :: Lazy [a] -> Try (Lazy a, Lazy [a])
cons (Lazy (Eval [subTermX, subTermY], x:xs)) = pure (Lazy (subTermX, x), Lazy (subTermY, xs))
cons (Lazy (Eval _, _)) = fail
cons (Lazy (Uneval, _)) = suspend

val :: Lazy a -> Try a
val (Lazy (subTerm, v)) = condEval subTerm (pure v)

condEval :: EvalTree -> a -> a
condEval (Eval subTerm) termValue = foldr condEval termValue subTerm
condEval Uneval _ = suspend

assert :: Text -> (Lazy a -> Try ()) -> a -> a
assert = undefined
