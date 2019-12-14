{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ExactlyOne where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

data ExactlyOne a = ExactlyOne a deriving (Eq, Show)

runExactlyOne :: ExactlyOne a -> a
runExactlyOne (ExactlyOne a) = a

-- | like map in scala
mapExactlyOne :: (a -> b) -> ExactlyOne a -> ExactlyOne b
mapExactlyOne f (ExactlyOne a)    = ExactlyOne (f a)

-- | P.fmap (+3) (ExactlyOne 10)
-- return ExactlyOne 13
bindExactlyOne :: (a -> ExactlyOne b) -> ExactlyOne a -> ExactlyOne b
bindExactlyOne f (ExactlyOne a) = f a

-- | (ExactlyOne (\n -> (n + 3))) A.<*> (ExactlyOne 10)
-- return ExactlyOne 13
-- | (ExactlyOne (+3)) A.<*> (ExactlyOne 10)
-- return ExactlyOne 13
instance P.Functor ExactlyOne where
  fmap =
    M.liftM

-- | (\n -> (ExactlyOne (n+3))) M.=<< (ExactlyOne 10)
-- return ExactlyOne 13
-- | (ExactlyOne 10) M.>>= (\n -> (ExactlyOne (n+3)))
-- return ExactlyOne 13
instance A.Applicative ExactlyOne where
  (<*>) =
    M.ap
  pure =
    ExactlyOne

instance P.Monad ExactlyOne where
  (>>=) =
    flip bindExactlyOne
  return =
    ExactlyOne

