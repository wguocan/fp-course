{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a =
  Full a
  | Empty
  deriving (Eq, Show)

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
mapOptional ::
  (a -> b)
  -> Optional a
  -> Optional b
mapOptional _ Empty =
  Empty
mapOptional f (Full a) =
  Full (f a)


-- | Bind the given function on the possible value.
--
-- >>> bindOptional Full Empty
-- Empty
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- Full 7
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- Full 10
bindOptional ::
  (a -> Optional b)
  -> Optional a
  -> Optional b
bindOptional _ Empty =
  Empty
bindOptional f (Full a) =
  f a


-- | Return the possible value if it exists; otherwise, the second argument.
--
-- >>> Full 8 ?? 99
-- 8
--
-- >>> Empty ?? 99
-- 99
(??) ::
  Optional a
  -> a
  -> a
(??) Empty a =
  a
(??) (Full a) _ =
  a

-- | Try the first optional for a value. If it has a value, use it; otherwise,
-- use the second value.
--
-- >>> Full 8 <+> Empty
-- Full 8
--
-- >>> Full 8 <+> Full 9
-- Full 8
--
-- >>> Empty <+> Full 9
-- Full 9
--
-- >>> Empty <+> Empty
-- Empty
(<+>) ::
  Optional a
  -> Optional a
  -> Optional a
(<+>) Empty Empty =
  Empty
(<+>) Empty (Full a) =
  Full a
(<+>) (Full a) _ =
  Full a

-- | Replaces the Full and Empty constructors in an optional.
--
-- >>> optional (+1) 0 (Full 8)
-- 9
--
-- >>> optional (+1) 0 Empty
-- 0
optional ::
  (a -> b)
  -> b
  -> Optional a
  -> b
optional _ a Empty =
  a
optional f _ (Full a) =
  f a

-- | take both f and data from the box -- apply f to the data -- put the result to the box
-- | applyOptional (Full (+3)) (Full 20) >>> Full 23
-- | applyOptional (Full (+3)) $ Full 20 >>> Full 23
--- TODO: check the implementation
applyOptional :: Optional (a -> b) -> Optional a -> Optional b
applyOptional f a = bindOptional (\f' -> mapOptional f' a) f

-- | take data from the box -- apply f to the data -- put the result back to the box
--- twiceOptional (+) (Full 20) (Full 30) >>> Full 50
--- twiceOptional (:.) (Full 20) (Full (30:.31:.Nil)) >> Full [20,30,31]
--- twiceOptional (++) (Full (20:.21:.Nil)) (Full (30:.31:.Nil)) >>> Full [20,21,30,31]
twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional . mapOptional f

-- | contains 20 $ Full 20
---- True
-- | contains 20 $ Full 21
---- False
contains :: Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

-- | P.fmap (+1) (Full 9)
instance P.Functor Optional where
  fmap =
    M.liftM

-- | (Full (+3)) A.<*> (Full 9)
instance A.Applicative Optional where
  (<*>) =
    M.ap
  pure =
    Full

-- | (Full 9) M.>>= (\n -> Full (n + 3))
-- | (\n -> (Full (n + 3))) M.=<< (Full 9)
instance P.Monad Optional where
  (>>=) =
    flip bindOptional
  return =
    Full
