{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.HValue.Existential(
    Setable, Getable,
    HValue, hValue,
    Pretty(..)
    ) where

import Data.Typeable

-- Classes
class Setable hv where
    set :: (Show a, Typeable a) => a -> hv -> Maybe hv

class Getable hv where
    get :: Typeable a => hv -> Maybe a

-- Data
data HValue = forall a . (Show a, Typeable a) => HValue a

-- Constructor
hValue :: forall a . (Show a, Typeable a) => a -> HValue
hValue = HValue

-- Instances
instance Setable HValue where
    set a (HValue v)
        | typeOf a == typeOf v = Just $ HValue a
        | otherwise            = Nothing

instance Getable HValue where
    get (HValue v) = cast v

instance Show HValue where
  showsPrec p (HValue a) = showsPrec p a

-- There is no way to write Read instance as a in (HValue a)
-- can be of an type. Thus all type should be tested to deserialized
-- a string.

-- show = type + value version
newtype Pretty = Pretty HValue
    deriving (Setable, Getable)

instance Show Pretty where
    showsPrec p (Pretty (HValue a)) =
        showsPrec p (a) . (" :: " ++) . showsPrec p (typeOf a)
