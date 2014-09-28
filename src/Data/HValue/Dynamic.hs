
module Data.HValue.Dynamic(
    HValue, hValue,
    get, set
    ) where

import Data.Dynamic

newtype HValue = HValue { getHValue :: Dynamic }

hValue :: Typeable a => a -> HValue
hValue = HValue . toDyn

get :: Typeable a => HValue -> Maybe a
get = fromDynamic . getHValue

set :: Typeable a => a -> HValue -> Maybe HValue
set a (HValue d)
    | typeOf a == dynTypeRep d = Just $ hValue a
    | otherwise                = Nothing

-- It is impossible to implement Show or Read classes as constraint
-- of readability or showability can not be expressed with the Dynamic
-- type.
--
-- Smart constructor hValue could add this constraint but it would require
-- unsafe cast during show/read as the type system can not know that
-- fromDynamic . toDyn = Id
