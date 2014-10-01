{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- This helped a lot:
--   http://www.haskell.org/pipermail/haskell-prime/2011-May/003416.html

-- TODO check that all types in HValue are diferent

-- module FunctionalDependencies & type-level recursion
module Data.HValue.FDAndTLR(
    HValue, mkHValue,
    get, set
    ) where

data End v = End v
    deriving (Show, Read)

data HValue a b = Head a | Tail b
    deriving (Show, Read, Eq, Ord)

class Accessor hv v | hv -> v where
    mkHValue :: v -> hv
    get :: hv -> Maybe v
--    set :: hv -> v -> Maybe (hv)

instance Accessor (End v) v where
    mkHValue = End
    get (End v) = Just v
--    set _ _ = Nothing

instance Accessor (HValue v t) v where
    mkHValue = Head
    get (Head v) = Just v  -- Containing type v => return the value
    get _        = Nothing -- Containing an other type
--    set (Head v) v' = Just $ Head v'
--    set _ _         = Nothing

instance (Accessor t v) => Accessor (HValue h t) v where
    mkHValue = Tail . mkHValue
    get (Head _) = Nothing -- Containing type h, not v
    get (Tail t) = get t   -- Delegate to tail
--    set (Head a) v = Nothing
--    set (Tail t) v = set t v

set :: Accessor hv v => v -> hv -> Maybe hv
set v = fmap (replace v) . get

replace :: Accessor hv v => v -> v -> hv
replace v _ = mkHValue v

-- TODO use a real UT framework
end :: End Int
end = End 1 -- End 1
end' :: End String
end' = mkHValue "1" -- End "1"

test :: HValue Int (End String)
test = Head 12 -- Head 12
test' :: HValue Int (End String)
test' = mkHValue 123 -- Head 123
test'' :: HValue Int (End String)
test'' = mkHValue "123" -- Tail (End "123")

testGet :: Maybe Int
testGet = get end -- Just 1
testGet' :: Maybe String
testGet' = get test' -- Nothing
testGet'' :: Maybe String
testGet'' = get test'' -- Just "123"

testSet :: Maybe (HValue Int (End String))
testSet = set 1 test -- Just * 1
testSet' :: Maybe (HValue Int (End String))
testSet' = set "111" test' -- Nothing
testSet'' :: Maybe (HValue Int (End String))
testSet'' = set "111" test''-- Just * "111"
