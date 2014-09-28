{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.HValue.Plain(
    HValue, mkHValue,
    get, set
    ) where

data HValue = IntV Int | StringV String | CharV Char
    deriving (Read, Show)

class Helper a where
    mkHValue :: a -> HValue
    get :: HValue -> Maybe a

instance Helper Int where
    mkHValue = IntV
    get (IntV i) = Just i
    get _        = Nothing

instance Helper String where
    mkHValue = StringV
    get (StringV s) = Just s
    get _           = Nothing

instance Helper Char where
    mkHValue = CharV
    get (CharV c) = Just c
    get _         = Nothing

-- Version with {-# LANGUAGE ScopedTypeVariables #-}
-- set :: forall a . Helper a => a -> HValue -> Maybe HValue
-- set v = fmap replace . get
--     where replace :: a -> HValue
--           replace _ = mkHValue v

set :: Helper a => a -> HValue -> Maybe HValue
set v = fmap (replace v) . get

replace :: Helper a => a -> a -> HValue
replace v _ = mkHValue v
