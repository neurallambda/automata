{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.List.Extra where

import Data.Aeson (Value(..), FromJSON, parseJSON, ToJSON (..))
import Data.List (foldl')

--------------------------------------------------
-- * MatchAny: a helper class to allow searching lists with wildcards

-- | A "wildcard" match, useful for doing lookups in lists where the element might
-- contain `Any a`.
data Any a = A !a | Any
  deriving (Eq, Ord, Show)

instance (FromJSON a) => FromJSON (Any a) where
  parseJSON (String "ANY") = pure Any
  parseJSON x = A <$> parseJSON x

instance (ToJSON a) => ToJSON (Any a) where
  toJSON Any = String "ANY"
  toJSON (A x) = toJSON x

-- | Search a list where elements can have wildcard values and stop after the first match
lookupMatchAny :: MatchAny a => a -> [a] -> Maybe a
lookupMatchAny key xs = foldl' go (const Nothing) xs key
  where
    -- fold w continuation allows early return
    go cont x k
      | matchAny k x = Just x
      | otherwise = cont k

class MatchAny a where
    matchAny :: a -> a -> Bool

instance Eq a => MatchAny (Any a) where
    matchAny (A x) (A y) = x == y
    matchAny Any _ = True
    matchAny _ Any = True

instance (MatchAny a, MatchAny b) => MatchAny (a, b) where
    matchAny (a1, b1) (a2, b2) = matchAny a1 a2 && matchAny b1 b2

instance (MatchAny a, MatchAny b, MatchAny c) => MatchAny (a, b, c) where
    matchAny (a1, b1, c1) (a2, b2, c2) = matchAny a1 a2 && matchAny b1 b2 && matchAny c1 c2

instance (MatchAny a) => MatchAny (Maybe a) where
    matchAny (Just a) (Just b) = matchAny a b
    matchAny Nothing Nothing = True
    matchAny _ _ = False
