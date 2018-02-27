module Polynomials where

import Data.List (union)
import qualified Data.Set.Internal as S

type Variable = String

data MonF2 = M (S.Set Variable) deriving (Eq, Ord)

instance Show MonF2 where
 show (M S.Tip) = "1"
 show (M xs)   = (concat . S.elems) xs

data PolF2 = P (S.Set MonF2)
 deriving (Eq, Ord)

instance Show PolF2 where
 show (P S.Tip) = "0"
 show (P xs)  = showAux xs

showAux xs = foldr (\x acc -> acc ++ "+" ++ (show x)) (show a) ((S.elems) xs')
  where (a,xs') = S.deleteFindMin xs

instance Num PolF2 where
  (+) = suma 
  (*) = producto
  (-) = suma
  abs = undefined
  signum = undefined
  fromInteger = undefined

suma :: PolF2 -> PolF2 -> PolF2
suma (P p1) (P p2) = P $ S.foldr (\x acc -> if S.member x acc then (S.delete x acc) else (S.insert x acc)) p2 p1

producto :: PolF2 -> PolF2 -> PolF2
producto (P p1) (P p2) = P $ S.foldr (\(M m) acc -> (S.union (S.map (\(M x) -> M (S.union x m)) p2) acc)) S.empty p1

vars :: PolF2 -> [Variable]
vars (P p) = S.toAscList $ foldr (\(M m) acc -> (foldr (\x acc2 -> S.insert x acc2) acc m)) S.empty p

varsKB :: S.Set PolF2 -> [Variable]
varsKB ps = S.foldr (\x acc -> union acc (vars x)) [] ps

zero :: PolF2
zero = P S.Tip

one :: PolF2
one = P $ S.singleton $ M S.Tip

var :: Variable -> PolF2
var x = P $ S.singleton $ M $ S.singleton x