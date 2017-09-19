-- |This module contains several examples in order to verify the good
-- functioning of the other modules.
module Examples where

import PolAux

import Data.List (nub,iterate,partition,foldl')
import qualified Data.Set as S

import System.Environment

import qualified Data.Set as S

-------------------------------------------------------------------------
clause2pol :: [String] -> (PolF2, S.Set (PolF2))
clause2pol (c:cs) | c == "c" || c == "p" = (1, S.empty)
clause2pol cs = aux $ foldl' (\acc x -> disj (var' x) acc) (zerov,S.empty) cs
  where aux (a,b) = (expTo1 a,b)
        disj (x,v) (y,vs) | v == zerov = (y,vs)
                          | otherwise   = (x + y + x*y, S.insert v vs)

insertPol (a,b) (acc,vs) = (S.insert  a acc, S.union vs b)

var' :: String -> (PolF2,PolF2)
var' "0"       = (zerov,zerov)
var' ('-':lit) = (1 + x,x)
                   where x = var ('x':lit)
var' lit       = (x,x)
                   where x = var ('x':lit)

fileSat0 = ["c This Formular is generated by mcnf","c","c    horn? no ","c    forced? no ","c    mixed sat? no ","c    clause length = 3 ","c","p cnf ? ?","30 -19 -3 0","89 31 -42 0","1 10 38 0","2 -21 -98 0","36 56 -78 0","14 -59 -87 0","89 -75 -86 0","-20 -80 4 0","-63 90 -55 0","59 75 9 0","-5 31 -97 0","48 -35 58 0","28 84 16 0","65 -4 94 0","-72 -23 7 0","18 -64 -55 0","-96 3 -35 0","89 -65 83 0","8 -60 -28 0","34 50 -14 0","64 -47 -35 0","-19 57 93 0","52 99 -94 0","-59 10 48 0","-78 -44 76 0","-63 -48 52 0","-88 84 -68 0","10 88 -76 0","92 -81 -88 0","66 44 42 0","-45 -98 74 0","-8 -84 -24 0","53 -75 -48 0","-6 70 50 0","90 75 -60 0","58 -60 -93 0","-71 64 73 0","54 92 -12 0","70 25 -59 0","-36 7 -42 0","-90 96 -16 0","72 -97 -35 0","-81 44 -66 0","82 14 10 0","8 -69 -33 0","6 54 -67 0","92 -8 -91 0","-99 32 -25 0","-50 5 83 0","85 53 84 0","-90 7 1 0","50 19 -44 0","21 43 27 0","-13 57 -55 0","-11 100 -52 0","42 -7 -60 0","78 -13 6 0","16 73 -92 0","92 30 -55 0","-47 36 -70 0","52 71 -95 0","99 90 70 0","53 -26 95 0","-43 59 -42 0","67 51 2 0","25 1 -19 0","10 2 -45 0","66 38 -60 0","52 -63 -13 0","-16 -19 80 0","-77 -68 11 0","70 89 25 0","80 -68 -75 0","11 -28 41 0","-1 92 -27 0","91 21 61 0","25 -84 28 0","-86 77 -97 0","-39 26 -1 0","88 49 69 0","-7 20 33 0","6 -45 32 0"]

fileSat00 = (foldr (\x acc -> (insertPol ((clause2pol . words) x) acc)) (S.empty,S.empty)) fileSat0

-------------------------------------------------------------------------
var'' :: String -> PolF2
var'' "0"       = zerov
var'' ('-':lit) = 1 + var ('x':lit)
var'' lit       = var ('x':lit)

clause2pol' :: [String] -> PolF2
clause2pol' (c:cs) | c == "c" || c == "p" = 1
clause2pol' cs = expTo1 $ foldl' (\acc x -> disj (var'' x) acc) zerov cs
  where disj x y = x + y + x*y

variable2List (a,b) = (a,S.toList b)

fileSat0'   = (foldr (\x acc -> (S.insert ((clause2pol' . words) x) acc)) S.empty) fileSat0
fileSat0''  = (foldr (\x acc -> (insertPol ((clause2pol . words) x) acc)) (S.empty,S.empty)) fileSat0
fileSat0''' = variable2List fileSat0''


-------------------------------------------------------------------------

fileSat1 = ["c This Formular is generated by mcnf","c","c    horn? no ","c    forced? no ","c    mixed sat? no ","c    clause length = 3 ","c","p cnf ? ?","30 -19 -3 0","89 31 -42 0","1 10 38 0","2 -21 -98 0","36 56 -78 0","14 -59 -87 0","89 -75 -86 0","-20 -80 4 0","-63 90 -55 0","59 75 9 0","-5 31 -97 0","48 -35 58 0","28 84 16 0","65 -4 94 0","-72 -23 7 0","18 -64 -55 0","-96 3 -35 0","89 -65 83 0","8 -60 -28 0","34 50 -14 0","64 -47 -35 0","-19 57 93 0","52 99 -94 0","-59 10 48 0","-78 -44 76 0","-63 -48 52 0","-88 84 -68 0","10 88 -76 0","92 -81 -88 0","66 44 42 0","-45 -98 74 0","-8 -84 -24 0","53 -75 -48 0","-6 70 50 0","90 75 -60 0","58 -60 -93 0","-71 64 73 0","54 92 -12 0","70 25 -59 0","-36 7 -42 0","-90 96 -16 0","72 -97 -35 0","-81 44 -66 0","82 14 10 0","8 -69 -33 0","6 54 -67 0","92 -8 -91 0","-99 32 -25 0","-50 5 83 0","85 53 84 0","-90 7 1 0","50 19 -44 0","21 43 27 0","-13 57 -55 0","-11 100 -52 0","42 -7 -60 0","78 -13 6 0","16 73 -92 0","92 30 -55 0","-47 36 -70 0","52 71 -95 0","99 90 70 0","53 -26 95 0","-43 59 -42 0","67 51 2 0","25 1 -19 0","10 2 -45 0","66 38 -60 0","52 -63 -13 0","-16 -19 80 0","-77 -68 11 0","70 89 25 0","80 -68 -75 0","11 -28 41 0","-1 92 -27 0","91 21 61 0","25 -84 28 0","-86 77 -97 0","-39 26 -1 0","88 49 69 0","-7 20 33 0","6 -45 32 0","-63 80 6 0","32 -78 -48 0","-9 -44 -45 0","90 64 93 0","70 -37 -57 0","29 -33 -30 0","56 -24 47 0","98 65 48 0","16 -31 -69 0"]

fileSat1'   = (foldr (\x acc -> (S.insert ((clause2pol' . words) x) acc)) S.empty) fileSat1
fileSat1''  = (foldr (\x acc -> (insertPol ((clause2pol . words) x) acc)) (S.empty,S.empty)) fileSat1
fileSat1''' = variable2List fileSat1''
