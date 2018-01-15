{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module F2 ( VarF2
          , PolF2
          , unbox ) where

import Haskell4Maths ( Vect
                     , Lex
                     , F2
                     , var)
import Test.QuickCheck ( Arbitrary
                       , Gen
                       , arbitrary
                       , vectorOf
                       , choose
                       , quickCheck)


-- | The data type PolF2 is the field of polynomial with coefficients in the
-- finite field F2.
type PolF2 = Vect F2 (Lex String)

instance Arbitrary PolF2 where
  arbitrary = polGen

-- | The data type VarF2 is the set of polynomial variables of PolF2.
newtype VarF2 = Box (Vect F2 (Lex String))
  deriving (Eq, Ord)

unbox :: VarF2 -> PolF2
unbox (Box x) = x

instance Show VarF2 where
  show = show . unbox

instance Arbitrary VarF2 where
  arbitrary = varGen

-- | varGen is a variable generator.
varGen :: Gen VarF2
varGen = do
  n <- choose ((1::Int),100)
  return (Box (var ('x':(show n))))

-- | varGen is a variable and exponent generator.
varExpGen :: Gen (PolF2,Int)
varExpGen = do
  Box x <- varGen
  i <- choose ((1::Int),5)
  return $ (x,i)

-- | varGen is a variable and exponent generator.
monGen :: Gen PolF2
monGen = do
  n <- choose ((1::Int),5)
  xs <- vectorOf n varExpGen
  return $ product [ x ^ i | (x,i) <- xs]

-- | varGen is a variable and exponent generator.
polGen :: Gen PolF2
polGen = do
  n <- choose ((1::Int),5)
  xs <- vectorOf n monGen
  return $ sum xs