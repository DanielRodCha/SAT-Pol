{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_HADDOCK hide #-}

module LogicParser (parseFProp) where


import Haskell4Maths (var
                     , zerov)
import F2
import Transformations ( phi)
import Subsumption

import Data.List (foldl')
import Data.Char
import qualified Data.Set as S

import Logic (FProp (..), VarProp (..))

import Text.ParserCombinators.Parsec
    ((<|>), char, choice, eof, letter, parse, spaces, string, try)

import Text.ParserCombinators.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec.Pos (SourceName)
import Text.ParserCombinators.Parsec.Prim (GenParser)

-- | The 'parseFProp' function accepts the name of a source, and a string to be
-- parsed, and attempts to parse the string as a logical expression of the
-- following forms, where @&#966;@ and @&#968;@ are metalinguistic variables
-- standing for any valid expression.
--
-- * Variables: @\"P\"@, @\"Q\"@, @\"a\"@, @\"b\"@ etc.; basically anything in
--   the character class @[a-zA-Z]@
--
-- * Negation: @\"~&#966;\"@
--
-- * Conjunction: @\"(&#966; & &#968;)\"@
--
-- * Disjunction: @\"(&#966; | &#968;)\"@
--
-- * Conditional: @\"(&#966; -> &#968;)\"@
--
-- * Biconditional: @\"(&#966; \<-> &#968;)\"@
--
-- Top-level expressions where the primary connective is a binary one do not
-- need to be parenthesised. For example, @\"p -> (q & r)\"@ is a valid
-- expression, although @\"(p -> (q & r))\"@ is also fine.
parseFProp :: String -> FProp
parseFProp xs = unbox' $ parseFProp' "" xs
 where unbox' (Right x) = x

parseFProp' :: SourceName -> String -> Either ParseError FProp
parseFProp' = parse statement

statement :: GenParser Char st FProp
statement = do spaces
               x <- try binary <|> expr
               spaces
               eof
               return x

expr :: GenParser Char st FProp
expr = choice [binaryP, negation, variable]

variable :: GenParser Char st FProp
variable = do c <- letter
              return $ Atom [c]

negation :: GenParser Char st FProp
negation = do char '~'
              spaces
              x <- expr
              return $ Neg x

binaryP :: GenParser Char st FProp
binaryP = do char '('
             spaces
             x <- binary
             spaces
             char ')'
             return x

binary :: GenParser Char st FProp
binary = do x1 <- expr
            spaces
            s  <- choice $ map string ["&", "|", "->", "<->"]
            spaces
            x2 <- expr
            return $ connective s x1 x2
  where
    connective c = case c of
      "&"   -> Conj
      "|"   -> Disj
      "->"  -> Impl
      "<->" -> Equi
      _     -> error "Impossible case"
      