{-# Language OverloadedStrings #-}

{- Copyright 2012 Robin Bate Boerop

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

   Contributor(s):
      Robin Bate Boerop <me@robinbb.com>
-}

module Text.EDN
   ( EDN(..)
   , ednElements
   , prettyEDN
   )  where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
   ( isSpace )
import Data.List
   ( intercalate )
import qualified Data.Set as Set
   ( Set, fromList, member, union )
import qualified Data.Text as T
   ( Text, pack, unpack, cons, append )

data EDN
   = EdnNil
   | EdnBool Bool
   | EdnStr T.Text
   | EdnChar Char
   | EdnSymbol T.Text
   | EdnKeyword T.Text
   | EdnInt Integer
   | EdnFloat Double
   | EdnList [EDN]
   | EdnVector [EDN]
   | EdnMap [(EDN, EDN)]
   | EdnSet [EDN]
   | EdnTagged T.Text EDN
   deriving (Eq, Show)

type P = Parser

isWhitespace :: Char -> Bool
isWhitespace c =
   case c of ' '  -> True
             '\n' -> True
             ','  -> True
             '\r' -> True
             _    -> isSpace c

comment :: P ()
comment =
   char ';' *> skipWhile (/= '\n') <* char '\n'
   <?> "comment"

optionalWhitespace :: P ()
optionalWhitespace =
   skipWhile isWhitespace
   <* many (comment <* skipWhile isWhitespace)
   <?> "optionalWhitespace"

nil :: P EDN
nil =
   EdnNil <$ string "nil"
   <?> "nil"

boolean :: P EDN
boolean =
   EdnBool <$> (True <$ string "true" <|> False <$ string "false")

str :: P EDN
str =
   EdnStr . T.pack
   <$> (char '\"' *> many escapedChar <* char '\"')
   <?> "str"
   where
      escapedChar :: P Char
      escapedChar =
         satisfy (\c -> c /= '\"' && c /= '\\')
         <|> char '\\' *> trn
         <?> "escapedChar"

      trn :: P Char
      trn = ('\n' <$ char 'n' <|> '\r' <$ char 'r' <|> '\t' <$ char 't')

character :: P EDN
character =
   EdnChar <$> (char '\\' *> (nl <|> ret <|> spc <|> tab <|> nWS))
   where
      nl = '\n' <$ string "newline"
      ret = '\r' <$ string "return"
      spc = ' ' <$ string "space"
      tab = '\t' <$ string "tab"
      nWS = satisfy (not . isWhitespace)

textCons :: Char -> [Char] -> T.Text
textCons c cs =
   T.pack (c:cs)

symbolSet1, symbolSet2, symbolSet3 :: Set.Set Char
symbolSet1 = Set.fromList "-+."
symbolSet2 = Set.fromList "*!_?$%&=></"
symbolSet3 = Set.fromList ":#"
symbolSet4 = symbolSet1 `Set.union` symbolSet2 `Set.union` symbolSet3

alphasPlus = 
   letter <|> satisfy (`Set.member` symbolSet4)

alphaNumsPlus =
   many (alphasPlus <|> digit)

sym :: P T.Text
sym =
   ( textCons <$> (letter <|> satisfy (`Set.member` symbolSet2))
     <|> (\c1 c2 cs -> T.pack $ c1:c2:cs) <$> satisfy (`Set.member` symbolSet1)
                                         <*> alphasPlus )
   <*> alphaNumsPlus

symbol :: P EDN
symbol =
   EdnSymbol <$> sym
   <?> "symbol"

keyword :: P EDN
keyword =
   EdnKeyword . textCons ':' <$> (char ':' *> alphaNumsPlus)
   <?> "keyword"

integer :: P EDN
integer =
   EdnInt . read
   <$> (some digit <|> (char '-' <|> char '+') *> some digit)
   -- TODO optionally followed by N

floatingPointNumber :: P T.Text
floatingPointNumber =
   T.append <$> int <*> ( string "M"
                          -- <|> (T.append <$> frac <*> ex)
                          <|> frac
                          <|> ex )
   where
      one2nine :: P Char
      one2nine =
         satisfy (\c -> c >= '1' && c <= '9')

      raw =
         string "0"
         <|> textCons <$> one2nine <*> many digit

      int =
         raw
         <|> T.cons <$> char '+' <*> raw
         <|> T.cons <$> char '-' <*> raw

      frac =
         textCons <$> char '.' <*> some digit

      ex =
         T.append <$> e <*> (T.pack <$> some digit)

      e =
         string "e+" <|> string "E+"
         <|> string "e-" <|> string "e+"
         <|> string "E" <|> string "e"

float :: P EDN
float =
   EdnFloat . read . T.unpack <$> floatingPointNumber

atom :: P EDN
atom =
   str
   <|> float <|> integer
   <|> symbol <|> keyword
   <|> character <|> nil <|> boolean

list :: P EDN
list =
   EdnList <$> (char '(' *> ednElements <* char ')')

vector :: P EDN
vector =
   EdnVector <$> (char '[' *> ednElements <* char ']')

ednMap :: P EDN
ednMap =
   EdnMap <$> (char '{'
               *> optionalWhitespace
               *> many ((,) <$> ednElement <* optionalWhitespace
                            <*> ednElement <* optionalWhitespace)
               <* char '}')

set :: P EDN
set =
   EdnSet <$> (string "#{" *> ednElements <* char '}')

composite :: P EDN
composite =
   list <|> vector <|> ednMap <|> set

ednElement :: P EDN
ednElement =
   atom <|> composite

ednElements :: P [EDN]
ednElements =
   optionalWhitespace *> many (ednElement <* optionalWhitespace)

prettyEDN :: EDN -> String
prettyEDN EdnNil =
   "nil"
prettyEDN (EdnBool b) =
   if b then "True" else "False"
prettyEDN (EdnStr t) =
   show t
prettyEDN (EdnChar c) =
   show c
prettyEDN (EdnSymbol t) =
   T.unpack t
prettyEDN (EdnKeyword k) =
   T.unpack k
prettyEDN (EdnInt i) =
   show i
prettyEDN (EdnFloat d) =
   show d
prettyEDN (EdnList l) =
   '(' : intercalate " " (map prettyEDN l) ++ ")"
prettyEDN (EdnVector v) =
   '[' : intercalate " " (map prettyEDN v) ++ "]"
prettyEDN (EdnMap m) =
   "{ "
   ++ intercalate "\n " (map (\(a,b) -> prettyEDN a ++ " " ++ prettyEDN b) m)
   ++ "}"
prettyEDN (EdnSet s) =
   "#{" ++ intercalate " " (map prettyEDN s) ++ "}"
prettyEDN (EdnTagged t edn) =
   '#' : show t ++ prettyEDN edn
