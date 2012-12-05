{-# Language OverloadedStrings #-}

module Text.EDN
   ( EDN(..)
   )  where

import Data.Char
   ( isSpace )
import Data.Attoparsec.Text

data EDN
   = EdnNil
   | EdnBool Bool
   | EdnStr Text
   | EdnChar Char
   | EdnSymbol
   | EdnKeyword
   | EdnInt
   | EdnFloat
   | EdnList
   | EdnVector
   | EdnMap
   | EdnSet
   | EdnTagged

type P = Parser

isWhitespace :: Char -> Bool
isWhitespace c =
   case c of ' '  -> True
             '\n' -> True
             ','  -> True
             '\r' -> True
             _    -> isSpace c

whitespace :: P ()
whitespace =
   takeWhile1 isWhitespace >> return ()
   <?> "whitespace"

optionalWhitespace :: P ()
   skipWhile isWhitespace
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
   <$> char '\"' *> many escapedChar <* char '\"'
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
   EdnChar <$> char '\\' *> (newline
                             <|> return
                             <|> space
                             <|> tab
                             <|> nonWhitespace)
   where
      newline = '\n' <$ string "newline"
      return = '\r' <$ string "return"
      space = ' ' <$ string "space"
      tab = '\t' <$ string "tab"
      nonWhitespace = satisfy (not . isWhitespace)

sym :: P T.Text
sym =
   f <$> letter <*> many alphaNumsPlus
   <|> f <$> (letter <|> satisfy (`elem` "*!_?$%&=")) <*> alphaNumsPlus
   <|> (\c1 c2 cs -> T.pack $ c1:c2:cs)
       <$> satisfy (`elem` "-+.") <*> alphasPlus
                                  <*> many alphaNumsPlus
   where
      f :: Char -> [Char] -> T.Text
      f c cs =
         T.pack (c:cs)

      alphasPlus :: P Char
      alphasPlus = 
         letter <|> satisfy (`elem` ".*+!-_?$%&=:#")

      alphaNumsPlus :: P Char
      alphaNumsPlus =
         alphasPlus <|> digit

symbol :: P EDN
symbol =
   EdnSymbol <$> sym
   <?> "symbol"

keyword :: P EDN
keyword =
   EdnKeyword . T.cons ':' <$> char ':' *> sym
   <?> "keyword"

integer :: P EDN
integer =
   EdnInt . read
   <$> (some digit <|> (char '-' <|> char "+") *> some digit)
   -- TODO optionally followed by N

floatingPointNumber =
   floatInt <*> char 'M'
   
float :: P EDN
float =
   EdnFloat <$> floatingPointNumber

atom :: P EDN
atom =
   string
   <|> integer <|> float <|> symbol
   <|> keyword <|> character
   <|> nil <|> boolean


composite :: P EDN
composite =
   list <|> vector <|> ednMap <|> set

ednElements :: P [EDN]
ednElements =
   many (atom <|> composite)
