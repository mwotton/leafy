{-# LANGUAGE OverloadedStrings #-}
module Treetop where

import Prelude hiding(takeWhile)
import qualified Data.ByteString.Char8 as S
import Data.Attoparsec.Char8
import Data.Either
import Control.Applicative hiding (many)

type Name = S.ByteString
newtype Modname = Modname [Name]

data Grammar = Grammar { grammarName :: Name, 
                         includes :: [Include], 
                         rules :: [Rule]}
data Include = Include {modName :: Name}
data Rule = Rule { ruleName :: Name, 
                   expr :: Expr}
data Expr = Expr {}

grammar :: Parser Grammar
grammar = do grammarName' <- string "grammar" >> grammarNameP
             option "" (string "do") >> skipSpace
             declarations <- many declarationP
             skipSpace >> string "end"
             return Grammar { grammarName = grammarName', 
                              includes    = lefts declarations, 
                              rules       = rights declarations}
  
declarationP :: Parser (Either Include Rule)
declarationP = eitherP includeP ruleP


ruleP :: Parser Rule
ruleP = do ruleName' <- string "rule" >> skipSpace >> nameP -- ignore keyword matches, do it later
           expr' <- skipSpace >> option () (string "do" >> skipSpace) >> expressionP
           skipSpace >> string "end"
           return Rule { ruleName = ruleName', 
                         expr = expr'}
       
nameP :: Parser Name
nameP = do first <- satisfy $ inClass ['a' .. 'z']
           rest <- takeWhile isAlpha_ascii
           return (first `S.cons` rest)
           
includeP :: Parser Include
includeP = string "include" >> skipSpace >> Include <$> modNameP
choiceP = undefined
sequenceP = undefined

primaryP = undefined


expressionP = choiceP <|> sequenceP <|> primaryP

modNameP = do head <- satisfy $ inClass ['A' .. 'Z']
              -- tail <- sepBy (string "::") (many isAlpha_ascii)
              undefined
              
              
grammarNameP :: Parser S.ByteString
grammarNameP = do
  -- quick upper?
  head <- satisfy $ inClass ['A' .. 'Z']
  tail <- takeWhile isAlpha_ascii
  return (head `S.cons` tail)

  
  
  
