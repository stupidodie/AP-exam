-- Copyright (c) 2021 GuanRan Tai
-- 
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module ParserImpl where

import Defs
import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative((<|>))

lexeme :: ReadP a -> ReadP a
lexeme p = do a <- p; whitespace; return a

whitespace :: ReadP ()
whitespace =
  skipMany $
    do satisfy (`elem` " \n\t"); return ()
      <|> comment

comment :: ReadP ()
comment =do string "{-";  manyTill get (string "-}") ;return ()

symbol :: String -> ReadP ()
symbol s = lexeme $ do string s; return ()

reserved :: [String]
reserved = ["type", "newtype", "data"]

pCName :: ReadP String
pCName = lexeme $ do
  first <- satisfy isAsciiUpper
  rest <- munch (\c -> isAsciiUpper c || isAsciiLower c || isDigit c || c == '\'' || c == '_')
  return (first:rest)

pVName ::  ReadP String
pVName = lexeme $ do
  first <- satisfy isAsciiLower
  rest <- munch (\c -> isAsciiUpper c || isAsciiLower c || isDigit c || c == '\'' || c == '_')
  let vname = first : rest
  if vname `notElem` reserved
    then return vname
    else pfail

pFDeclz:: ReadP [(String, PType)]
pFDeclz=do fDecl <-pFDecl  `sepBy` symbol "," ;return (concat fDecl) <++ return mempty
pTVarz:: ReadP [String]
pTVarz= many pVName
pTDHead :: ReadP TDHead
pTDHead=do cname<-pCName; tvNameList<-pTVarz; return (cname,tvNameList)
pTDecl :: ReadP TDecl
pTDecl=
  choice [  do symbol "type"; tdHead<-pTDHead; symbol "="; TDSyn tdHead <$> pType,
            do symbol "newtype"; tdHead'<-pTDHead;symbol "="; rcname<-pCName; symbol "{"; field<-pVName;symbol "::";type'<-pType; symbol "}";return (TDRcd  tdHead' rcname [(field,type')]),
            do symbol "data"; tdHead<-pTDHead;symbol "="; rcname<-pCName; symbol "{"; fDeclz<-pFDeclz; symbol "}" ;return (TDRcd  tdHead rcname fDeclz)]
pTDeclz:: ReadP [TDecl]
pTDeclz=do tDel <-pTDecl  `sepBy` symbol ";"; return tDel <++ return mempty

pFDecl :: ReadP [(String, PType)]
pFDecl=do field <-pVName  `sepBy1` symbol ","; symbol "::" ; type'<-pType; return $ map (\x-> (x,type')) field
pType:: ReadP PType
pType=pTypeOther `chainr1` (do symbol "->"; return ( \x y->  PTApp "(->)" [x,y] ))
pTypeOther ::ReadP PType
pTypeOther =  pTypeVar <++ pTypeCon<++ pTypeScal <++ pType1
pTypeScal:: ReadP PType
pTypeScal=do symbol "("; type1<-pType;symbol ",";type2<-pType;symbol ")";return (PTApp "(,)" [type1,type2])
pType1::ReadP PType
pType1 = do symbol "("; type' <- pType; symbol ")"; return type'
pTypeVar:: ReadP PType
pTypeVar= PTVar <$> pVName
pTypeCon:: ReadP PType
pTypeCon=do cname<-pCName;PTApp cname <$> pTypez
pTypez:: ReadP [PType]
pTypez = many (pTypeScal <++ pType1 <++ pTypeVar <++ (do cname<-pCName; return (PTApp cname [])) ) 

add :: Foldable t => a -> t a -> [a]
add a = foldr (:) [a]
parseStringType :: String -> EM PType
parseStringType s =
  case readP_to_S (between whitespace eof pType) s of
        [] -> Left "parse Error"
        [(idb, _)] ->  Right idb
        _ -> error "ambiguous grammar"

parseStringTDeclz :: String -> EM [TDecl]
parseStringTDeclz s= 
    case readP_to_S (between whitespace eof pTDeclz) s of
        [] -> Left "parse Error"
        [(idb, _)] ->  Right idb
        _ ->  error "ambiguous grammar"

main=print (parseStringType "z123 -> z{-12 -} -> zzz")