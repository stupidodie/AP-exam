module ParserImpl where

import Defs
import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative((<|>)) 

lexeme :: ReadP a -> ReadP a
lexeme p = do a <- p; whitespace; return a
-- skip over whitespace and/or comments
whitespace :: ReadP ()
whitespace =
  skipMany $
    do satisfy (`elem` " \n\t"); return ()
      <|> comment

--skip over a complete comment
comment :: ReadP ()
comment = between (string "{-") (string "-}") $ skipMany $ do get;return () 

--parse a specific symbol
symbol :: String -> ReadP ()
symbol s = lexeme $ do string s; return ()

reserved :: [String]
reserved = ["type", "newtype", "data"]

pCName :: ReadP String
pCName = lexeme $ do
  first <- satisfy isAsciiUpper
  rest <- munch (\c -> isAsciiUpper c || isAsciiLower c || isDigit c || c == '\'' || c == '_')
  return (first:rest)
  -- let cname = first : rest
  -- if cname `notElem` reserved
  --   then return cname
  --   else pfail

pVName ::  ReadP String
-- 记得前面有lexeme
pVName = lexeme $ do
  first <- satisfy isAsciiLower
  rest <- munch (\c -> isAsciiUpper c || isAsciiLower c || isDigit c || c == '\'' || c == '_')
  let vname = first : rest
  if vname `notElem` reserved
    then return vname
    else pfail
pType:: ReadP PType
pType=pTypeOther `chainr` (do symbol "->"; return (PTApp "(->)" ))
pFDeclz:: ReadP [(String, PType)]
pFDeclz=do fDecl <-pFDecl  `sepBy` (symbol ",") ;return (concat fDecl) <++ return ()
pTVarz:: ReadP [pTVar]
pTVarz= do tVar<-pName; tVarz<-pTVarz ;return (tVar:tVarz) <++ return ()
pTDHead :: ReadP TDHead 
pTDHead=do cname<-pCName; tvNameList<-pTVarz; return (cname,tvNameList)
pTDecl :: ReadP TDecl
pTDecl=
  choice [  do symbol "type"; tdHead<-pTDHead; symbol "="; type'<-pType;return (TDSyn tdHead type'),
            do symbol "newtype"; tdHead<-pTDHead;symbol "="; rcname<-pcName; resultList<-(between (char '{') (char '}') (do field<-pVName;string "::";type'<-pType;return ([(field,type')]))); return(TDRcd  tdHead rcname resultList),
            do symbol "data"; tdHead<-pTDHead;symbol "="; rcname<-pcName; resultList<-(between (char '{') (char '}') (pFDeclz));return(TDRcd  tdHead rcname resultList)]
pTDeclz:: ReadP [TDecl]
pTDeclz=do tDel <-pTDecl  `sepBy` (symbol ";"); return tDel <++ return ()



pFDecl :: ReadP [(String, PType)]
pFDecl=do field <-pVName  `sepBy1` (symbol ","); string "::" ; type'<-pType; return $ map (\x-> (x,type')) field
pTypeOther ::ReadP PType
pTypeOther =  pTypeScal <++ pType1<++ pTypeVar <++ pTypeCon
pTypeScal:: ReadP PType
pTypeScal=do symbol "("; type1<-pType;symbol ",";type2<-pType;symbol ")";return (PTAPP "(,)" [type1,type2])
pType1::ReadP PType 
pType1=do symbol "("; type<-pType; symbol ")";return type
pTypeVar:: ReadP PType
pTypeVar= do vname<-pVName; return (PTVar vname)
pTypeCon:: ReadP PType
pTypeCon=do cname<-pCName; resultList<-pTypez;return (PTApp cname resultList)
pTypez:: ReadP PType
pTypez=do {PTApp cname typeList}<-pType ;rest<- pTypez ; return (PTAPP cname (add rest typeList )) <++ return ()
add a [] = [a]
add a (x:xs) = x : add a xs
parseStringType :: String -> EM PType
parseStringType = undefined

parseStringTDeclz :: String -> EM [TDecl]
parseStringTDeclz = undefined

main=  putStrLn $ show $ readP_to_S (do comment;whitespace;pVName)  "{- dsas-} \n\t dsadsa" 