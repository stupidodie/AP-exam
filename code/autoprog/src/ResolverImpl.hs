module ResolverImpl where

import Defs
import qualified Data.Set
import Debug.Trace
resolve :: TCEnv -> (TVName -> EM SType) -> PType -> EM SType
resolve tce tve pt=
    case pt of
        (PTVar vname)-> tve vname
        (PTApp cname typeList)->
            case lookup cname tce of
                Just f-> f (map (\x->case resolve tce tve x of (Right stype)->stype; _-> error "Wrong") typeList)
                Nothing->Left "not find"

declare :: [TDecl] -> EM TCEnv
declare tdList=return (updateEnv tdList tce0)

updateEnv ::[TDecl]->TCEnv -> TCEnv
updateEnv x tce = foldl (flip bindEnv) tce x
functionCorr::TCName->[TVName]->PType->TCEnv->[SType]->EM SType
functionCorr cname vnameList pt tce st =
        if length st==length vnameList then case convertPTypeToSType pt tce (generateStList vnameList pt st) of (Right x,st')->Right x ;_->error "Error" else Left $ "bad args for tycon "++ cname

bindEnv::TDecl->TCEnv->TCEnv
bindEnv (TDSyn (cname,vnameList) pt) tce=
    if judgeDuplicatedName vnameList then (cname,functionCorr cname vnameList pt tce):tce else error "Not distinct variables!"
bindEnv (TDRcd (cname,vnameList) rcname fpList) tce=
    if judgeDuplicatedName vnameList
        then (cname,\st->
            if length st==length vnameList
                then return (STRcd rcname  (convertfpList  fpList (generateStList' vnameList fpList st) tce)  )
                else  error $ "bad args for tycon "++ cname ):tce
        else error "Not distinct variables!"

generateStList::[TVName]->PType ->[SType]->[SType]
generateStList [] _ _=[]
generateStList vnameList pt st=replaceName vnameList (getName pt) st
generateStList'::[TVName]->[(FName, PType)] ->[SType]->[SType]
generateStList' [] _ _=[]
generateStList' vnameList fpList st=replaceName vnameList (concatMap (getName . snd) fpList) st
replaceName::[TVName]->[TVName]->[SType]->[SType]
replaceName [] _ _=[]
replaceName vnameList nameList st= let zipList=zip vnameList st in map (\x->case lookup x zipList of (Just r)->r;Nothing->error "Not find") nameList

getName::PType->[TVName]
getName (PTVar x)= [x]
getName (PTApp _ xs)= concatMap getName xs
convertfpList::[(FName, PType)]->[SType]-> TCEnv ->[(FName, SType)]
convertfpList fpList st tce=
    case fpList of
        ((fname,pt):xs)->let result=convertPTypeToSType pt tce st in (fname,case result of (Right x,st1)->x;err->error $"Wrong"++show err):convertfpList xs (snd result) tce
        []->[]
judgeDuplicatedName::[TVName]->Bool
judgeDuplicatedName ts = length ts == length ( Data.Set.fromList ts)

convertPTypeToSType::PType ->TCEnv-> [SType]->(EM SType,[SType])
-- convertPTypeToSType pt tce [st]= 
--     case pt of
--         (PTVar _)-> Right st
--         (PTApp cname ptList)->
--             case lookup cname tce of
--                 -- Just f->case f $ map (`convertPTypeToSType` tce) ptList of (Right stype)-> stype; _->error "convet error"
--                 Just f->f $ replicate (length ptList) st
--                 Nothing->error "not find"
convertPTypeToSType (PTVar _) _ st= (Right (head st),drop 1 st)
convertPTypeToSType (PTApp cname ptList) tce st=
    case lookup cname tce of
       -- Just f->case f $ map (`convertPTypeToSType` tce) ptList of (Right stype)-> stype; _->error "convet error"
        Nothing->error "not find"
        Just f->
            case cname of
                "(->)" -> 
                    case ptList of
                        [PTVar _,PTVar _]->(f (take 2 st), drop 2 st)
                        [PTVar _,pt]->let (Right x,st1)=convertPTypeToSType pt tce (drop 1 st) in (f [head st, x],st1)
                        [pt,PTVar _]->let (Right x,st1)=convertPTypeToSType pt tce  st in (f [x, head st1],drop 1 st1)
                        [pt1,pt2]->let (Right x,st1)=convertPTypeToSType pt1 tce st ; (Right y,st2)=convertPTypeToSType pt2 tce st1 in (f [x ,y],st2)
                        _->error "Wrong ptList"
                "(,)"->
                    case ptList of
                        [PTVar _,PTVar _]->(f (take 2 st), drop 2 st)
                        [PTVar _,pt]->let (Right x,st1)=convertPTypeToSType pt tce (drop 1 st) in (f [head st, x],st1)
                        [pt,PTVar _]->let (Right x,st1)=convertPTypeToSType pt tce  st in (f [x, head st1],drop 1 st1)
                        [pt1,pt2]->let (Right x,st1)=convertPTypeToSType pt1 tce st ; (Right y,st2)=convertPTypeToSType pt2 tce st1 in (f [x ,y],st2)
                        _->error "Wrong ptList"
                _->
                    case ptList of
                        [PTVar _]->(f $take 1 st,drop 1 st)
                        [pt]->let (Right x,st1)=convertPTypeToSType pt tce st in (f [x],st1)
                        _->error "Wrong ptList"
                    





