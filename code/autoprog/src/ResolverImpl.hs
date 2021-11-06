{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ResolverImpl where

import Defs
import qualified Data.Set
import Data.Map.Strict (update)
import GHC.IO.Encoding.Failure (codingFailureModeSuffix)
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
functionCorr cname vnameList pt tce ts =
        if length ts==length vnameList then convertPTypeToSType pt tce ts else Left $ "bad args for tycon "++ cname

bindEnv::TDecl->TCEnv->TCEnv
bindEnv (TDSyn (cname,vnameList) pt) tce=
    if judgeDuplicatedName vnameList then (cname,functionCorr cname vnameList pt tce):tce else error "Not distinct variables!"
bindEnv (TDRcd (cname,vnameList) rcname fpList) tce=
    if judgeDuplicatedName vnameList
        then (rcname,\st->
            if length st==length vnameList 
                then return (STRcd rcname  (convertfpList  fpList st tce)  )
                else  error $ "bad args for tycon "++ cname ):tce
        else error "Not distinct variables!"

convertfpList::[(FName, PType)]->[SType]-> TCEnv ->[(FName, SType)]
convertfpList fpList st tce=
    case fpList of
        ((fname,pt):xs)->(fname,case convertPTypeToSType pt tce st of Right x->x;_->error "Wrong"):convertfpList xs st tce
        []->[]
judgeDuplicatedName::[TVName]->Bool
judgeDuplicatedName ts = length ts == length ( Data.Set.fromList ts)

convertPTypeToSType::PType ->TCEnv-> [SType]->EM SType 
convertPTypeToSType pt tce [st]= case pt of
    (PTVar vname)-> Right st
    (PTApp cname ptList)->
        case lookup cname tce of
            -- Just f->case f $ map (`convertPTypeToSType` tce) ptList of (Right stype)-> stype; _->error "convet error"
            Just f->f $ replicate (length ptList) st
            Nothing->error "not find"

