module CoderImpl where

import Defs
import Control.Monad (ap, liftM)
import Data.Bifunctor ( Bifunctor(second) )

-- no need to touch these
instance Functor Tree where fmap = liftM
instance Applicative Tree where pure = return; (<*>) = ap

instance Monad Tree where
    return = Found
    (Found x)  >>= f= f x
    (Choice l) >>= f= Choice (fmap (>>= f) l)

pick :: [a] -> Tree a
pick l=  Choice (map Found l)

bfs :: ([Tree a],[a])->Int->[a]
bfs ([],result) _= result
bfs (_,result) 0 = result
bfs (t:ts,result) n= case t of
    Found x -> bfs (ts,x:result) (n-1)
    Choice l -> bfs (ts++l,result) (n-1)

solutions :: Tree a -> Int -> Maybe a -> [a]
solutions t x j=  reverse $ case j of
    (Just p)-> p : bfs ([t],[]) x
    Nothing -> bfs ([t],[]) x

produce :: [(String,SType)] -> SType -> Tree Exp
produce g t= do (e,_)<-differentNameProduce g t 0;return e
-- THE ORIGINAL CODE
-- case t of
--     (STProd st1 st2)->do  e1<-produce g st1 ;e2<-produce g st2; return (Pair e1 e2)
--     (STArrow st1 st2)->do t<-produce (("X",st1):g) st2;return (Lam "X" t)
--     (STRcd rcname fpList)->return $ RCons rcname (map (\(fp,st)->case produce g st of Found e->(fp,e); _->error "Error Type") fpList)
--     (STVar v)->case concatMap (\(fp,st')->[(fp,st') | containStype st' t]) g  of
--         []->pick []
--         l-> do f<- extract g t (snd $ head l) ;return $ replace (fst $ head l) (f (convertSTypeToExp t))
differentNameProduce::[(String,SType)] -> SType->Int -> Tree (Exp,[(String,SType)])
differentNameProduce g t n=
     case t of
        (STProd st1 st2)->
            do  (e1,g1)<-differentNameProduce g st1 (n+1)
                (e2,g2)<-differentNameProduce g1 st2 (n+1) 
                return (Pair e1 e2,g2)
        (STArrow st1 st2)->
            do 
                (t,g1)<-differentNameProduce ((givenName n,st1):g) st2 (n+1)
                return (Lam (givenName n) t,g1)
        (STRcd rcname fpList)-> 
            let resultList=reverse (updateEnvList g fpList n) 
                in  return (RCons rcname (map (\(x,y,_)->(x,y)) resultList),
                case head resultList of (_,_,z)->z)
        (STVar _)->
            case concatMap (\(fp,st')->[(fp,st') | containStype st' t]) g  of
                []->pick []
                l-> do 
                    f<- extract g t (snd $ head l) 
                    return (replace (fst $ head l) (f (convertSTypeToExp t)),g)

updateEnvList::[(String,SType)]->[(FName,SType)] ->Int->[(FName, Exp,[(String,SType)])]
updateEnvList _ [] _=[]
updateEnvList g ((f,st):l) n=
    case differentNameProduce g st (n+1) of
        Found (e,g1)->(f,e,g1):updateEnvList g1 l (n+1)
        Choice [Found (e,g1)]->(f,e,g1):updateEnvList g1 l (n+1)
        err->error $ "Error Type "++ f++" "++show st ++" "++show err
givenName:: Int ->String
givenName n="X"++show n
replace :: String->Exp->Exp
replace name exp= case exp of
    (Var _)-> Var name
    (Fst exp')-> Fst (replace name exp')
    (Snd exp')-> Snd (replace name exp')
    (App e1 e2)-> App (replace name e1) (replace name e2)
    _->error "Unexpected type"
containStype::SType->SType->Bool
containStype st1 st2=  st1==st2 ||(case st1 of
                                               (STProd st1' st2')->containStype st1' st2  || containStype st2' st2
                                               (STArrow st1' st2')->containStype st1' st2  || containStype st2' st2
                                               (STRcd _ fpList)->case sum $ map (\(_,st)->if containStype st st2 then 1 else 0 )  fpList of 0->False;_->True
                                               (STVar _)->False)
-- recommended, but not mandated, helper function:
extract :: [(String,SType)] -> SType -> SType -> Tree (Exp -> Exp)
-- construct a search tree for extracting 
-- a t-typed result from a t'-typed expression
extract _ t t' = if t'==t
    then pick [\_->convertSTypeToExp t']
    else case t' of
            (STVar _) ->  pick []
            (STProd st1 st2)->
                do
                    pick [\_-> Fst $ convertSTypeToExp st1]
                    pick [\_-> Snd $ convertSTypeToExp st2]
            (STArrow st1 _)->pick [\_->App  (convertSTypeToExp t') (convertSTypeToExp st1)]
            (STRcd _ fpList)->  head $ map (\(_,st)->pick[\_->App (convertSTypeToExp st) (convertSTypeToExp t')]) fpList

convertSTypeToExp::SType -> Exp
convertSTypeToExp (STVar vname)= Var vname
convertSTypeToExp (STProd st1 st2)=Pair  (convertSTypeToExp st1)  (convertSTypeToExp st2)
convertSTypeToExp (STArrow  _ st2)= Lam "X" (convertSTypeToExp st2)
convertSTypeToExp (STRcd rcname fpList)= RCons rcname $ map (Data.Bifunctor.second convertSTypeToExp) fpList