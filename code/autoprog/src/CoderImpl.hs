module CoderImpl where

import Defs
import Control.Monad (ap, liftM)
import Debug.Trace


-- no need to touch these
instance Functor Tree where fmap = liftM
instance Applicative Tree where pure = return; (<*>) = ap

instance Monad Tree where
    return = Found
    (Found x)>>= f= f x
    (Choice l) >>= f= Choice (fmap (>>= f) l)

pick :: [a] -> Tree a
pick l=  Choice (map Found l)

bfs :: ([Tree a],[a])->Int->[a]
bfs ([],result) _= result
bfs (x,result) 0 = result
bfs (t:ts,result) n= case t of
    Found x -> bfs (ts,x:result) (n-1)
    Choice l -> bfs (ts++l,result) (n-1)

solutions :: Tree a -> Int -> Maybe a -> [a]
solutions t x j=  reverse $ case j of
    (Just p)-> p : bfs ([t],[]) x
    Nothing -> bfs ([t],[]) x

produce :: [(String,SType)] -> SType -> Tree Exp
produce = undefined

-- recommended, but not mandated, helper function:
extract :: [(String,SType)] -> SType -> SType -> Tree (Exp -> Exp)
-- construct a search tree for extracting 
-- a t-typed result from a t'-typed expression
extract g t t' = 
-- main = 
        
