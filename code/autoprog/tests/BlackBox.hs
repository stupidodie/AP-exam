-- Sample black-box test suite. Feel free to adapt, or start from scratch.

-- Do NOT import from your ModImpl files here. These tests should work with
-- any implementation of the AutoProg APIs. Put any white-box tests in
-- suite1/WhiteBox.hs.
import Defs
import Parser
import Resolver
import Coder

import Test.Tasty
import Test.Tasty.HUnit
import Resolver (declare)

import Debug.Trace
main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Minimal tests" [
  testGroup "Parser" [
    testCase "...Type" $
      parseStringType "a -> a" @?= Right pt0,
    testCase "...TDeclz" $
      parseStringTDeclz "type T a = a -> a" @?= Right [td0],
    testCase "type2" $
      parseStringType "F G x" @?= Right pt2,
    testCase "type3" $
      parseStringType "F G  a->B x" @?= Right (PTApp "(->)" [PTApp "F" [PTApp "G" [],PTVar "a"],PTApp "B" [PTVar "x"]]),
    testCase "type4" $
      parseStringType "F x -> (y, A)" @?= Right (PTApp "(->)" [PTApp "F" [PTVar "x"],PTApp "(,)" [PTVar "y",PTApp "A" []]]),
    testCase "type5 with comment" $
      parseStringType "z\'123 -> z{-12 -} -> zzz" @?= Right (PTApp "(->)" [PTVar "z'123",PTApp "(->)" [PTVar "z",PTVar "zzz"]]),
    testCase "type6 with comment" $
      parseStringType "z\'123 -> z{-12 -} z3-}" @?= Left "parse Error",
    testCase "new type 1" $
      parseStringTDeclz "newtype Reader r a = Rd {runRd :: r -> a}" @?= Right [TDRcd ("Reader",["r","a"]) "Rd" [("runRd",PTApp "(->)" [PTVar "r",PTVar "a"])]],
    testCase "new type 2" $
      parseStringTDeclz "newtype State s a = St {runSt :: s -> (a,s)}" @?= Right [TDRcd ("State",["s","a"]) "St" [("runSt",PTApp "(->)" [PTVar "s",PTApp "(,)" [PTVar "a",PTVar "s"]])]],
    testCase "type 1" $
      parseStringTDeclz "type F x = x -> x" @?= Right [TDSyn ("F",["x"]) (PTApp "(->)" [PTVar "x",PTVar "x"])],
    testCase "data "$ 
      parseStringTDeclz "data State s a = St {runSt,b,c :: s -> (a,s)}" @?= Right [TDRcd ("State",["s","a"]) "St" [("runSt",PTApp "(->)" [PTVar "s",PTApp "(,)" [PTVar "a",PTVar "s"]]),("b",PTApp "(->)" [PTVar "s",PTApp "(,)" [PTVar "a",PTVar "s"]]),("c",PTApp "(->)" [PTVar "s",PTApp "(,)" [PTVar "a",PTVar "s"]])]]
  ],
  testGroup "Resolver" [
    testCase "resolve" $
      resolve tce0 (\x -> return $ STVar (x++"'")) pt0 @?= Right st0,
    testCase "declare" $
      do tce <- declare [td0]
         tf <- case lookup "T" tce of Just tf -> return tf; _ -> Left "no T"
         tf [STVar "a'"]
      @?= Right st0,
    testCase "declare2" $
      do tce<- declare [td1] 
         tf1 <- case lookup "T" tce of Just tf -> return tf; _ -> Left "no T" 
         tf1 [STVar "bbb"]
      @?= Right st1,
    testCase "declare3" $
      do tce<- declare [td3] 
         tf1 <- case lookup "State" tce of Just tf -> return tf; _ -> Left "no T" 
         tf1 [STVar "bbb",STVar "ccc"]
      @?= Right st3,
    testCase "declare4" $
      do tce<- declare [td4] 
         tf1 <- case lookup "State" tce of Just tf -> return tf; _ -> Left "no T" 
         tf1 [STVar "bbb",STVar "ccc"]
      @?= Right st4,
    testCase "declare5" $
      do tce<- declare [td4] 
         tf1 <- case lookup "State" tce of Just tf -> return tf; _ -> Left "no T" 
         tf1 [STProd (STVar "bbb")  (STVar "bbb"),STVar "ccc"]
      @?= Right st5

  ],
  testGroup "Coder" [
    testCase "pick" $
      do n <- pick [0,3]
         if n > 0 then return n
         else do m <- pick [4,0]
                 if m > 0 then return m else pick []
      @?= tr0,
    testCase "solutions" $
      solutions tr0 10 Nothing @?= [3,4],
    testCase "solutions 2" $
      solutions tr0 3 (Just 5) @?= [3,5],
    testCase "produce" $
      do e <- dfs (produce [] st0)
         return $ case e of
                    Lam x (Var x') | x' == x -> e0
                    _ -> e 
      @?= [e0],
      testCase "produce2" $
        do e<-dfs (produce [] (STRcd "C" [("f",STArrow (STVar "bbb") (STVar "bbb")),("x",STVar "bbb")]))
           return $ case e of
                      RCons "C" [("x",Var x),("f",Lam y (Var z))]| z==x->RCons "C" [("x",Var "X"),("f",Lam "X" (Var "X"))]
                      _ -> e
        @?= [RCons "C" [("x",Var "X"),("f",Lam "X" (Var "X"))]]
    ]]
 where pt0 = PTApp "(->)" [PTVar "a", PTVar "a"]
       td0 = TDSyn ("T", ["a"]) pt0
       td3= TDRcd ("State",["s","a"]) "St"  pt3
       pt3= [("runSt",PTApp "(->)" [PTVar "s",PTApp "(,)" [PTVar "a",PTVar "s"]])]
       st3= STRcd "St" [("runSt",STArrow (STVar "bbb") (STProd (STVar "ccc") (STVar "bbb")))]
       st0 = STArrow (STVar "a'") (STVar "a'")
       tr0 = Choice [Choice [Found 4, Choice []], Found 3]
       td4=TDRcd ("State",["s","a"]) "St" pt4
       pt4=[("b",PTApp "(->)" [PTVar "s",PTApp "(->)" [PTVar "a",PTVar "s"]])]
       st4= STRcd "St" [("b",STArrow (STVar "bbb") (STArrow (STVar "ccc") (STVar "bbb")))]
       dfs (Found a) = [a]
       dfs (Choice ts) = concatMap dfs ts
       e0 = Lam "X" (Var "X")
       td1=TDRcd ("T", ["a"]) "C" [("x", PTVar "a"), ("f", PTApp "(->)" [PTVar "a", PTVar "a"])]
       st1=STRcd "C" [("x",STVar "bbb"),("f",STArrow (STVar "bbb") (STVar "bbb"))]
       pt2=PTApp "F" [PTApp "G" [],PTVar "x"]
       st5=STRcd "St" [("b",STArrow (STProd (STVar "bbb") (STVar "bbb")) (STArrow (STVar "ccc") (STProd (STVar "bbb") (STVar "bbb"))))]
