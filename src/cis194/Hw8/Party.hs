{-#OPTIONS_GHC -Wall
               -Werror
               -fno-warn-orphans #-}

module Party where

import Data.Tree
import Employee
import Test.HUnit

--- Exercise 1

-- this implementation of mappend will include duplicates if the same Employee
-- is contained in both GuestList
instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ ef) (GL es gf) = GL (e:es) (ef + gf)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 =
  case compare gl1 gl2 of
    GT -> gl1
    _  -> gl2


--- Exercise 2

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f acc (Node v ts) = foldr (flip $ treeFold f) (f v acc) ts
-- treeFold f acc (Node l ts) = foldr (\t acc' -> treeFold f acc' t) (f l acc) ts


--- Exercise 3

-- First argument is the 'boss' of the current subtree and the second argument
-- is the list of results from each subtree
-- First result is best possible result with 'boss' and second is best possible
-- result without 'boss'
nextLevel :: Employee -> [(GuestList,GuestList)] -> (GuestList,GuestList)
nextLevel b = let withBoss = glCons b mempty
                  noBoss = mempty
              in foldr builder (withBoss,noBoss)
  where
    builder :: (GuestList,GuestList) -> (GuestList,GuestList) -> (GuestList,GuestList)
    builder (withSubBoss,noSubBoss) (withBoss,noBoss) =
      let withBoss' = mappend noSubBoss withBoss
          noBoss'   = mappend (max withSubBoss noSubBoss) noBoss
      in (withBoss',noBoss')


--- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . maxFun'
  where
    maxFun' :: Tree Employee -> (GuestList,GuestList)
    maxFun' (Node e []) = nextLevel e []
    maxFun' (Node e ts) = nextLevel e (map maxFun' ts)

--- Exercise 5

main :: IO ()
main = do fileString <- readFile "company.txt"
          let bestGuestList = maxFun . read $ fileString
          putStrLn . prettyGL $ bestGuestList

prettyGL :: GuestList -> String
prettyGL (GL es f) = let firstLine = "Total fun: " ++ show f
                         subsequentLines = map empName es
                     in unlines (firstLine:subsequentLines)


--- Tests ---

tests :: IO Counts
tests = runTestTT $ TestList
  [ glConsTests
  , monoidLawTests
  , moreFunTests
  , treeFoldTests
  , nextLevelTests
  , maxFunTests
  ]
    where

      testGuestList :: GuestList
      testGuestList = GL [Emp "Ayesha" 2, Emp "Razi" 100] 105

      glConsTests = TestLabel "glCons" $
        glCons (Emp "Matt" 3) (GL [Emp "Ayesha" 2, Emp "Razi" 100] 102) ~?=
          GL [ Emp {empName = "Matt", empFun = 3}
             , Emp {empName = "Ayesha", empFun = 2}
             , Emp {empName = "Razi", empFun = 100}
             ] 105

      monoidLawTests = TestLabel "Monoid GuestList Law tests" $ TestList
        [ mempty `mappend` testGuestList ~?= testGuestList
        , testGuestList `mappend` mempty ~?= testGuestList
        , let gl1 = testGuestList
              gl2 = GL [Emp "Matt" 3] 3
              gl3 = GL [Emp "Drake" 4, Emp "Grayson" 4] 8
          in (gl1 `mappend` gl2) `mappend` gl3 ~?= gl1 `mappend` (gl2 `mappend` gl3)
        ]

      moreFunTests = TestLabel "moreFun" $ TestList
        [ moreFun (GL [] 3) (GL [] 4) ~?= GL [] 4
        , moreFun (GL [] 4) (GL [] 3) ~?= GL [] 4
        , moreFun (GL [] 4) (GL [] 4) ~?= GL [] 4
        ]

      treeFoldTests = TestLabel "treeFold" $ TestList
        [ treeFold (\(Emp _ n) acc -> n+acc) 0 testCompany  ~?= 46
        , treeFold (\(Emp _ n) acc -> n*acc) 1 testCompany2 ~?= 137700
        ]

      nextLevelTests = TestLabel "nextLevel" $ TestList
        [ nextLevel boss []                                 ~?= (GL [boss]       111, GL []      0)
        , nextLevel boss [(GL [happy] 222, GL [sad]   7)]   ~?= (GL [sad,boss]   118, GL [happy] 222)
        , nextLevel boss [(GL [sad]   7  , GL [happy] 222)] ~?= (GL [happy,boss] 333, GL [happy] 222)
        , nextLevel boss [(GL [happy] 222, GL [mid]   150)] ~?= (GL [mid,boss]   261, GL [happy] 222)
        , nextLevel boss [(GL [happy] 222, GL [mid]   150)
                         ,(GL [mid]   150, GL [sad]   7  )] ~?= (GL [mid,sad,boss] 268, GL [happy,mid] 372)
        ]
          where
            boss = Emp "boss" 111
            happy = Emp "happy" 222
            mid = Emp "medium" 150
            sad = Emp "sad" 7

      maxFunTests = TestLabel "maxFun" $
        maxFun testCompany ~?= GL [Emp "John" 1, Emp "Sue" 5, Emp "Fred" 3, Emp "Sarah" 17] 26
