{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

{-ex 1-}
firstSeed :: Seed
firstSeed = mkSeed 1
{-
genRandList :: Int -> Seed-> [Integer]
genRandList 0 x = []
genRandList n x = frand : genRandList (n-1) newSeed
            where (frand, newSeed) = rand x

fiveRands :: [Integer]
fiveRands = genRandList 5 firstSeed

{-ex 2-}
randLetter :: Seed -> (Char, Seed)
randLetter x = (c, newSeed)
           where (n, newSeed) = rand x
                 c = toLetter n

randString :: Int -> Seed -> String
randString 0 x = []
randString n x = letter : randString (n-1) newSeed
           where (letter, newSeed) = randLetter x

randString3 :: String
randString3 = randString 3 firstSeed
-}
{-ex 3-}
data Gen a = Gen   {
                        next :: Seed  -> (a, Seed)
                   }

mkGen :: (Integer -> a) -> Gen a
mkGen f = Gen {next = g}
          where g s = (f nNum, nSeed)
                      where (nNum, nSeed) = rand s

randN :: Gen Integer
randN = mkGen id

randLetter :: Gen Char
randLetter = mkGen toLetter

{- Previous exercises using our gen type -}
genRandList :: Int -> Gen [Integer]
genRandList 0 = mkGen $ const []
genRandList n = Gen {next = f}
                where f s = (theList, newSeed)
                            where (fNum, nSeed)      = next randN s
                                  (remArr, newSeed)  = (next $ genRandList (n-1)) nSeed
                                  theList            = fNum : remArr
fiveRands :: [Integer]
fiveRands = fst ((next $ genRandList 5) firstSeed) 

mkGenList :: Int -> (Integer -> a) -> Gen [a]
mkGenList n f = Gen {next = g}
                where g s             = (map f nList, nState)
                                        where (nList, nState) = (next $ genRandList n) s 

genRandString :: Int -> Gen String
genRandString n = mkGenList n toLetter

randString3 :: String
randString3 = fst ((next $ genRandString 3) firstSeed)

{- exercise 3 questions -}
randEven :: Gen Integer
randEven = mkGen (*2)

randOdd :: Gen Integer
randOdd = mkGen $ \x -> 2 *x +1

randTen :: Gen Integer
randTen = mkGen (*10)

{- exercise 4 -}
generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair gen1 gen2 = Gen {next = f}
                            where f s = ((c1, c2), seed)
                                        where (c1, tseed)  = next gen1 $ s
                                              (c2, seed)   = next gen2 $ tseed
randPair :: Gen (Char, Integer)
randPair = generalPair randLetter randN
