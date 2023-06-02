module Basenames (bases, baseName) where

import Data.List (minimumBy)
import Data.Function (on)
import Data.Numbers.Primes (isPrime)

-- Get a number base's name.
baseName :: Integral i => i -> String
baseName   0 = "nullary"
baseName   1 = "unary"
baseName   2 = "binary"
baseName   3 = "trinary"
baseName   4 = "quaternary"
baseName   5 = "quinary"
baseName   6 = "seximal"
baseName   7 = "septimal"
baseName   8 = "octal"
baseName   9 = "nonary"
baseName  10 = "decimal"
baseName  11 = "elevenary"
baseName  12 = "dozenal"
baseName  13 = "baker's dozenal"
baseName  16 = "hex"
baseName  17 = "suboptimal"
baseName  20 = "vigesimal"
baseName  36 = "niftimal"
baseName 100 = "centesimal"
baseName n | n < 0 = "nega" ++ baseName (-n)
           | isPrime n = "un" ++ baseName (n - 1)
           | otherwise = uncurry joinWords $ cuteFactors n prefixPostfix

-- Base multiplier prefix
factorPrefix :: Integral i => i -> String
factorPrefix   2 = "bi"
factorPrefix   3 = "tri"
factorPrefix   4 = "tetra"
factorPrefix   5 = "penta"
factorPrefix   6 = "hexa"
factorPrefix   7 = "hepta"
factorPrefix   8 = "octo"
factorPrefix   9 = "enna"
factorPrefix  10 = "deca"
factorPrefix  11 = "leva"
factorPrefix  12 = "doza"
factorPrefix  13 = "baker"
factorPrefix  16 = "tesser"
factorPrefix  17 = "mal"
factorPrefix  20 = "icosi"
factorPrefix  36 = "feta"
factorPrefix 100 = "hecto"
factorPrefix n | isPrime n = "hen" ++ factorPrefix (n - 1) ++ "sna"
               | otherwise = uncurry joinWords $ cuteFactors n prefixPrefix
               where prefixPrefix = factorPrefix >< factorPrefix

-- Base multiplicand suffix, just the same as it's base name with minor exceptions.
factorPostfix :: Integral i => i -> String
factorPostfix 13 = "ker's dozenal"
factorPostfix 10 = "gesimal"
factorPostfix n = baseName n

-- Factorise a number into two factors, from two closest together to two furthest apart.
-- Smallest factor first.  This is the preferred way of splitting factors.
-- Such factors radiate from the square root of the number.
siblingFactors :: forall i. Integral i => i -> [(i, i)]
siblingFactors k = (findFactor . floor . sqrt . fromIntegral) k []
  where findFactor :: i -> [(i, i)] -> [(i, i)]
        findFactor n ns | n == 1 = ns
                        | k `mod` n == 0 = (n, k `div` n) : findFactor (n - 1) ns
                        | otherwise = findFactor (n - 1) ns

closestFactors :: Integral i => i -> (i, i)
closestFactors = head . siblingFactors

-- Get sibling factors which produce the shortest pair of root words.
-- This is not need for *validity*, but it is preferred.
-- Also note that this is *much* slower than just using the first factorisation.
cuteFactors :: Integral i => i -> ((i, i) -> (String, String)) -> (String, String)
cuteFactors k names = shortest . map names $ siblingFactors k
  where shortest = minimumBy (compare `on` uncurry ((+) `on` length))

-- Apply two functions onto the 1st and 2nd elements of a tuple.
(><) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
(f >< g) (x, y) = (f x, g y)
infixr 8 ><

-- Gets the prefix and suffix root words from two factors.
prefixPostfix :: Integral i => (i, i) -> (String, String)
prefixPostfix = factorPrefix >< factorPostfix

-- A vowel-conscious way of joining root words.
joinWords :: String -> String -> String
joinWords a b | (last a ==) `any` ['a', 'o'] && isVowel (head b) = init a ++ b
              | last a == 'i' && (head b ==) `any` ['i', 'u'] = a ++ tail b
              | otherwise = a ++ b
  where isVowel c = (c ==) `any` ['a', 'e', 'i', 'o', 'u', 'y']

-- An infinite list of base names, whose indices correspond to the base.
bases :: [String]
bases = map baseName [0 :: Int ..]
