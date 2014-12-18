module Testing where

import Logic
import Test.QuickCheck

-- Generates a random tile
instance Arbitrary Tile where
  arbitrary = oneof [return Black,
                    return White,
                    return Arrow,
                    return Empty]

-- Generates a weighted tile for arbitrary boards
tile :: Gen(Tile)
tile = frequency [(10, return Empty),
                  (2, oneof [return Black,
                             return White,
                             return Arrow])]

-- Generates a randomized board
instance Arbitrary Board where
  arbitrary =
    do rows <- sequence [ sequence [ tile | j <- [1..10] ] | i <- [1..10] ]
       return (Board rows)

board :: Gen(Board)
board = do rows <- sequence [ sequence [ tile | j <- [1..10] ] | i <- [1..10] ]
           return (Board rows)

-- For random generation
data APos = APos { p :: Pos}
    deriving (Show, Eq)

-- Generates random position
instance Arbitrary APos where
  arbitrary = do
    x <- choose (0,9)
    y <- choose (0,9)
    return (APos (x,y))

prop_shoot :: Board -> APos -> APos -> Property
prop_shoot b pos1 pos2 = b /= newBoard ==> getPos newBoard p2 == Arrow
  where p1 = (p pos1)
        p2 = (p pos2)
        newBoard = shoot b p1 p2

prop_replacePos :: APos -> Tile -> Board -> Property
prop_replacePos ap t b = t /= getPos b pos ==> (getPos (replacePos pos t b) pos == t)
  where pos = (p ap)