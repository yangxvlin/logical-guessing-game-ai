module Main where

import Proj1
import Card

-- |The list of selected cards from a deck.
type Selection = [Card]

-- |The constant for the first card in the Card Enum.
firstCard = minBound :: Card

-- |The constant for the last card in the Card Enum.
lastCard = maxBound :: Card

generateAllSelections :: Int -> Selection -> [Selection]
generateAllSelections 0 _ = [[]]
generateAllSelections cardNum remainingCards = 
    [(x:y) | x <- remainingCards, 
             y <- generateAllSelections (cardNum-1) (tail [x .. lastCard])]

main :: IO()
main = do
    -- print((feedback [(Card Club R3), (Card Heart R4)] [(Card Heart R4), (Card Club R3)]) == (2,0,2,0,2))
    -- print((feedback [(Card Club R3), (Card Heart R4)] [(Card Club R3), (Card Heart R3)]) == (1,0,1,1,2))
    -- print((feedback (map read ["3D", "3H"]) (map read ["3S", "3C"])) == (0,0,2,0,0))
    -- print((feedback (map read ["3C", "4H"]) (map read ["2H", "3H"])) == (0,0,1,1,1))
    -- print((feedback (map read ["AC", "2C"]) (map read ["3C", "4H"])) == (0,1,0,1,1))

    let allGuesses = generateAllSelections 4 [firstCard .. lastCard]
    print(elem (map read ["AC", "AD", "AH", "AS"]) allGuesses)
    print(elem (map read ["2C", "2D", "2H", "2S"]) allGuesses)
