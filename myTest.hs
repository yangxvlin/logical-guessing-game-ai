module Main where

import Proj1
import Card
import Data.List
import Debug.Trace

-- |The list of selected cards from a deck.
type Selection = [Card]

-- |The respondent's feedback, made up by (correctCards, lowerRanks, 
--  correctRanks, higherRanks, correctSuits).
--
--  See `feedback` function to see how each element is defined.
type Feedback = (Int,Int,Int,Int,Int)

-- |The constant for the first card in the Card Enum.
firstCard = minBound :: Card

-- |The constant for the last card in the Card Enum.
lastCard = maxBound :: Card

generateAllSelections :: Int -> Selection -> [Selection]
generateAllSelections 0 _ = [[]]
generateAllSelections cardNum remainingCards = 
    [(x:y) | x <- remainingCards, 
             y <- generateAllSelections (cardNum-1) (tail [x .. lastCard])]

-- |A helper function. It associates a guess with a list of answers and then 
--  group the associated list by the feedback of each list element. 
--  In the code, `sort` before the `group` because haskell only group 
--  consecutive common elements.
groupGuessAnsFeedback :: Selection -> [Selection] -> [[Feedback]]
groupGuessAnsFeedback g as = trace (show $ grouped) grouped
    where
        grouped = group $ sort $ [feedback a g | a <- as]

-- |A helper function. It takes grouped Feedback and return the calculated 
--  by: 
--  (the sum of the squares of the group sizes) / (the sum of the group sizes)
calScore :: [[Feedback]] -> Int
calScore groups = (sum (map (\x -> (length x) ^ 2) groups)) `div` 
                    (sum (map length groups))

main :: IO()
main = do
    -- print((feedback [(Card Club R3), (Card Heart R4)] [(Card Heart R4), (Card Club R3)]) == (2,0,2,0,2))
    -- print((feedback [(Card Club R3), (Card Heart R4)] [(Card Club R3), (Card Heart R3)]) == (1,0,1,1,2))
    -- print((feedback (map read ["3D", "3H"]) (map read ["3S", "3C"])) == (0,0,2,0,0))
    -- print((feedback (map read ["3C", "4H"]) (map read ["2H", "3H"])) == (0,0,1,1,1))
    -- print((feedback (map read ["AC", "2C"]) (map read ["3C", "4H"])) == (0,1,0,1,1))

    -- let allGuesses = generateAllSelections 4 [firstCard .. lastCard]
    -- print(elem (map read ["AC", "AD", "AH", "AS"]) allGuesses)
    -- print(elem (map read ["2C", "2D", "2H", "2S"]) allGuesses)

    -- let guess = 
    -- let ans = 

    print(calScore $ groupGuessAnsFeedback [Card Club R2] [[Card Club R2], [Card Club R3], 
                                                           [Card Diamond R3], 
                                                           [Card Club R4],
                                                           [Card Club R5]])
