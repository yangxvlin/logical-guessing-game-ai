--  File     : Proj1.hs
--  Author   : XuLin Yang 904904 <xuliny@student.unimelb.edu.au>
--  Origin   : Sat Aug 24 14:58:04 2019
--  Purpose  : An implementation of 2-player logical guessing game solution.
--
-- |This code is for providing the implementation for the two-player logical 
--  guessing game. It is defined in three functions: 
--      `feedback` for the respondent side 
--      `initialGuess`, `nextGuess` for the guesser side.
--
--  The program is for solving the game that is the respondent have `N` 
--  selected secret cards from a deck of 52 cards without jokers for the 
--  guesser to guess. The guesser first make the guess by calling 
--  `initialGuess` and then the respondent response `feedback` for the guessed
--  selection of cards and the secret selection of cards. After that the 
--  guesser repeat the process by using `nextGuess` with the respondent's 
--  `feedback` until the guesser get the correct selection.
--  
--  The program assume the respondent has a selection of 2-4 cards. (i.e.: N 
--  range from 2 to 4 inclusively).

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List
import Debug.Trace

-- |The list of selected cards from a deck.

type Selection = [Card]

-- |The respondent's feedback, made up by (correctCards, lowerRanks, 
--  correctRanks, higherRanks, correctSuits).
--
--  See `feedback` function to see how each element is defined.

type Feedback = (Int, Int, Int, Int, Int)

-- |The representation of informaiton to be passed from one call of either of
--  the guess function to the other call. In order to cascading data between 
--  funcitons.

data GameState = GameState
    -- |The attribute for the list of not guessed selections.
    { remaining :: [Selection]
    -- |The attribute for the list of gussed selections.
    , guesses   :: [Selection]
    -- |The attribute for the list of feedbacks for each guess.
    , feedbacks :: [Feedback] }

-- ******************************* constants **********************************
-- |The constant for the first card in the Card Enum.
firstCard :: Card
firstCard = minBound :: Card

-- |The constant for the last card in the Card Enum.
lastCard :: Card
lastCard = maxBound :: Card

-- ***************************** helper function ******************************

-- |A helper function. Calculate the lowesr rank among a list of cards.
--
--  Assume non emopty list input.
lowestRank :: Selection -> Rank
lowestRank [] = error "Selection should not be empty!"
lowestRank cards = foldr1 min $ map rank cards

-- |A helper function. Calculate the highest rank among a list of cards.
--
--  Assume non emopty list input.
highestRank :: Selection -> Rank
highestRank [] = error "Selection should not be empty!"
highestRank cards = foldr1 max $ map rank cards

-- |A helper function. Get the list of suit from the selection of cards.
suits :: Selection -> [Suit]
suits cards = map (\x -> suit x) cards

-- |A helper function. Get the list of rank from the selection of cards.
ranks :: Selection -> [Rank]
ranks cards = map (\x -> rank x) cards

-- |A helper function. Get the common elements between two lists. Each match is
--  counted once.
--
--      E.g. 
--          intersectOnce [1, 1, 2] [1, 2, 3] = [1, 2]
--          intersectOnce [1, 2, 3] [1, 1, 2] = [1, 2] 
--
--  Modify from: https://stackoverflow.com/a/27332905
intersectOnce :: Ord a => [a] -> [a] -> [a]
intersectOnce xs ys = xs \\ (xs \\ ys)

-- |A helper function. It takes a number of cards to be chosen from the 
--  remaining possible cards. It generates all possible selections of `N` cards
--  from a deck with 52 non-joker cards.
--
--  Note: 
--      As the order of selections is not the matter, so the successor selected
--      card will have a larger enum index. Otherwise, it is a duplicate 
--      selection. In the code, drop the first possible card in the domain when
--      choosing the next card ensuring the generated selections will not have 
--      duplication.
generateAllSelections :: Int -> Selection -> [Selection]
generateAllSelections 0 _ = [[]]
generateAllSelections cardNum remainingCards = 
    [(x:y) | x <- remainingCards, 
             y <- generateAllSelections (cardNum-1) (tail [x .. lastCard])]

getRemainPossibleAnswers :: GameState -> [Selection]
getRemainPossibleAnswers gameState = [x | x <- remainingSelections, checkSelectionPossible (x, guessedSelections, guessedFeedbacks)]
    where
        remainingSelections = remaining gameState
        guessedSelections = guesses gameState
        guessedFeedbacks = feedbacks gameState

checkSelectionPossible :: (Selection, [Selection], [Feedback]) -> Bool
checkSelectionPossible (possibleSelection, guessed, feedbacks) = and (map (\(x, y) -> (feedback possibleSelection x) == y) (zip guessed feedbacks))



chooseGuess :: [[Card]] -> [(Int, [Card])]
chooseGuess guesses = [(calScore x, g) | (x, g) <- grouped]
    where 
        pairedGuesses = [(g, guesses) | g <- guesses]
        grouped = [(groupGuess g as, g) | (g, as) <- pairedGuesses]

groupGuess :: [Card] -> [[Card]] -> [[(Int,Int,Int,Int,Int)]]
groupGuess g as = group $ sort $ [feedback a g | a <- as]

calScore :: [[(Int,Int,Int,Int,Int)]] -> Int
calScore groups = (sum (map (\x -> (length x) ^ 2) groups)) `div` (sum (map length groups))

-- ****************************** major function ******************************

-- |A major function. It takes a target and a guess (in the order as described 
--  in Cards.hs Line 33), each represented as a `Selection`, and returns the 5
--  feedback numbers, as explained below, as a tuple.
feedback :: Selection -> Selection -> Feedback
feedback target guess = 
        (correctCards, lowerRanks, correctRanks, higherRanks, correctSuits)
    where
        guessesLowestRank = lowestRank guess
        guessesHighestRank = highestRank guess
        guessesSuits = suits guess
        targetSuits = suits target
        guessedRanks = ranks guess
        targetRanks = ranks target

        -- |The number of cards in the target are also in the guess.
        correctCards = length $ filter (\x -> elem x guess) target

        -- |The number of cards in the target having the same rank as a card in
        --  the guess.
        --  
        --  Note: X for arbitrary suit
        --      target = [QX, QX], guesses [QX]     => correctRanks = [QX]
        --      target = [QX],     guesses [QX, QX] => correctRanks = [QX]
        correctRanks = length $ intersectOnce guessedRanks targetRanks 
        
        -- |The number of cards in the target having the same Suit as a card in
        --  the guess.
        --  
        --  Note:
        --      The procedure of matched card is symmetric as the above one.
        correctSuits = length $ intersectOnce guessesSuits targetSuits
        
        -- |The number of cards in the target having the rank lower than the 
        --  lowest rank in the guess.
        lowerRanks = length $ filter (<guessesLowestRank) targetRanks
        
        -- |The number of cards in the target having the rank higher than the 
        --  highest rank in the guess.
        higherRanks = length $ filter (>guessesHighestRank) targetRanks


initialGuess :: Int -> (Selection,GameState)
initialGuess cardNum = (firstGuess, gameState)
    where
        deck = [(minBound :: Card) .. (maxBound :: Card)]
        allGuesses = filter (\x -> cardNum == length x) $ generateAllSelections cardNum deck
        
        firstGuess = take cardNum $ zipWith (\s r -> Card s ([R2 .. Ace] !! r)) [Club .. Spade] (reverse [0, (13 `div` (cardNum+1)) .. 12])

        gameState = GameState allGuesses [] []

-- TODO test
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (previousGuess, oldGameState) guessFeedback = (guess, newGameState)
    where
        oldGameStateRemaining = remaining oldGameState
        oldGameStateGuesses = guesses oldGameState
        oldGameStateFeedbacks = feedbacks oldGameState
        newGameState = GameState (delete previousGuess oldGameStateRemaining) (previousGuess:oldGameStateGuesses) (guessFeedback:oldGameStateFeedbacks)
        remainingPossibleAnswers = getRemainPossibleAnswers newGameState
        tmp = chooseGuess remainingPossibleAnswers
        guess = snd $ head $ sort tmp
