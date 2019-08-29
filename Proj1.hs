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
type Feedback = (Int,Int,Int,Int,Int)

-- |The representation of informaiton to be passed from one call of either of
--  the guess function to the other call. In order to cascading data between 
--  funcitons. It records the possible selections based on the past guesses.
data GameState = GameState {domain::[Selection], ansNum::Int, guessesNum::Int}

-- ******************************* constants **********************************
-- |The constant for the first card in the Card Enum.
firstCard :: Card
firstCard = minBound :: Card

-- |The constant for the last card in the Card Enum.
lastCard :: Card
lastCard = maxBound :: Card

-- |The constant for the number of ranks in the Card Enum.
rankNum :: Int
rankNum = 13

-- ***************************** helper function ******************************
-- |A helper function. Get the list of suit from the selection of cards.
suits :: Selection -> [Suit]
suits cards = map suit cards

-- |A helper function. Get the list of rank from the selection of cards.
ranks :: Selection -> [Rank]
ranks cards = map rank cards

-- |A helper function. Get the common elements between two lists. Each match is
--  counted once. Modify from: https://stackoverflow.com/a/27332905
--  E.g. 
--      intersectOnce [1, 1, 2] [1, 2, 3] = [1, 2]
--      intersectOnce [1, 2, 3] [1, 1, 2] = [1, 2] 
intersectOnce :: Ord a => [a] -> [a] -> [a]
intersectOnce xs ys = xs \\ (xs \\ ys)

-- |A helper function. It takes a number of cards to be chosen from the 
--  remaining possible cards and remaining possible cards. It generates all 
--  possible selections of `N` cards from a deck with 52 non-joker cards.
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

-- score: expected number of remaining possible answers for the guess
-- |A helper function. It takes a list of selections (i.e.: possible guesses)
--  and returns a list of tuples consists of a score and the guess
calGuessesScore :: [Selection] -> [(Int, Selection)]
calGuessesScore guesses = [(calScore x, g) | (x, g) <- grouped]
    where 
        -- divide guesses by a guess and the rest
        dividedGuesses = [(g, guesses) | g <- guesses]
        grouped = [(groupGuessAnsFeedback g as, g) | (g, as) <- dividedGuesses]

-- |A helper function. It associates a guess with a list of answers and then 
--  group the associated list by the feedback of each list element. 
--  In the code, `sort` before the `group` because haskell only group 
--  consecutive common elements.
groupGuessAnsFeedback :: Selection -> [Selection] -> [[Feedback]]
groupGuessAnsFeedback g as = group $ sort $ [feedback a g | a <- as]

-- |A helper function. It takes grouped Feedback and return the calculated 
--  by: 
--  (the sum of the squares of the group sizes) / (the sum of the group sizes)
calScore :: [[Feedback]] -> Int
calScore groups = (sum (map (\x -> (length x) ^ 2) groups)) `div` 
                    (sum (map length groups))

-- ****************************** major function ******************************
-- |A major function. It takes a target and a guess (in the order as described 
--  in Cards.hs Line 33), each represented as a `Selection`, and returns the 5
--  feedback numbers: (correctCards, lowerRanks, correctRanks, higherRanks, 
--  correctSuits), as a tuple.
feedback :: Selection -> Selection -> Feedback
feedback target guess = 
        (correctCards, lowerRanks, correctRanks, higherRanks, correctSuits)
    where
        guessesSuits = suits guess
        targetSuits = suits target
        guessesRanks = ranks guess
        targetRanks = ranks target
        guessesLowestRank = minimum guessesRanks
        guessesHighestRank = maximum guessesRanks

        -- |The number of cards in the target are also in the guess.
        correctCards = length $ filter (\x -> elem x guess) target

        -- |The number of cards in the target having the same rank as a card in
        --  the guess.
        --  
        --  Note: X for arbitrary suit
        --      target = [QX, QX], guesses [QX]     => correctRanks = [QX]
        --      target = [QX],     guesses [QX, QX] => correctRanks = [QX]
        correctRanks = length $ intersectOnce guessesRanks targetRanks 

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

-- |A major function. It takes the number of cards in the answer. It outputs a
--  selection of initial guess and the game state, as a tuple.
--
--  It assumes that the cardNum ranges from 2-4 inclusively.
initialGuess :: Int -> (Selection,GameState)
initialGuess ansNum = (firstGuess, GameState gameState ansNum 0) -- <- 0 for no guesses yet
    where
        -- initially game state is the whole domain
        deck = [firstCard .. lastCard]
        gameState = generateAllSelections ansNum deck
        
        -- In general, for an n card answer, first guess is chosen according to
        -- the guideline: ranks that are about 13/(n + 1) ranks apart are 
        -- chosen and associated with different suits.
        rankApart = rankNum `div` (ansNum + 1)
        -- ans4Cards = zipWith (\s i -> Card s ([Ace, King .. R2] !! i)) 
        --                 [Club .. Spade] ([0, rankApart .. 12])
        ans4Cards = reverse $ zipWith (\s i -> Card s ([R2 .. Ace] !! i)) 
                        [Club .. Spade] ([0, rankApart .. 12])
        firstGuess = take ansNum $ ans4Cards

-- |A major function. It takes as input a pair of the previous guess and game 
--  state, and the feedback to this guess, and returns a pair of the next guess
--  and new game state.
nextGuess :: (Selection,GameState) -> Feedback -> (Selection,GameState)
nextGuess (preGuess, oldGameState) preGuessFeedback = (guess, newGameState)
    where
        oldGameStateDoamin = domain oldGameState
        _ansNum = ansNum oldGameState
        _guessesNum = guessesNum oldGameState
        newGameStateDomain = filter (\x -> preGuessFeedback == feedback x preGuess) $ 
                        delete preGuess oldGameStateDoamin
        --  score: expected number of remaining possible answers for the guess
        --  Sort the list of (score, guess) increasingly. Thus the guess with 
        --  min score is chosen.
        guess = if _ansNum==4 && (_guessesNum==0 || (_guessesNum==1 && (length newGameStateDomain)>1200)) then
                    head newGameStateDomain
                else
                    snd $ head $ sort $ calGuessesScore newGameStateDomain

        newGameState = GameState newGameStateDomain _ansNum (_guessesNum+1)
