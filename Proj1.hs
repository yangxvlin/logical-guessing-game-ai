

module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List
import Debug.Trace

lastCard = (Card Spade Ace)

type Selection = [Card]
type Feedback = (Int, Int, Int, Int, Int)

-- lists of card guess
-- type GameState = [Selection]
data GameState = GameState {remaining::[Selection], guesses::[Selection], feedbacks::[Feedback]}

-- Does not work for empty lists (so maybe needs to be wrapped in some logic)
lowestRank :: Selection -> Rank
lowestRank cards = foldr1 min $ map rank cards

highestRank :: Selection -> Rank
highestRank cards = foldr1 max $ map rank cards

suits :: Selection -> [Suit]
suits cards = map (\x -> suit x) cards

ranks :: Selection -> [Rank]
ranks cards = map (\x -> rank x) cards

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

-- https://stackoverflow.com/a/27332905
filterCommonOnce :: Ord a => [a] -> [a] -> [a]
filterCommonOnce xs ys = xs \\ (xs \\ ys)

-- https://stackoverflow.com/q/6121256
-- allTheSame :: (Eq a) => [a] -> Bool
-- allTheSame xs = and $ map (== head xs) (tail xs)

-- https://stackoverflow.com/a/39342644
-- getMatch :: Ord a => [a] -> [a] -> Int
-- getMatch l r = getMatch' (sort l) (sort r)
-- getMatch' :: Ord a => [a] -> [a] -> Int
-- getMatch' [] _ = 0
-- getMatch' _ [] = 0
-- getMatch' l@(a: as) r@(b: bs) | a < b = getMatch' as r
--                               | a == b = getMatch' as bs + 1
--                               | otherwise = getMatch' l bs

-- TODO test
feedback :: Selection -> Selection -> Feedback
feedback target cards = (correctCards, lowerRanks, correctRanks, higherRanks, correctSuits)
    where
        cardsLowestRank = lowestRank cards
        cardsHighestRank = highestRank cards
        cardSuits = suits cards
        targetSuits = suits target
        cardRanks = ranks cards
        targetRanks = ranks target

        correctCards = length $ filter (\x -> elem x cards) target
        lowerRanks = length $ filter (\x -> rank x < cardsLowestRank) target
        correctRanks = length $ filterCommonOnce cardRanks targetRanks -- getMatch cardRanks targetRanks
        higherRanks = length $ filter (\x -> rank x > cardsHighestRank) target
        correctSuits = length $ filterCommonOnce cardSuits targetSuits -- getMatch cardSuits targetSuits

-- generateAllGuess :: Int -> [[Card]]
-- generateAllGuess 0 = [[]]
-- generateAllGuess cardNum = nub [sort (x:y) | x <- deck, y <- generateAllGuess (cardNum-1), not $ hasDuplicates (x:y)]
--     where
--         deck = [(Card Club R2) .. (Card Spade Ace)]

-- initialGuess :: Int -> ([Card],GameState)
-- initialGuess cardNum = (firstGuess, delete firstGuess allGuesses)
-- -- initialGuess cardNum = trace (show allGuesses) (firstGuess, delete firstGuess allGuesses)
--     where
--         allGuesses = generateAllGuess cardNum
--         firstGuess = head allGuesses

generateAllGuess :: Int -> Selection -> [Selection]
generateAllGuess 0 _ = [[]]
generateAllGuess cardNum remainingCards = [(x:y) | x <- remainingCards, y <- generateAllGuess (cardNum-1) (tail [x .. lastCard])]

initialGuess :: Int -> (Selection,GameState)
initialGuess cardNum = (firstGuess, gameState)
-- initialGuess cardNum = trace (show allGuesses) (firstGuess, delete firstGuess allGuesses)
    where
        deck = [(Card Club R2) .. lastCard]
        allGuesses = filter (\x -> cardNum == length x) $ generateAllGuess cardNum deck
        -- firstGuess = trace (show (reverse [0, (13 `div` (cardNum+1)) .. 12]))
        --     (take cardNum $ zipWith (\s r -> Card s ([R2 .. Ace] !! r)) [Club .. Spade] (reverse [0, (13 `div` (cardNum+1)) .. 12]))
        firstGuess = take cardNum $ zipWith (\s r -> Card s ([R2 .. Ace] !! r)) [Club .. Spade] (reverse [0, (13 `div` (cardNum+1)) .. 12])

        gameState = GameState allGuesses [] []

-- TODO test
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
-- nextGuess (previousGuess, gameState) guessFeedback = trace (show $ length gameState) (guess, delete guess gameState)
nextGuess (previousGuess, oldGameState) guessFeedback = (guess, newGameState)
    where
        oldGameStateRemaining = remaining oldGameState
        oldGameStateGuesses = guesses oldGameState
        oldGameStateFeedbacks = feedbacks oldGameState
        newGameState = GameState (delete previousGuess oldGameStateRemaining) (previousGuess:oldGameStateGuesses) (guessFeedback:oldGameStateFeedbacks)
        remainingPossibleAnswers = getRemainPossibleAnswers newGameState
        -- guess = trace (show updatedGameState) (head updatedGameState)
        -- tmp = trace (show updatedGameState) (chooseGuess updatedGameState)
        -- tmp = trace ((show $ sort $ chooseGuess updatedGameState) ++ "\n") (chooseGuess updatedGameState)
        tmp = chooseGuess remainingPossibleAnswers
        -- guess = head updatedGameState
        guess = snd $ head $ sort tmp

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
        -- pairedGuesses = [(g, delete g guesses) | g <- guesses]
        pairedGuesses = [(g, guesses) | g <- guesses]
        -- grouped = trace (show pairedGuesses) [(groupGuess g as, g) | (g, as) <- pairedGuesses]
        grouped = [(groupGuess g as, g) | (g, as) <- pairedGuesses]

groupGuess :: [Card] -> [[Card]] -> [[(Int,Int,Int,Int,Int)]]
groupGuess g as = group $ sort $ [feedback a g | a <- as]

calScore :: [[(Int,Int,Int,Int,Int)]] -> Int
calScore groups = (sum (map (\x -> (length x) ^ 2) groups)) `div` (sum (map length groups))



compareFeedback :: (Int,Int,Int,Int,Int) -> (Int,Int,Int,Int,Int) -> Bool
compareFeedback (correctCardsX, lowerRanksX, correctRanksX, higherRanksX, correctSuitsX)
                (correctCardsY, lowerRanksY, correctRanksY, higherRanksY, correctSuitsY)
    = correctCardsX == correctCardsY && 
      correctSuitsX == correctSuitsY &&
      correctRanksX == correctRanksY &&
      lowerRanksX == lowerRanksY &&
      higherRanksX == higherRanksY

-- updateGameState :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> GameState
-- updateGameState (previousGuess, gameState) guessFeedback = filter (\x -> compareFeedback guessFeedback (feedback x previousGuess)) gameState

-- updateGameState :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> GameState
-- updateGameState (previousGuess, gameState) guessFeedback = filter (\x -> guessFeedback == (feedback previousGuess x)) gameState
