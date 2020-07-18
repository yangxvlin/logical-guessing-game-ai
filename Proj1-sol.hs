--  File     : Proj1.hs
--  Author   : Peter Schachte
--  Purpose  : A guessing game in Haskell
--
--  This module implements the guessing part of the Proj1 game. In
--  this game one player chooses two cards (without replacement) from
--  a standard deck and the other person tries to guess the cards.
--  After each guess, the player is given feedback to indicate how
--  close they are to the right two cards, and they can use this
--  information to inform the next guess. The object of the game is to
--  guess both cards with the fewest guesses. This program implements
--  a generalisation of this game, allowing it work for more than 2
--  cards (up to about 5).
--
--  The strategy we use is to generate a list of all possible guesses.
--  After receiving the feedback from each guess, we filter the list
--  of possible guesses down to just the ones that would have elicited
--  the feedback we actually received. Thus the list of possibilities
--  contains all and only the possibilities not ruled out by the
--  feedback we've received. Since each guess removes at least one
--  possibility, this is guaranteed to guess the right pair
--  eventually.
--
--  To try to find the right answer faster, we carefully select our
--  guess to minimise the number of remaining possible guesses after
--  we receive feedback. This is described it detail below.


module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List
import qualified Data.Map as Map


-- | The maximum number of possible guesses to carefully choose the
-- best next guess for.  If there are more possibilities than this,
-- just pick the first one and hope the feedback eliminates enough of
-- the rest to get below this limit.  Since careful selection is
-- quadratic in cost (actually n^2 log n because we use a Map to count
-- guesses with each feedback), more than this many starts to take too
-- long.  This limit was determined by trial and error.
possibilityLimit :: Int
possibilityLimit = 2000


----------------------------------------------------------------
--                            Local Types
----------------------------------------------------------------

-- | The type of both answers and guesses.
type Selection = [Card]


----------------------------------------------------------------
--                      Our External Interface
----------------------------------------------------------------

-- | Our exported GameState type, to remember what we need to know
--   between guesses.  This is simply a list of the remaining
--   possible selections.

data GameState = GameState [Selection] deriving Show


-- | The feedback we receive for our guesses: a quintuple of the
--   number of completely correct cards, the number of of ranks below
--   the lowest rank in the target, the number of correct ranks, the
--   number of ranks above the highest rank in the target, and the
--   number of correct suits.

type Feedback = (Int, Int, Int, Int, Int)


-- | Returns our first guess.  This function could just do what
--    nextGuess does: determine a best guess to make based on the
--    likely outcomes.  But instead we use the much more efficient
--    approach suggested in the project spec: we guess cards spaced
--    about evenly over the space of possible ranks, and guess suits
--    in order.


initialGuess :: Int -> (Selection,GameState)
initialGuess size =
    let interval = max (13 / (fromIntegral size + 1)) 1.0
    in  ([Card 
          (toEnum $ i `mod` 4)
          (toEnum $ (round (fromIntegral i * interval)) `mod` 13) |
          i <- [1..size]],
         GameState (allPossibilities size))


-- | Returns our next guess, based on our previous guess, the
--   remembered list of still-possible answers, and the feedback we
--   received for our previous guess.  Our algorithm is to first
--   eliminate the possible answers that are ruled out by the feedback
--   from our previous guess, and then choose the one of those that
--   has the lowest expected number of remaining possible answers.  To
--   keep performance manageable, if there are more than 663
--   possibilities remaining (that's half of the possibilities at the
--   start of a 2-card game), then we don't try to pick the best
--   guess, we just pick the first possibility.  This speeds up
--   guessing significantly (in runtime) for games with more than 2
--   cards, and in my testing, it doesn't seem to take more guesses
--   for up to a 4-card game.

nextGuess :: (Selection,GameState) -> Feedback -> (Selection,GameState)
nextGuess (guess, GameState poss) answer =
  let poss'  = remainingPossibilities poss guess answer
      guess' = if (length poss') > possibilityLimit then head poss'
               else bestGuess poss'
  in (guess', GameState poss')


----------------------------------------------------------------
--                  Managing the list of possible cards
----------------------------------------------------------------

-- | The list of all possible distinct sets of cards or cardinality n,
-- as ordered lists.

allPossibilities :: Int -> [Selection]
allPossibilities n 
  | n < 0 = error "allPossibilities called with negative length argument"
  | n == 0 = [[]]
  | n > 0  = [(card:rest) 
             | rest <- allPossibilities $ n - 1,
               card <- case rest of
                 []        -> [minBound .. maxBound]
                 (bound:_) -> if bound == minBound then []
                              else [minBound .. pred bound]]


-- | Returns the possible cards remaining after considering the
--   feedback from a single guess.  This filters out any cards from
--   the previous list of possibilities that are excluded because
--   they would have different feedback for the guess than the
--   feedback we actually received.

remainingPossibilities :: [Selection] -> Selection -> Feedback -> [Selection]
remainingPossibilities poss guess fback =
  filter ((==fback) . flip feedback guess) poss


-- | Compute the correct feedback for a guess.  First argument is the
--   target, second is the guess.

feedback :: Selection -> Selection -> Feedback
feedback target guess = (right, belowRank, rightRank, aboveRank, rightSuit)
  where guess'      = nub guess         -- shouldn't be necessary
        right       = length $ multisetSortedIntersection
                               (sort target)
                               (sort guess)
        targetRanks = sort $ map rank target
        guessRanks  = sort $ map rank guess'
        belowRank   = length $ filter (< (head guessRanks)) targetRanks
        rightRank   = length $ multisetSortedIntersection targetRanks guessRanks
        aboveRank   = length $ filter (> (last guessRanks)) targetRanks
        rightSuit   = length $ multisetSortedIntersection
                               (sort $ map suit target)
                               (sort $ map suit guess')


-- | Returns elements that appear in both input lists.  Both input
--   lists must be sorted.  Repeated elements are counted each time they
--   appear in *both* lists.

multisetSortedIntersection :: Ord a => [a] -> [a] -> [a]
multisetSortedIntersection [] _ = []
multisetSortedIntersection _ [] = []
multisetSortedIntersection alist@(a:as) blist@(b:bs)
  | a == b    = a:multisetSortedIntersection as bs
  | a < b     = multisetSortedIntersection as blist
  | otherwise = multisetSortedIntersection alist bs


----------------------------------------------------------------
--                   Choosing the best next guess
----------------------------------------------------------------

-- | Choose the best guess, given the list of remaining possible
--   cards. The approach is as described earlier: we pick a possible
--   card that gives us the lowest "expected outcome", where the
--   expected outcome is the average over all remaining possible
--   targets of the number of targets that will remain if we make that
--   guess for that target. We also reduce the number of possible
--   cards we consider by only guessing one card of each "shape" based
--   on what we've guessed before. This is handled by the
--   differentGuesses function.
--
--   This approach is not ideal, because it restricts itself to only
--   ever guessing known possible targets. When some cards are known,
--   that means we have to guess them each time, which limits the
--   number of places we can put guesses. But if we allow ourselves to
--   guess impossible targets, it's possible that a guess won't
--   eliminate any possible cards, making it possible to get into an
--   infinite loop. So we stick to the policy of only guessing known
--   possible answers.

bestGuess :: [Selection] -> Selection
bestGuess poss = minimize (expectedOutcome poss) poss


-- | expectedOutcome poss guess
--   Returns the average number of possible cards that would remain after
--   making the specified guess over all the possible targets on poss

expectedOutcome :: [Selection] -> Selection -> Double
expectedOutcome poss guess =
    let counts = Map.elems $ foldl countValue Map.empty
                 $ map (flip feedback guess) poss
    in  fromIntegral (sum $ map (^2) counts) / fromIntegral (sum counts)


-- | Increment the count associated with the given key in the map,
-- taking the count to be zero if there's no mapping for the key.
countValue :: Ord t => Map.Map t Int -> t -> Map.Map t Int
countValue m k = Map.alter incrMaybeCount k m

incrMaybeCount :: Maybe Int -> Maybe Int
incrMaybeCount Nothing = Just 1
incrMaybeCount (Just i) = Just $ i+1


-- | minimize f lst
--   Returns element e of lst with the minimum value of f e
minimize :: Ord b => (a -> b) -> [a] -> a
minimize _ [] = error "minimising over an empty list"
minimize f (e:es) = minimize' f (f e) e es

minimize' f fm m [] = m
minimize' f fm m (e:es)
  | fe < fm   = minimize' f fe e es
  | otherwise = minimize' f fm m es
  where fe = f e
