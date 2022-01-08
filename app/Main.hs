{-# language ImportQualifiedPost #-}
{-# language TupleSections       #-}

import Control.Arrow
import Control.Monad
import Control.Parallel.Strategies
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord
import System.IO
import System.IO.Unsafe

main :: IO ()
main = if True then interactive else batch

-- | The Wordle word list, extracted from the js on the site.
-- There are actually two word list, which I call the answer list
-- and the guess list. The answer must come from the answer list,
-- but a guess may be from either list. In practice guessing only
-- from the answer list works fine.
allwords :: [String]
allwords  = unsafePerformIO $ lines <$> readFile "data/answerlist"

-- | A map from a character to it's indices in a word.
-- This is the internal representation of words in the solver.
-- We don't bother with using IntSet instead of [Int] even though we do set
-- operations on it because the size is at most 3 and usually 1.
type IxMap    = Map Char [Int]
-- | The result of comparing a guess to an answer.
type Feedback = Map Char
  ( [Int] -- The list of indices where the char from the guess is in the right place.
  , [Int] -- The list of indices where the char from the guess is in the wrong place.
  , [Int] -- The list of indices where the char from the guess is in not in the answer.
  )

-- | Take a word to the internal representation.
toIxMap :: String -> IxMap
toIxMap = M.fromListWith (flip (++)) . flip zip (map pure [0..])

-- | Recover a word from the internal representation.
fromIxMap :: IxMap -> String
fromIxMap = map snd . sort . concatMap (\(c, ns) -> map (,c) ns) . M.toList

-- | The word list in the internal representation.
allIxMaps :: [IxMap]
allIxMaps = map toIxMap allwords

-- | Given a list of words, brute force the guess that minimizes the number
-- of valid words after getting feedback on the guess.
--
-- Replace `allIxMaps` in this function with `words` to play on "Hard Mode".
-- There may be a small number of words on Hard Mode for which the solver
-- fails to find an answer.
bestGuess :: [IxMap] -> IxMap
bestGuess []     = error "bestGuess: empty list"
bestGuess [word] = word
bestGuess words  = fst $ minimumBy (comparing snd) $ zip allIxMaps weights where
  weights  = map (sortBy (comparing Down) . map length) filtered
  filtered = map (\guess -> map (\answer -> applyFeedback (makeFeedback answer guess) words) words) allIxMaps

-- | Get the feedback for a guess against a specific answer.
--
-- For letters that are not in the correct position, Wordle scans the answer
-- from left to right to try to find an unmatched letter to match it to.
-- This means that a letter in the guess can be marked out even if it
-- appears in the answer because the multiplicity of the letter in the
-- guess is greater than the multiplicity of the letter in the answer.
makeFeedback :: IxMap -> IxMap -> Feedback
makeFeedback answer guess = M.mapWithKey f guess where
  f c gs = (rights, wrongs, outs) where
    as             = M.findWithDefault [] c answer
    rights         = intersect gs as
    (gsDas, asDgs) = (gs \\ as, as \\ gs)
    (wrongs, outs) = splitAt (min (length gsDas) (length asDgs)) gsDas

-- | Filter out words from the list of guesses which are incompatible with
-- the feedback we've gotten.
applyFeedback :: Feedback -> [IxMap] -> [IxMap]
applyFeedback = filter . flip compatible

-- | Check if a word is compatible with a piece of feedback.
-- - The right placed letters must be where they are expected
-- - The wrong placed letters must not be where they are not expected.
-- - There must be enough of the wrong placed letters.
-- - None of the letters which are out of the word must appear, after taking
--   into account the ones that are matched with wrong placed letters.
-- - There may be extra of a letter not matched with right and wrong placed
--   letters only that letter is not marked out at any index.
compatible :: IxMap -> Feedback -> Bool
compatible word = all f . M.toList where
  f (c, (rights, wrongs, outs)) = rightTest && wrongTest && outTest where
    ws = M.findWithDefault [] c word
    rightTest = null (rights \\ ws)
    wrongTest = null (intersect wrongs ws) && length (ws \\ rights) >= length wrongs
    outTest   = null outs || length ws == length rights + length wrongs

-- Interactive

-- | Run the game interactively, asking the user to provide feedback against
-- an unknown word.
interactive :: IO ()
interactive = go (toIxMap "raise") allIxMaps where
  go guess words = do
    words' <- flip applyFeedback words <$> promptFeedback guess
    putStrLn $ "remaining words: " ++ show (map fromIxMap words')
    go (bestGuess words') words'

-- | Prompt the user for feedback on a guess.
promptFeedback :: IxMap -> IO Feedback
promptFeedback guess = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "guess: " ++ fromIxMap guess
  putStr "enter feedback (r: right, w: wrong, o: out): "
  -- If the feedback string is short, we assume out by default.
  M.unionsWith (<>) . map parse . zip3 [0..] (fromIxMap guess) . (++ "ooooo") <$> getLine
  where
  parse (n, g, c) | c == 'r' || c == 'R' = M.singleton g ([n], [] , [] )
                  | c == 'w' || c == 'W' = M.singleton g ([] , [n], [] )
                  | otherwise            = M.singleton g ([] , [] , [n])

-- Batch

-- | Run the game against every word in the word list.
batch :: IO ()
batch = do
  hSetBuffering stdout LineBuffering
  mapM_ print $ parMap rdeepseq (second playKnown) (zip allwords allIxMaps)

-- | Run the game against a specific known answer.
playKnown :: IxMap -> [String]
playKnown answer = reverse . map fromIxMap $ go 6 allIxMaps (toIxMap "raise") []  where
  go n words guess guesses
    | guess == answer = guess : guesses                           -- Found the answer!
    | n == 1          = reverse words ++ guesses                  -- Did not find the answer in time. Dump all remaining words as possible guesses.
    | otherwise       = go (n-1) words' guess' (guess : guesses)  -- Make a new guess and decrease the round counter.
    where words' = applyFeedback (makeFeedback answer guess) words
          guess' = bestGuess words'
