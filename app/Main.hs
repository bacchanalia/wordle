{-# language BlockArguments      #-}
{-# language ImportQualifiedPost #-}
{-# language MultiWayIf          #-}
{-# language RecordWildCards     #-}
{-# language TupleSections       #-}
{-# language TypeApplications    #-}

import Control.Monad
import Control.Parallel.Strategies
import Data.ByteString.Lazy         qualified as BS
import Data.Either
import Data.IntSet                  (IntSet)
import Data.IntSet                  qualified as IS
import Data.List
import Data.Map.Strict              (Map)
import Data.Map.Strict              qualified as M
import Data.Ord
import Data.Serialize
import Data.Vector.Serialize        ()
import Data.Vector                  qualified as V
import Data.Vector.Unboxed          qualified as U
import Data.Word
import System.Environment
import System.FilePath
import System.IO

-- Main

-- | usage: wordle [OPTIONS|CACHEOPTIONS]
--
-- OPTIONS
--   --hard               Play on hard-mode
--   --batch              Play against all known words
-- CACHEOPTIONS
--   --cache-feedback     Write data/feedbackcache
--   --cache-apps         Write data/feedbackappcache
main :: IO ()
main = do
  args <- getArgs
  let flags = ["--hard", "--batch", "--cache-feedback", "--cache-feedback-app"]
      parse flag (fs, as) = (elem flag as : fs, delete flag as)
      ([optHard, optBatch, optCacheFB, optCacheFBA], args') = foldr parse ([], args) flags
      opts      = [optHard, optBatch]
      cacheOpts = [optCacheFB, optCacheFBA]
      invalid     = not (null args') || or opts && or cacheOpts
  env <- readEnv optHard
  if| invalid      -> error "usage: wordle [OPTIONS|CACHEOPTIONS]"
    | or cacheOpts -> do
        let write name make = BS.writeFile ("data" </> name) $ encodeLazy (make env)
        when optCacheFB  $ write "feedbackcache"    makeFeedbackCache
        when optCacheFBA $ write "feedbackappcache" makeFeedbackAppCache
    | optBatch     -> batch       env
    | otherwise    -> interactive env

-- Using the word list

-- | A map from a character to it's indices in a word.
-- This is the internal uncached representation of words in the solver.
-- We don't bother with using IntSet instead of [Int] even though we do set
-- operations on it because the size is at most 3 and usually 1.
type CharIxs = Map Char [Int]

-- | Env carries the word list and values derived from it.
data Env = Env
  { allWords      :: [String]
  -- ^ The Wordle word list, extracted from the js on the site.
  -- There are actually two word list, which I call the answer list
  -- and the guess list. The answer must come from the answer list,
  -- but a guess may be from either list. In practice guessing only
  -- from the answer list works fine.
  , firstGuess    :: String
  , numWords      :: Int
  , allCharIxs    :: [CharIxs]
  -- ^ allWords converted to CharIxs
  , allWordIxs    :: [Int]
  -- ^ allWords as enumerated values
  , wordToEnum    :: String -> Int
  , enumToWord    :: Int -> String
  , charIxsToEnum :: CharIxs -> Int
  , enumToCharIxs :: Int -> CharIxs
  , hardMode      :: Bool
  }

-- | Read the word list and construct related values
readEnv :: Bool -> IO Env
readEnv hardMode = do
  allWords         <- lines <$> readFile "data/answerlist"
  let firstGuess    = "raise"
      numWords      = length allWords
      allCharIxs    = map toCharIxs allWords
      allWordIxs    = [0..length allWords-1]
      wordToEnum    = (M.!) . M.fromList $ zip allWords [0..]
      enumToWord    = (M.!) . M.fromList $ zip [0..] allWords
      charIxsToEnum = (M.!) . M.fromList $ zip allCharIxs [0..]
      enumToCharIxs = (M.!) . M.fromList $ zip [0..] allCharIxs
  return Env{..}

-- Precomputing feedback

-- | The result of comparing a guess to an answer.
type Feedback = Map Char
  ( [Int] -- The list of indices where the char from the guess is in the right place.
  , [Int] -- The list of indices where the char from the guess is in the wrong place.
  , [Int] -- The list of indices where the char from the guess is in not in the answer.
  )

-- | Compute all values of `makeFeedback` in a form easy to access and store.
--
-- The index of the Vector encodes answer and guess.
-- The index and the value together encode a Feedback
makeFeedbackCache :: Env -> U.Vector Word8
makeFeedbackCache Env{..} = U.fromList do
  answer <- allCharIxs
  guess  <- allCharIxs
  return . fromIntegral . feedbackToEnum $ makeFeedback answer guess

-- | Compute the values of `applyFeedback` for every Feedback against
-- the complete word list in a form easy to access and store.
--
-- The index encodes a Feedback
-- The value encodes the list of words compatible with that feedback
makeFeedbackAppCache :: Env -> V.Vector (U.Vector Word16)
makeFeedbackAppCache Env{..} = V.fromList do
  guess      <- allWords
  feedbackIx <- [0..numFeedbacks-1]
  let compatWords = applyFeedback (enumToFeedback guess feedbackIx) allCharIxs
  return . U.fromList $ map (fromIntegral . charIxsToEnum) compatWords

-- | Get the feedback for a guess against a specific answer.
--
-- For letters that are not in the correct position, Wordle scans the answer
-- from left to right to try to find an unmatched letter to match it to.
-- This means that a letter in the guess can be marked out even if it
-- appears in the answer because the multiplicity of the letter in the
-- guess is greater than the multiplicity of the letter in the answer.
makeFeedback :: CharIxs -> CharIxs -> Feedback
makeFeedback answer guess = M.mapWithKey f guess where
  f char guessIxs = (rights, wrongs, outs) where
    answerIxs      = M.findWithDefault [] char answer
    rights         = intersect guessIxs answerIxs
    (gsDas, asDgs) = (guessIxs \\ answerIxs, answerIxs \\ guessIxs)
    (wrongs, outs) = splitAt (min (length gsDas) (length asDgs)) gsDas

-- | Filter out words from the list of guesses which are incompatible with
-- the feedback we've gotten.
applyFeedback :: Feedback -> [CharIxs] -> [CharIxs]
applyFeedback = filter . flip compatible

-- | Check if a word is compatible with a piece of feedback.
-- - The right placed letters must be where they are expected
-- - The wrong placed letters must not be where they are not expected.
-- - There must be enough of the wrong placed letters.
-- - None of the letters which are out of the word must appear, after taking
--   into account the ones that are matched with wrong placed letters.
-- - There may be extra of a letter not matched with right and wrong placed
--   letters only that letter is not marked out at any index.
compatible :: CharIxs -> Feedback -> Bool
compatible word = all f . M.toList where
  f (char, (rights, wrongs, outs)) = rightTest && wrongTest && outTest where
    wordIxs   = M.findWithDefault [] char word
    rightTest = null (rights \\ wordIxs)
    wrongTest = null (intersect wrongs wordIxs) && length (wordIxs \\ rights) >= length wrongs
    outTest   = null outs || length wordIxs == length rights + length wrongs

-- | A Feedback consists of a word and information about whether each character
-- in the word is right, wrong, or out. Enumerate the second part only.
feedbackToEnum :: Feedback -> Int
feedbackToEnum = enum . foldMap snd . M.toList where
  enum (r, w, _) = sum $ map ((2*).(3^)) r ++ map (3^) w

-- | Combine a guess with the result of `feedbackEnum` to recover a Feedback.
enumToFeedback :: String -> Int -> Feedback
enumToFeedback guess = mkFeedback guess . map toRWO . unfold id where
  toRWO n = case n of 2 -> right; 1 -> wrong; 0 -> out; _ -> error "enumToFeedback: impossible case"

  unfold acc 0 = acc []
  unfold acc n = let (a, b) = n `divMod` 3 in unfold (acc . (b:)) a

-- Construct right, wrong, and out values
right, wrong, out :: Int -> ([Int], [Int], [Int])
right i = ([i],[] ,[] )
wrong i = ([] ,[i],[] )
out   i = ([] ,[] ,[i])

mkFeedback :: String -> [Int -> ([Int], [Int], [Int])] -> Feedback
mkFeedback guess
  = M.fromListWith (flip (<>))
  . zipWith3 (\c i f -> (c, f i)) guess [0..]
  . take 5 . (++ [out,out,out,out,out])


-- Using precomputed feedback

-- | Cache carries data representing precomputed functions
data Cache = Cache
  { feedbackCache       :: U.Vector Word8
  , feedbackAppCache    :: V.Vector IntSet
  , applyFeedbackCached :: Int -> Int -> IntSet -> IntSet
  }

readCache :: IO Cache
readCache = do
  feedbackCache    <- decodeFile @(U.Vector Word8) "feedbackcache"
  feedbackAppCache <- V.map (IS.fromAscList . map fromIntegral . U.toList)
                  <$> decodeFile @(V.Vector (U.Vector Word16)) "feedbackappcache"
  let applyFeedbackCached answerIx guessIx = IS.intersection cached where
        cached             = feedbackAppCache `V.unsafeIndex` feedbackAppCacheIx
        feedbackAppCacheIx = guessIx * numFeedbacks + fromIntegral feedbackEnum
        feedbackEnum       = feedbackCache `U.unsafeIndex` (answerIx * numWords + guessIx)
        numWords           = V.length feedbackAppCache `div` numFeedbacks
  return Cache{..}

-- Guess!

-- | Given a list of words, brute force the guess that minimizes the number
-- of valid words after getting feedback on the guess.
--
-- When the flag in True, play on "hard-mode". Only guess from words that
-- match previous feedback.
bestGuess :: Env -> Cache -> IntSet -> Int
bestGuess Env{..} Cache{..} words
  | IS.null words      = error "bestGuess: empty list"
  | IS.size words == 1 = IS.findMin words
  | otherwise          = fst $ minimumBy (comparing snd) $ zip guesses weights where
  guesses  = if hardMode then IS.toList words else allWordIxs
  weights  = map (sortBy (comparing Down) . map IS.size) filtered
  filtered = map (\guess -> map (\answer -> mk answer guess) (IS.toList words)) guesses
  mk a g   = applyFeedbackCached a g words

-- Batch

-- | Run the game against every word in the word list. When flag is True play on hard-mode.
batch :: Env -> IO ()
batch env@Env{..} = do
  hSetBuffering stdout LineBuffering
  cache <- readCache
  mapM_ print $ zip allWords (parMap rdeepseq (playKnown env cache) allWordIxs)

-- | Run the game against a specific known answer. When flag is True play on hard-mode.
playKnown :: Env -> Cache -> Int -> [String]
playKnown env@Env{..} cache@Cache{..} answer = reverse $ map enumToWord start where
  start = go 6 (IS.fromAscList allWordIxs) (wordToEnum firstGuess) []
  go n words guess guesses
    | guess == answer = guess : guesses                          -- Found the answer!
    | n == 1          = reverse (IS.toList words) ++ guesses     -- Did not find the answer in time. Dump all remaining words as possible guesses.
    | otherwise       = go (n-1) words' guess' (guess : guesses) -- Make a new guess and decrease the round counter.
    where words' = applyFeedbackCached answer guess words
          guess' = bestGuess env cache words'

-- Interactive

-- | Run the game interactively, asking the user to provide feedback against
-- an unknown word. When flag is True play on hard-mode.
interactive :: Env -> IO ()
interactive env@Env{..} = do
  cache <- readCache
  let bestGuess' = enumToCharIxs . bestGuess env cache . IS.fromAscList . map charIxsToEnum
      go guess words = do
        words' <- flip applyFeedback words <$> promptFeedback guess
        putStrLn $ "remaining words: " ++ show (map fromCharIxs words')
        go (bestGuess' words') words'
  go (toCharIxs firstGuess) allCharIxs

-- | Prompt the user for feedback on a guess.
promptFeedback :: CharIxs -> IO Feedback
promptFeedback guess = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "guess: " ++ fromCharIxs guess
  putStr "enter feedback (r: right, w: wrong, o: out): "
  mkFeedback (fromCharIxs guess) . map toRWO <$> getLine
  where
  toRWO c = case c of 'r' -> right; 'R' -> right
                      'w' -> wrong; 'W' -> wrong
                      _   -> out

-- Utils

-- | Each of the 5 characters in a word can be one of three values: Right, Wrong, Out
numFeedbacks :: Int
numFeedbacks = 3^5

-- | Take a word to the internal uncached representation.
toCharIxs :: String -> CharIxs
toCharIxs = M.fromListWith (flip (++)) . flip zip (map pure [0..])

-- | Recover a word from the internal representation.
fromCharIxs :: CharIxs -> String
fromCharIxs = map snd . sort . concatMap (\(c, ns) -> map (,c) ns) . M.toList

decodeFile :: Serialize a => [Char] -> IO a
decodeFile name = fromRight (error $ "could not decode " ++ name)
                . decodeLazy <$> BS.readFile ("data" </> name)
