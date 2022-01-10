{-# language BlockArguments      #-}
{-# language DerivingVia         #-}
{-# language ImportQualifiedPost #-}
{-# language MultiWayIf          #-}
{-# language RankNTypes          #-}
{-# language RecordWildCards     #-}
{-# language TupleSections       #-}
{-# language TypeApplications    #-}

import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST
import Control.Parallel.Strategies
import Data.ByteString.Lazy         qualified as BS
import Data.Either
import Data.IntMap.Strict           (IntMap)
import Data.IntMap.Strict           qualified as IM
import Data.IntSet                  (IntSet)
import Data.IntSet                  qualified as IS
import Data.List
import Data.Map.Strict              (Map)
import Data.Map.Strict              qualified as M
import Data.Maybe
import Data.Ord
import Data.Serialize
import Data.Vector.Serialize
import Data.Vector                  qualified as V
import Data.Vector.Generic          qualified as VG
import Data.Vector.Generic.Mutable  qualified as VM
import Data.Vector.Unboxed          qualified as U
import Data.Word
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Unsafe

-- | usage: wordle [[--hard] [--batch]|[--cache-feedback] [--cache-feedback-app]]
--
-- OPTIONS
--   --hard               Play on hard-mode
--   --batch              Play against all known words
--   --cache-feedback     Write the data/feedbackcache file
--   --cache-feedback-app Write the data/feedbackappcache file
main :: IO ()
main = do
  args <- getArgs
  let flags = ["--hard", "--batch", "--cache-feedback", "--cache-feedback-app"]
      parse flag (fs, as) = (elem flag as : fs, delete flag as)
      ([optHard, optBatch, optCacheFB, optCacheFBA], args') = foldr parse ([], args) flags
  env  <- makeEnv optHard
  if | not . null $ args'              -> error "usage: wordle [[--hard] [--batch]|[--cache-feedback] [--cache-feedback-app]]"
     | optBatch                        -> runReaderT batch env
     -- | not $ optCacheFB || optCacheFBA -> interactive optHard
     | otherwise                       -> return ()
  when optCacheFB  do
     BS.writeFile "data/feedbackcache"    $ encodeLazy $ makeFeedbackCache env
  when optCacheFBA do
     notExistsFeedbackCache <- not <$> doesFileExist "data/feedbackcache"
     when notExistsFeedbackCache do
       BS.writeFile "data/feedbackcache"  $ encodeLazy $ makeFeedbackCache env
     BS.writeFile "data/feedbackappcache" $ encodeLazy $ makeFeedbackAppCache env

data Env = Env
  { allwords         :: [String]
  , allIxMaps        :: [IxMap]
  , allNs            :: [Int]
  , wordToEnum       :: String -> Int
  , enumToWord       :: Int -> String
  , ixMapToEnum      :: IxMap -> Int
  , enumToIxMap      :: Int -> IxMap
  , feedbackCache    :: U.Vector Word8
  , feedbackAppCache :: V.Vector IntSet
  , hard             :: Bool
  }

makeEnv :: Bool -> IO Env
makeEnv hard = do
  -- | The Wordle word list, extracted from the js on the site.
  -- There are actually two word list, which I call the answer list
  -- and the guess list. The answer must come from the answer list,
  -- but a guess may be from either list. In practice guessing only
  -- from the answer list works fine.
  allwords         <- lines <$> readFile "data/answerlist"
  feedbackCache    <- (decodeFile "feedbackcache" :: IO (U.Vector Word8))
  feedbackAppCache <- V.map (IS.fromAscList . map fromIntegral . U.toList)
                  <$> (decodeFile "feedbackappcache" :: IO (V.Vector (U.Vector Word16)))
  -- | The word list in the internal representation.
  let allIxMaps     = map toIxMap allwords
      allNs         = [0..length allwords-1]
      wordToEnum    = (M.!) . M.fromList $ zip allwords [0..]
      enumToWord    = (M.!) . M.fromList $ zip [0..] allwords
      ixMapToEnum   = (M.!) . M.fromList $ zip allIxMaps [0..]
      enumToIxMap   = (M.!) . M.fromList $ zip [0..] allIxMaps
  return Env{..}

decodeFile name = fromRight (error $ "could not decode " ++ name)
                . decodeLazy <$> BS.readFile ("data" </> name)

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

-- | Given a list of words, brute force the guess that minimizes the number
-- of valid words after getting feedback on the guess.
--
-- When the flag in True, play on "hard-mode". Only guess from words that
-- match previous feedback.
bestGuess :: Env -> IntSet -> Int
bestGuess env@Env{..} words
  | IS.null words      = error "bestGuess: empty list"
  | IS.size words == 1 = IS.findMin words
  | otherwise          = fst $ minimumBy (comparing snd) $ zip guesses weights where
  guesses  = if hard then IS.toList words else allNs
  weights  = map (sortBy (comparing Down) . map IS.size) filtered
  filtered = map (\guess -> map (\answer -> mk answer guess) (IS.toList words)) guesses
  mk a g   = applyFeedbackCached env a g words
{-
bestGuess :: Bool -> [IxMap] -> IxMap
bestGuess _    []     = error "bestGuess: empty list"
bestGuess _    [word] = word
bestGuess hard words  = fst $ minimumBy (comparing snd) $ zip guesses weights where
  guesses  = if hard then words else allIxMaps
  weights  = map (sortBy (comparing Down) . map length) filtered
  filtered = map (\guess -> map (\answer -> mk answer guess) words) guesses
  mk a g   = applyFeedback (makeFeedback a g) words
-}

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
{-
-- | Run the game interactively, asking the user to provide feedback against
-- an unknown word. When flag is True play on hard-mode.
interactive :: Bool -> IO ()
interactive hard = go (toIxMap "raise") allIxMaps where
  go guess words = do
    words' <- flip applyFeedback words <$> promptFeedback guess
    putStrLn $ "remaining words: " ++ show (map fromIxMap words')
    go (bestGuess hard words') words'

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
-}
-- Batch

-- | Run the game against every word in the word list. When flag is True play on hard-mode.
batch :: ReaderT Env IO ()
batch = ask >>= \env@Env{..} -> do
  lift $ hSetBuffering stdout LineBuffering
  lift $ mapM_ print $ parMap rdeepseq (second $ playKnown env) (zip allwords allNs)

-- | Run the game against a specific known answer. When flag is True play on hard-mode.
playKnown :: Env -> Int -> [String]
playKnown env@Env{..} answer = reverse . map enumToWord $ go 6 (IS.fromAscList allNs) (wordToEnum "raise") [] where
  go n words guess guesses
    | guess == answer = guess : guesses                           -- Found the answer!
    | n == 1          = reverse (IS.toList words) ++ guesses      -- Did not find the answer in time. Dump all remaining words as possible guesses.
    | otherwise       = go (n-1) words' guess' (guess : guesses)  -- Make a new guess and decrease the round counter.
    where words' = applyFeedbackCached env answer guess words
          guess' = bestGuess env words'
{-
-- | Run the game against every word in the word list. When flag is True play on hard-mode.
batch :: Bool -> IO ()
batch hard = do
  hSetBuffering stdout LineBuffering
  mapM_ print $ parMap rdeepseq (second $ playKnown hard) (zip allwords allIxMaps)

-- | Run the game against a specific known answer. When flag is True play on hard-mode.
playKnown :: Bool -> IxMap -> [String]
playKnown hard answer = reverse . map fromIxMap $ go 6 allIxMaps (toIxMap "raise") [] where
  go n words guess guesses
    | guess == answer = guess : guesses                           -- Found the answer!
    | n == 1          = reverse words ++ guesses                  -- Did not find the answer in time. Dump all remaining words as possible guesses.
    | otherwise       = go (n-1) words' guess' (guess : guesses)  -- Make a new guess and decrease the round counter.
    where words' = applyFeedback (makeFeedback answer guess) words
          guess' = bestGuess hard words'
-}

-- Cache

feedbackToEnum :: Feedback -> Int
feedbackToEnum = enum . foldMap snd . M.toList where
  enum = (\(r, w, _) -> sum $ map ((2*).(3^)) r ++ map (3^) w)

enumToFeedback :: String -> Int -> Feedback
enumToFeedback guess = M.fromListWith (flip (<>)) . zipWith3 (\c i f -> (c, f i)) guess [0..] . unfold where
  unfold = take 5 . (++ [o,o,o,o,o]) . go id
  go acc 0 = acc []
  go acc n | b == 2 = go (acc . (r:)) a
           | b == 1 = go (acc . (w:)) a
           | b == 0 = go (acc . (o:)) a
           where (a, b) = n `divMod` 3
  r i = ([i],[] ,[] )
  w i = ([] ,[i],[] )
  o i = ([] ,[] ,[i])

feedbackToEnum' :: Env -> Int -> Int -> Int
feedbackToEnum' Env{..} nAnswer nGuess = fromIntegral $ feedbackCache `U.unsafeIndex` (nAnswer * n + nGuess) where
  n = length allIxMaps

feedbackAppToEnum :: Env -> Int -> Int -> Int
feedbackAppToEnum env nAnswer nGuess = nGuess * 3^5 + feedbackToEnum' env nAnswer nGuess

makeFeedbackCache :: Env -> U.Vector Word8
makeFeedbackCache Env{..} = U.fromList do
  answer <- allIxMaps
  guess  <- allIxMaps
  return . fromIntegral . feedbackToEnum $ makeFeedback answer guess

makeFeedbackAppCache :: Env -> V.Vector (U.Vector Word16)
makeFeedbackAppCache Env{..} = V.fromList do
  guess     <- allwords
  nFeedback <- [0..3^5-1]
  return . U.fromList . map (fromIntegral . ixMapToEnum) $ applyFeedback (enumToFeedback guess nFeedback) allIxMaps

applyFeedbackCached :: Env -> Int -> Int -> IntSet -> IntSet
applyFeedbackCached env@Env{..} nAnswer nGuess = IS.intersection cached where
  nFeedbackApp = feedbackAppToEnum env nAnswer nGuess
  cached       = feedbackAppCache `V.unsafeIndex` nFeedbackApp
