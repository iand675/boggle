module Main where
import Control.Monad
import Control.Monad.RWS
import Data.List
import Data.Maybe
import Data.Vector ((!?), (!))
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as T
import qualified Data.Trie as Trie

{-
4x4 grid
find all words
-}

sample :: V.Vector (V.Vector Char)
sample = V.fromList $ map V.fromList $
  [ "hello"
  , "dogsg"
  , "weird"
  , "tests"
  ]

type PrefixTree = Trie.Trie ()

type Grid = V.Vector (V.Vector Char)
type Index = (Int, Int)

-- keeps track of read only data (prefix tree & grid), and maintains an updatable set of all potential words
type BoggleContext = RWST (PrefixTree, Grid) () (S.Set T.Text) IO

runBoggleWordFinder :: PrefixTree -> Grid -> BoggleContext () -> IO (S.Set T.Text)
runBoggleWordFinder p g m = fmap fst $ execRWST m (p, g) S.empty

-- runs the word finding algorithm starting at each coordinate in the grid
traverseAll :: PrefixTree -> Grid -> IO (S.Set T.Text)
traverseAll p g = runBoggleWordFinder p g $ mapM_ (\ix -> findPotentialWords S.empty ix "") startingPoints
  where
    startingPoints = [ (x, y) | x <- [0 .. V.length g], y <- [0 .. V.length (g ! 0) ]]

-- looks up a coordinate value, returning nothing if the coordinate does not exist
gridLookup :: Index -> Grid -> Maybe Char
gridLookup (x', y') g = do
  col <- g !? x'
  col !? y'

adjacentIndexes :: Int -> Int -> [Index]
adjacentIndexes x y =
  [ (x - 1, y)
  , (x + 1, y)
  , (x, y - 1)
  , (x, y + 1)
  , (x - 1, y - 1)
  , (x + 1, y + 1)
  , (x - 1, y + 1)
  , (x + 1, y - 1)
  ]

shouldTraverse :: S.Set Index -> (Index, Maybe Char) -> Bool
shouldTraverse visited (ix, v) = not (S.member ix visited) && v /= Nothing

-- lookup values from a list of adjacent indexes, skipping them if they have already been traversed or are out of bounds
adjacentValues :: S.Set Index -> [Index] -> Grid -> [(Index, Char)]
adjacentValues visited indexes grid = map (\(ix, mv) -> (ix, fromJust mv)) $ untraversedValues $ zip indexes $ map (\ix -> gridLookup ix grid) indexes
  where untraversedValues = filter (shouldTraverse visited)

-- recursively accumulate potential words in the BoggleContext
-- only potential words that are valid trie prefixes are added to the set and recursed upon
findPotentialWords :: S.Set Index -> Index -> String -> BoggleContext ()
findPotentialWords visited (x, y) currentWord = do
  (prefixTree, grid) <- ask
  potentialWords <- get
  let indexes = adjacentIndexes x y
      -- add the current index to the set of indexes visited for this recursive traversal
      visited' = S.insert (x, y) visited
      nextChars = adjacentValues visited indexes grid
      -- make the currentWord for each of the indexes that will be visited next
      visitNext = map (\(ix, c) -> (ix, c : currentWord)) nextChars
      runNext (nextIx, nextWord) = findPotentialWords visited' nextIx nextWord
      potentialWords' = S.union (S.fromList $ filter (\w -> not $ Trie.null $ Trie.submap (encodeUtf8 w) prefixTree) $ map (T.pack . snd) visitNext) potentialWords
  -- store the current prefixes that might be valid words
  put potentialWords'
  -- for each of the index/current word pairs, find potential words
  mapM_ runNext visitNext

-- build a trie from the dictionary of words
retrieveWords :: IO PrefixTree
retrieveWords = do
  contents <- T.readFile "/usr/share/dict/words"
  return $! Trie.fromList $ zip (map encodeUtf8 $ T.lines $ T.toLower contents) $ repeat ()

main :: IO ()
main = do
  wordTree <- retrieveWords
  potentialWords <- traverseAll wordTree sample
  -- print only words that are valid according to the dictionary and that are longer than three letters
  mapM_ T.putStrLn $ filter (\w -> T.length w >= 3 && Trie.member (encodeUtf8 w) wordTree) $ S.toList potentialWords

