import Control.Arrow
import Control.Monad
import qualified Data.Map as M
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Debug.Trace

wordlist = "/usr/share/dict/words"

data Feedback = Black | Yellow | Green

-- data SolveState = S { yellows :: S.Set (Char, [Int]), greens :: S.Set (Char, Int), blacks :: S.Set Char }

-- TODO: account for when there are multiple of a letter, so At and NotAt state are both relevant
data LetterState = Absent | NotAt [Int] | At [Int] deriving (Eq, Show)
type SolveState = M.Map Char LetterState

-- TODO: try an ord instance that multiplies (1+yellows) * (1+unknowns), see which does better
data InformationMetric = IM { yellowsMoved :: Int, unknownsProbed :: Int, letterFrequenciesAmongUnknowns :: Int } deriving (Eq, Ord, Show)

count f = length . filter f

guess :: [String] -> SolveState -> String
guess dict state = guess' $ filter (all possible . zip [0..] . map query) dict
    where query = flip M.lookup state
          unknown = flip M.notMember state
          possible (i, Just (At l)) = elem i l
          possible (i, Just (NotAt l)) = not $ elem i l
          possible (i, Just Absent) = False
          possible (i, Nothing) = True
          --
          guess' [] = error "fuck, puzzle is impossible"
          guess' [ans] = ans
          guess' answers = maximumBy (comparing info) dict
              -- TODO: account for when there are multiple yellow results for a single letter
              where info word = IM (count yellowmoved $ zip [0..] $ map query word) (count unknown $ nub word) (sum $ map (fromMaybe 0 . flip lookup freqs) $ nub word)
                    yellowmoved (i, Just (NotAt l)) = not $ elem i l
                    yellowmoved _ = False
                    freqs = map (head &&& length) $ group $ filter unknown $ sort $ concat answers

-- TODO: rewrite this to interact with the website with just colors
-- TODO: foldl collect a string of colors and pretty print them
update state secret word = foldl checkletter state $ zip3 [0..] secret word
    -- TODO: consume letters in 'secret' when computing yellows (not that it matters here?)
    where checkletter state (i,s,w) | s == w = M.alter atify w state
              where atify (Just (At l)) = Just $ At $ nub $ sort $ i:l
                    atify (Just Absent) = error "fuck, letter reappeared 1"
                    atify _ = Just $ At [i]
          checkletter state (i,_,w) | elem w secret = M.alter notatify w state
              where notatify (Just (At l)) = Just $ At l -- don't downgrade better info
                    notatify (Just (NotAt l)) = Just $ NotAt $ nub $ sort $ i:l
                    notatify (Just Absent) = error "fuck, letter reappeared 2"
                    notatify Nothing = Just $ NotAt [i]
          checkletter state (_,_,w) | M.lookup w state == Just Absent = state -- ok
          checkletter state (_,_,w) | isJust $ M.lookup w state = error "fuck, letter vanished"
          checkletter state (_,_,w) = M.insert w Absent state

main = do
    print "input secret word:"
    (secret0:feedback) <- lines <$> getContents
    let secret = map toLower secret0
    --
    dict <- filter (all isLower) <$> lines <$> readFile wordlist
    when (not $ elem secret dict) $ error "secret word is too secret for gcc"
    print $ "secret: " ++ map toUpper secret
    --
    let candidates = filter ((== length secret) . length) dict
    --
    let solve s = traceShow ("guess:  " ++ word) $ if s == s' then word else solve s'
            where word = guess candidates s
                  s' = update s secret word
    print $ solve M.empty

