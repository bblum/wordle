import Control.Arrow
import Control.Monad
import qualified Data.Map as M
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Debug.Trace

wordlist = "/usr/share/dict/words"

-- TODO: account for when there are multiple of a letter, so At and NotAt state are both relevant
data LetterState = Absent | NotAt [Int] | At [Int] deriving (Eq, Show)

guess :: [String] -> M.Map Char LetterState -> (String, [String])
guess words state = guess' $ filter (all possible2 . zip [0..] . map query) $ filter possible1 words
    where query c = (c, M.lookup c state)
          knownletters = M.keys $ M.filter (/= Absent) state
          possible1 word = length (nub $ word ++ knownletters) <= length word
          possible2 (i, (c, _)) | any conflict $ M.toList state = False
              where conflict (c2, At l) | c2 /= c && elem i l = True
                    conflict _ = False
          possible2 (i, (_, Just (NotAt l))) = not $ elem i l
          possible2 (i, (_, Just Absent)) = False
          possible2 (i, (_, _)) = True
          --
          guess' [] = error "fuck, puzzle is impossible"
          -- kind of hax, can i do better? is 2 better than, say, 3 here?
          guess' answers | length answers <= 2 = (last $ sortOn (length . nub) answers, answers)
          guess' answers = (maximumBy (comparing info) words, answers)
              -- TODO: account for when there are multiple yellow results for a single letter
              -- TODO: when 'state' has as many entries as the length of the word, stop unknowns
              where info word = (cols, (1 + yellowsmoved) * (1 + unknownsprobed), frequencies)
                    -- TODO: try this sorting order instead, see which does better
                    -- info word = ((1 + yellowsmoved) * (1 + unknownsprobed), frequencies)
                        where cols = count unknowncol $ zip word $ map nub $ transpose answers
                              yellowsmoved = count yellowmoved $ zip [0..] $ map query word
                              unknownsprobed = count unknown $ nub word
                              frequencies = sum $ map (fromMaybe 0 . flip lookup freqs) $ nub word
                    unknowncol (c, c2s) = length c2s > 1 && elem c c2s
                    yellowmoved (i, (c, _)) | any othergreen $ M.assocs state = False
                        where othergreen (c2, At l) = c /= c2 && elem i l
                              othergreen _ = False
                    yellowmoved (i, (_, Just (NotAt l))) = not $ elem i l
                    yellowmoved _ = False
                    freqs = map (head &&& length) $ group $ filter unknown $ sort $ concat answers
                    unknown c = M.notMember c state
                    count f = length . filter f

-- TODO: maybe you can analyze adjacency patterns in the dictionary to rule stuff out
-- like q -> u
-- then again maybe too niche to be worth the LOC

-- TODO: rewrite this to interact with the website with just colors
-- TODO: foldl collect a string of colors and pretty print them
update state secret word = foldl checkletter state $ zip3 [0..] secret word
    -- TODO: consume letters in 'secret' when computing yellows (not that it matters here?)
    where checkletter state (i,s,w) | s == w = M.alter mkgreen w state
              where mkgreen (Just (At l)) = Just $ At $ nub $ sort $ i:l
                    mkgreen (Just Absent) = error "fuck, letter reappeared 1"
                    mkgreen _ = Just $ At [i]
          checkletter state (i,_,w) | elem w secret = M.alter mkyellow w state
              where mkyellow (Just (At l)) = Just $ At l -- don't downgrade better info
                    -- TODO: when you have all but one, promote it to a At
                    mkyellow (Just (NotAt l)) = Just $ NotAt $ nub $ sort $ i:l
                    mkyellow (Just Absent) = error "fuck, letter reappeared 2"
                    mkyellow Nothing = Just $ NotAt [i]
          checkletter state (_,_,w) | M.lookup w state == Just Absent = state -- ok
          checkletter state (_,_,w) | isJust $ M.lookup w state = error "fuck, letter vanished"
          checkletter state (_,_,w) = M.insert w Absent state

main = do
    putStrLn "input secret word:"
    (secret:feedback) <- lines <$> getContents
    --
    dict <- filter (all isLower) <$> lines <$> readFile wordlist
    when (not $ elem secret dict) $ error "secret word is too secret for gcc"
    putStrLn $ "secret: " ++ map toUpper secret
    let words = filter ((== length secret) . length) dict
    --
    let solve s = do
            let (ans, answers) = guess words s
            let s2 = update s secret ans
            let status = if ans == fst (guess words s2) then "answer" else "trying"
            putStrLn $ status ++ ": " ++ ans ++ " " ++
                if length answers > 20 then "(" ++ show (length answers) ++ " possibilities)"
                else show $ map nub $ transpose answers
            if status == "answer" then return () else solve s2
    solve M.empty

