import Control.Arrow
import Control.Concurrent
import Control.Monad
import qualified Data.Map as M
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import GHC.IO.Exception
import GHC.IO.Handle
import System.Process

wordlist = "dictionary.txt"

-- TODO: account for when there are multiple of a letter, so At and NotAt state are both relevant
data LetterState = Absent | NotAt [Int] | At [Int] deriving (Eq, Show)
type Knowledge = M.Map Char LetterState

guess :: [String] -> Knowledge -> (String, [String])
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
          -- if there are 2 possibilities, the heuristic will do us no good. just pick one.
          -- (in fact, it might hurt, since it doesn't necessarily pick from `answers`).
          -- kind of hax, can i do better? is 2 better than, say, 3 here?
          -- TODO: check with some sort of stress test if there's any place where 3 matters
          guess' answers | length answers <= 2 = (last $ sortOn (length . nub) answers, answers)
          guess' answers = (maximumBy (comparing info) words, answers)
              -- `info` is the information-gaining heuristic of candidate words to guess
              -- i prioritize attacking unknown columns first, then (simultaneously) guessing
              -- already-clued-as-yellow letters in different columns & trying untested letters,
              -- then finally commonality of untested letters among remaining possible answers.
              -- the last one one is mostly for the first guess, so i try vowels before Q/X/Z &c.
              -- the first one is mostly for the last guess, so i lean on process of elim harder.
              --
              -- TODO: account for when there are multiple yellow results for a single letter
              -- TODO: try this sorting order instead, see which does better:
              --       info word = (cols, yellowsmoved, unknownsprobed, frequencies)
              where info word = (cols, (1 + yellowsmoved) * (1 + unknownsprobed), frequencies)
                        where cols = count unknowncol $ zip word $ map nub $ transpose answers
                              yellowsmoved = count yellowmoved $ zip [0..] $ map query word
                              -- TODO: check if `doneprobing` actually does anything
                              -- TODO: use a stress test :)
                              -- because i think the 'cols' metric probably subsumes it
                              unknownsprobed = if doneprobing then 0 else count unknown $ nub word
                              doneprobing = length knownletters == length word
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

c_x = "\27[00m"
c_g = "\27[01;32m"
c_y = "\27[01;33m"
c_k = "\27[01;31m"

-- TODO: maybe you can analyze adjacency patterns in the dictionary to rule stuff out
-- like q -> u
-- then again maybe too niche to be worth the LOC

-- checker's return value shall be 0 for red, 1 for yellow, 2 for green
-- TODO: allow checker to be stateful (ie consume duplicate letters) when computing yellows
update :: Knowledge -> (Int -> Char -> Int) -> String -> (Knowledge, String)
update state checker word = finalize $ foldl checkletter (state,"") $ zip [0..] word
    where checkletter x (i,w) = checkletter' x (i, w, checker i w)
          checkletter' (state,msg) (i,w,2) = (M.alter mkgreen w state, msg ++ c_g ++ [w])
              where mkgreen (Just (At l)) = Just $ At $ nub $ sort $ i:l
                    mkgreen _ = Just $ At [i]
          checkletter' (state,msg) (i,w,1) = (M.alter mkyellow w state, msg ++ c_y ++ [w])
              where mkyellow (Just (At l)) = Just $ At l -- don't downgrade better info
                    mkyellow (Just (NotAt l)) = Just $ NotAt $ nub $ sort $ i:l
                    mkyellow Nothing = Just $ NotAt [i]
          checkletter' (state,msg) (_,w,0) = (M.insert w Absent state, msg ++ c_k ++ [w])
          finalize (state,msg) = (M.map promote state, msg ++ c_x)
              where greens = M.foldr (\x gs -> case x of At l -> l ++ gs; _ -> gs) [] state
                    -- when all but 1 position is yellow (or other green), process of elim it
                    -- this is most helpful for ~6ish letter words, often saving 1-2 guesses
                    promote (NotAt l) | length notats == n = At $ [0..n] \\ notats
                        where notats = nub $ l ++ greens
                              n = length word - 1
                    promote x = x

-- ^^^ secret-blind code ^^^  vvv secret-aware code for automatic mode vvv

check secret i w | secret !! i == w = 2
check secret _ w | elem w secret = 1
check secret _ _ = 0

-- TODO: debug 'sentimentalisms' non termination
-- it's also obviously a case where 3 helps instead of 2
-- but the actual problem is that 'e' is green elsewhere in the world,
-- so the knowledge of it yellow in 2nd to last place doesn't get to contribute to progress
-- you probably don't have to fully solve the duplicate-letters problem to fix this
main = do
    putStrLn "input secret word:"
    (secret:feedback) <- lines <$> getContents
    --
    dict <- filter (all isLower) <$> lines <$> readFile wordlist
    when (not $ elem secret dict) $ error $ c_k ++ "secret word is too secret for gcc" ++ c_x
    putStrLn $ "secret: " ++ map toUpper secret
    let words = filter ((== length secret) . length) dict
    --
    let solve s = do
            let (ans, answers) = guess words s
            let (s2, msg) = update s (check secret) ans
            let status = if ans == fst (guess words s2) then "solved" else "trying"
            putStrLn $ status ++ ": " ++ msg ++ " " ++
                if length answers > 20 then "(" ++ show (length answers) ++ " possibilities)"
                else show $ map nub $ transpose answers
            when (status == "solved" && ans /= secret) $ error "shit, i got stuck"
            if status == "solved" then return () else solve s2
    solve M.empty

-- terrible IPC code to the browser begins here

shellcommand cmd = do
    (_, Just stdout_h, _, ph) <- createProcess $ (shell cmd) { std_out = CreatePipe }
    code <- waitForProcess ph
    when (code /= ExitSuccess) $ error $ "command '" ++ cmd ++ "' failed: " ++ show code
    hGetContents stdout_h

backoffuntil = backoffuntil' 5 500 -- initial millis; max millis
backoffuntil' millis maxmillis f cmd = do
    output <- shellcommand cmd
    if f output then return output
    else do
        -- TODO comment this out
        putStrLn $ cmd ++ ": waiting for expected output (" ++ show millis ++ "ms)"
        threadDelay (millis * 1000);
        backoffuntil' (min maxmillis $ millis * 2) maxmillis f cmd

parsewordlength :: String -> Maybe Int
parsewordlength html = fmap (read . head) $ find isletters $ map words $ splitOn "<p>" html
    where isletters (_:str:_) = take 7 str == "letters"
          isletters _ = False

parserowresults :: Int -> String -> Maybe (String, [Int])
parserowresults n html = parserow <$> nthrow (splitOn "<div class=\"Row Row-locked-in\">" html)
    where nthrow rows | length rows < n+2 = Nothing
          nthrow rows = Just $ rows !! (n+1) -- first elem will always be headers junk
          parserow row = unzip $ map (parsecol . splitOn ">") $ takeWhile (not . null) $ splitOn "</div>" row
          parsecol ["<div class=\"Row-letter letter-absent\"",[c]] = (c,0)
          parsecol ["<div class=\"Row-letter letter-elsewhere\"",[c]] = (c,1)
          parsecol ["<div class=\"Row-letter letter-correct\"",[c]] = (c,2)
          parsecol col = error $ "unexpected parse: " ++ concat col
