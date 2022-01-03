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
import System.Random.Stateful

wordlist = "dictionary.txt"

-- TODO: account for when there are multiple of a letter, so At and NotAt state are both relevant
data LetterState = Absent | NotAt [Int] | At [Int] deriving (Eq, Show)
type Knowledge = M.Map Char LetterState

-- TODO: filter possible `answers` from `take 20000 commons`, not dictionary!
-- should narrow things down a bunch, like, a BUNCH, maybe
guess :: Int -> [String] -> Knowledge -> (String, [String])
guess seed words state = guess' $ filter (all possible2 . zip [0..] . map query) $ filter possible1 words
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
          guess' answers = (bestword, answers)
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
              where info word = ((cols, (1 + yellowsmoved) * (1 + unknownsprobed)), frequencies)
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
                    -- for variety, choose randomly from among the tied best candidates
                    -- (ignoring the letter frequency metric which is wildly noisy)
                    -- (only matters to do on the first word though, nexts will deviate anyway)
                    infowords = reverse $ sortOn snd $ map (\w -> (w, info w)) words
                    tiedwords = map fst $ takeWhile ((== (fst $ snd $ head infowords)) . fst . snd) infowords
                    bestword = if M.null state then tiedwords !! (mod seed $ length tiedwords)
                               else maximumBy (comparing info) words

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
          checkletter' (state,msg) (_,w,0) | M.member w state = (state, msg ++ c_k ++ [w]) -- same
          checkletter' (state,msg) (_,w,0) = (M.insert w Absent state, msg ++ c_k ++ [w])
          finalize (state,msg) = (M.map promote state, msg ++ c_x)
              where greens = M.foldr (\x gs -> case x of At l -> l ++ gs; _ -> gs) [] state
                    -- when all but 1 position is yellow (or other green), process of elim it
                    -- this is most helpful for ~6ish letter words, often saving 1-2 guesses
                    promote (NotAt l) | length notats == n = At $ [0..n] \\ notats
                        where notats = nub $ l ++ greens
                              n = length word - 1
                    promote x = x

-- terrible IPC code to the browser begins here

shellcommand cmd = do
    (_, Just stdout_h, _, ph) <- createProcess $ (shell cmd) { std_out = CreatePipe }
    code <- waitForProcess ph
    when (code /= ExitSuccess) $ error $ "command '" ++ cmd ++ "' failed: " ++ show code
    hGetContents stdout_h

withbackoff :: (String -> Maybe a) -> String -> IO a
withbackoff = withbackoff' 5 500 -- initial millis; max millis
withbackoff' millis maxmillis f cmd = do
    output <- shellcommand cmd
    case f output of
        Just result -> return result
        Nothing -> do
            when (millis == maxmillis) $ putStrLn $ cmd ++ ": waiting (" ++ show millis ++ "ms)"
            threadDelay (millis * 1000);
            withbackoff' (min maxmillis $ millis * 2) maxmillis f cmd

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

playone words wordlength = do
    --
    seed <- randomIO :: IO Int
    let solve s rownum = do
            let (ans, answers) = guess seed words s
            -- TODO: solve race condition if num letters changes here
            -- maybe: if backoff fails, then send backspaces and start over
            -- type attempt into the browser
            shellcommand $ "./glue.sh " ++ ans
            -- read the row feedback from the webpage
            (parsedans, feedback) <- withbackoff (parserowresults rownum) "pbpaste"
            when (parsedans /= ans) $ error $ "parsed ans different: " ++ ans ++ " " ++ parsedans
            -- update internal knowledge
            let (s2, msg) = update s (\i _ -> feedback !! i) ans
            let status = if ans == fst (guess seed words s2) then "solved" else "trying"
            putStrLn $ status ++ ": " ++ msg ++ " " ++
                if length answers > 20 then "(" ++ show (length answers) ++ " possibilities)"
                else show $ map nub $ transpose answers
            if status == "solved" then return () else solve s2 $ rownum + 1
    solve M.empty 0

playmany words wordlength = do
    putStrLn "================"
    playone words wordlength
    threadDelay $ 400 * 1000 -- just for legibility
    shellcommand $ "./glue.sh \"\""
    playmany words wordlength

main = do
    dict <- filter (all isLower) <$> lines <$> readFile wordlist
    wordlength <- withbackoff parsewordlength "pbpaste"
    let words = filter ((== wordlength) . length) dict
    threadDelay $ 1000 * 1000
    playmany words wordlength
