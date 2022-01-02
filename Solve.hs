import Control.Arrow
import Control.Monad
import qualified Data.Map as M
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord

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
              -- TODO: try this sorting order instead, see which does better
              -- info word = ((1 + yellowsmoved) * (1 + unknownsprobed), frequencies)
              where info word = (cols, (1 + yellowsmoved) * (1 + unknownsprobed), frequencies)
                        where cols = count unknowncol $ zip word $ map nub $ transpose answers
                              yellowsmoved = count yellowmoved $ zip [0..] $ map query word
                              -- TODO: check if this actually does anything
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
c_k = "\27[00;31m"

-- TODO: maybe you can analyze adjacency patterns in the dictionary to rule stuff out
-- like q -> u
-- then again maybe too niche to be worth the LOC

-- TODO: rewrite this to interact with the website with just colors
-- TODO: foldl collect a string of colors and pretty print them
update state secret word = fmap (++ c_x) $ foldl checkletter (state,"") $ zip3 [0..] secret word
    -- TODO: consume letters in 'secret' when computing yellows (not that it matters here?)
    where checkletter (state,msg) (i,s,w) | s == w = (M.alter mkgreen w state, msg ++ c_g ++ [w])
              where mkgreen (Just (At l)) = Just $ At $ nub $ sort $ i:l
                    mkgreen (Just Absent) = error "fuck, letter reappeared 1"
                    mkgreen _ = Just $ At [i]
          checkletter (state,msg) (i,_,w) | elem w secret = (M.alter mkyellow w state, msg ++ c_y ++ [w])
              where mkyellow (Just (At l)) = Just $ At l -- don't downgrade better info
                    -- when we have all but 1 position yellow, we could promote it to a green
                    -- but that probably never matters since we solve too fast already
                    mkyellow (Just (NotAt l)) = Just $ NotAt $ nub $ sort $ i:l
                    mkyellow (Just Absent) = error "fuck, letter reappeared 2"
                    mkyellow Nothing = Just $ NotAt [i]
          checkletter (state,msg) (_,_,w) | M.lookup w state == Just Absent = (state, msg ++ c_k ++ [w])
          checkletter (state,_) (_,_,w) | isJust $ M.lookup w state = error "fuck, letter vanished"
          checkletter (state,msg) (_,_,w) = (M.insert w Absent state, msg ++ c_k ++ [w])

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
            let (s2, msg) = update s secret ans
            let status = if ans == fst (guess words s2) then "answer" else "trying"
            putStrLn $ status ++ ": " ++ msg ++ " " ++
                if length answers > 20 then "(" ++ show (length answers) ++ " possibilities)"
                else show $ map nub $ transpose answers
            if status == "answer" then return () else solve s2
    solve M.empty

