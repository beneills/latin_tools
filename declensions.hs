-- Imports
import Data.Maybe (isNothing, fromJust)
import System.Environment
import Data.List
import System.IO
import System.Exit
import System.Random
-- import Database.HDBC
-- import Database.HDBC.Sqlite3

-- Datatypes
type Stem = String
type Translation = String
data Gender = Masc | Fem | Neut
type Declension = (Meta, NounCases String, NounCases String)
type Classification = String -- "<Maj> <Min>"
type Meta = (Translation, Gender, Classification)
type Endings = Declension
type NounCases a = (a, a, a, a, a) -- :: (Nom, Acc, Gen, Dat, Abl)
data Case = Nom | Acc | Gen | Dat | Abl
data Number = Sing | Plu

-- Constants/Parameters
asciiLongChar = '_'

coloring = True

vowels = "aeiou"

charLongA = "ā"
charLongE = "ē"
charLongI = "ī"
charLongO = "ō"
charLongU = "ū"

endingLongARUM = charLongA ++ "rum"
endingLongAS = charLongA ++ "s"
endingLongEI = charLongE ++ charLongI
endingLongEBUS = charLongE ++ "bus"
endingLongERUM = charLongE ++ "rum"
endingLongES = charLongE ++ "s"
endingLongIS = charLongI ++ "s"
endingLongORUM = charLongO ++ "rum"
endingLongOS = charLongO ++ "s"
endingLongUI = charLongI
endingLongUS = charLongU ++ "s"

colors = [("red", "\x1b[31m"),
          ("green", "\x1b[32m"),
          ("yellow", "\x1b[33m"),
          ("blue", "\x1b[34m"),
          ("magenta", "\x1b[35m"),
          ("cyan", "\x1b[36m"),
          ("white", "\x1b[37m")]


genderLetter :: Gender -> String
genderLetter Masc = "M"
genderLetter Fem = "F"
genderLetter Neut = "N"

hint :: Number -> Case -> (String -> String)
hint Sing c = hint' c
hint Plu Nom = ("The "++).(++"s... ")
hint Plu c = (++"s") . hint' c

hint' Nom = ("The "++).(++"... ")
hint' Acc = ("... the "++)
hint' Gen = ("of the "++)
hint' Dat = ("to the "++)
hint' Abl = ("on the "++)

numberHint :: Number -> String
numberHint Sing = "(sing.)"
numberHint Plu = "(plu.)"

-- Utility Functions

colorize :: String -> String -> String
colorize s = if isNothing x || not coloring
             then id
             else (fromJust x ++).(++"\x1b[0m")
  where x = lookup s colors

-- Random selection of declension
randomCase = randomChoice [Nom, Acc, Gen, Dat, Abl]
randomNumber = randomChoice [Sing, Plu]
randomPureDeclension = randomChoice fullPureDeclensions
randomChoice xs = fmap (xs!!) $ randomRIO (0, length xs - 1)

-- Decline a noun!
get :: Number -> Case -> Declension -> String
get Sing  c (_, s, _) = get' c s
get Plu c (_, _, p) = get' c p

get' Nom (x, _, _, _, _) = x
get' Acc (_, x, _, _, _) = x
get' Gen (_, _, x, _, _) = x
get' Dat (_, _, _, x, _) = x
get' Abl (_, _, _, _, x) = x

-- Alter a declension
set :: Number -> Case -> String -> Declension -> Declension
set Sing  c x (m, s, p) = (m, set' c x s, p)
set Plu c x (m, s, p) = (m, s, set' c x p)

set' Nom x (nom, acc, gen, dat, abl) = (x, acc, gen, dat, abl)
set' Acc x (nom, acc, gen, dat, abl) = (nom, x, gen, dat, abl)
set' Gen x (nom, acc, gen, dat, abl) = (nom, acc, x, dat, abl)
set' Dat x (nom, acc, gen, dat, abl) = (nom, acc, gen, x, abl)
set' Abl x (nom, acc, gen, dat, abl) = (nom, acc, gen, dat, x)


-- Map over declension
declensionMap :: (String -> String)
                 -> Declension
                 -> Declension
declensionMap f (m, sing, plu) = (m, caseMap f sing, caseMap f plu)

-- Map over a declension's number
caseMap :: (a -> b)
           -> NounCases a
           -> NounCases b
caseMap f (nom, acc, gen, dat, abl) = (f nom, f acc, f gen, f dat, f abl)

-- Convert a declension's number to a list
caseList :: NounCases a -> [a]
caseList (nom, acc, gen, dat, abl) = [nom, acc, gen, dat, abl]

-- Create a declension by adding endings to a common stem
buildFromStem :: Stem -> Endings -> Declension
buildFromStem stem = declensionMap (stem++)

-- e.g. "mensa_" -> "mensā"
asciiToLongChars :: String -> Maybe String
asciiToLongChars s = (fmap concat . sequence) (f s)
  where f "" = [Just ""]
        f ['_'] = [Nothing]
        f ('_':c:xs) = res : f xs
          where res = case c of
                  'a' -> Just charLongA
                  'e' -> Just charLongE
                  'i' -> Just charLongI
                  'o' -> Just charLongO
                  'u' -> Just charLongU
                  _ -> Nothing

        f (c:xs) = [Just [c]] ++ f xs

-- e.g. "mensā" -> "mensa_"
longCharsToAscii :: String -> String
longCharsToAscii s = concat $ map f s
  where f c
          | [c] == charLongA = asciiLongChar:"a"
          | [c] == charLongE = asciiLongChar:"e"
          | [c] == charLongI = asciiLongChar:"i"
          | [c] == charLongO = asciiLongChar:"o"
          | [c] == charLongU = asciiLongChar:"u"
          | otherwise        = [c]

-- Create string of five lines containing the whole declension
--   in a table format
declensionTable :: Declension -> String
declensionTable (_, sing, plu) = concat lines
  where lines = map (\ (s, p) -> s ++ p ++ "\n") $
                  zip (map (padded singWidth) (caseList sing)) (caseList plu)
        singWidth = longestSing + 2
        longestSing = maximum $ caseList $ caseMap length sing
        padded n s = if l >= n then s else s ++ replicate (n-l) ' '
          where l = length s

-- Output the noun as it might appeaar in a dictionary, including translation
dictionaryEntry :: Declension -> String
dictionaryEntry d@((t, g, c), _, _) = get Sing Nom d ++ ", " ++ get Sing Gen d ++ ", "
                                 ++ c ++ " " ++ genderLetter g ++ ". (" ++ t ++ ")"

dispatch = [("quiz", quiz),
            ("help", help)]


help :: [String] -> IO ()
help args = do
  usage Nothing False
  putStr $ "This program tests your knowledge of pure Latin declensions.\n" ++
           "Currently, it only can quiz you randomly on all declensions.\n\n" ++
           "When entering answers, prepend a vowel with an underscore ('_')" ++
           " to mark it as long.  The system currently requires you to" ++
           " correctly mark all such vowels.\n\n" ++
           "Try: 'declensions quiz' to try it out now!\n\n" ++
           "TODO: --nocolors doesn't work\n"
  
usage :: Maybe String -> Bool -> IO ()
usage error exit = do
  name <- getProgName
  putStr $ f error
  putStr $ "Usage: " ++ name ++ " quiz|help [--nocolors] [--nohints]\n"
  if exit
    then exitFailure
    else return ()
  where f Nothing = ""
        f (Just msg) = msg ++ "\n"


main = do
  args <- getArgs
  if length args < 1
    then usage (Just "No action specified!") True
    else do
      let (command:params) = args
      let coloring = if "--nocolors" `elem` params
                     then False
                     else True
      let action = lookup command dispatch
      if isNothing action
        then usage (Just $ "Invalid action: " ++ command ++ "!") True
        else fromJust action args

  

-- Challenge user on a particular declination
-- Bool: include stem?
ask :: Bool -> Number -> Case -> Declension -> IO Bool
ask s num c d@((t, _, _), _, _) = do
  let nHint = numberHint num
      latinHint = if s then "[" ++ get Sing Nom d ++ "] " else ""
  putStr $ latinHint ++ hint num c t  ++ " " ++ "[q]? "
  hFlush stdout
  raw <- getLine
  
  -- Possibly quit
  if raw == "q"
    then exitSuccess
    else return ()
  
  let response = asciiToLongChars raw
  let answer = get num c d
  if isNothing response
     then do
       putStr $ colorize "red" "Malformed input! Try again.\n"
       ask s num c d
     else if answer == fromJust response
          then return True
          else return False
            
          

quiz :: [String] -> IO ()
quiz args = do
  c <- randomCase
  n <- randomNumber
  d <- randomPureDeclension
  let latinHints = if "--nohints" `elem` args then False else True
  correct <- ask latinHints n c d
  if correct
    then putStr $ colorize "green" "Correct! " ++ get n c d ++ "\n"
    else putStr $ colorize "red" "Wrong!" ++ " Answer: " ++ get n c d ++ "\n"
  quiz args
  

-- Data
firstDeclension = buildFromStem "mens" (("table", Fem, "1 1"),
                                        ("a", "am", "ae", "ae", charLongA),
                                        ("ae", endingLongAS, endingLongARUM, endingLongIS, endingLongIS))

secondDeclension1 = buildFromStem "serv" (("servant", Masc, "2 1"),
                                          ("us", "um", charLongI, charLongO, charLongO),
                                          (charLongI, endingLongOS, endingLongORUM, endingLongIS, endingLongIS))

secondDeclension2 = buildFromStem "verb" (("word", Neut, "2 2"),
                                          ("um", "um", charLongI, charLongO, charLongO),
                                          ("a", "a", endingLongORUM, endingLongIS, endingLongIS))

thirdDeclension1 = buildFromStem "sol" (("sun", Masc, "3 1"),
                                        ("", "em", "is", charLongI, "e"),
                                        (endingLongES, endingLongES, "um", "ibus", "ibus"))

thirdDeclension2 = set Sing Nom "nomen" . set Sing Acc "nomen" $
                   buildFromStem "nomin" (("name", Neut, "3 2"),
                                          ("?", "?", "is", charLongI, "e"),
                                          ("a", "a", "um", "ibus", "ibus"))

fourthDeclension1 = buildFromStem "cas" (("fall", Masc, "4 1"),
                                         ("us", "um", endingLongUS, endingLongUI, charLongU),
                                         (endingLongUS, endingLongUS, "uum", "ibus", "ibus"))

fourthDeclension2 = buildFromStem "corn" (("fall", Masc, "4 2"),
                                          (charLongU, charLongU, endingLongUS, charLongU, charLongU),
                                          ("ua", "ua", "uum", "ibus", "ibus"))

fifthDeclension = buildFromStem "di" (("day", Neut, "5 1"),
                                      (endingLongES, "em", endingLongEI, endingLongEI, charLongE),
                                      (endingLongES, endingLongES, endingLongERUM, endingLongEBUS, endingLongEBUS))

fullPureDeclensions = [firstDeclension,
                       secondDeclension1,
                       secondDeclension2,
                       thirdDeclension1,
                       thirdDeclension2,
                       fourthDeclension1,
                       fourthDeclension2,
                       fifthDeclension]


                                          
