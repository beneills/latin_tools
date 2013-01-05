-- Imports
import Data.Maybe (isNothing, fromJust)
import System.Random

-- Datatypes
type Stem = String
type Translation = String
data Gender = Masc | Fem | Neut
type Declension = (Translation, Gender, Cases String, Cases String)
type Endings = Declension
type Cases a = (a, a, a, a, a) -- :: (Nom, Acc, Gen, Dat, Abl)
data Case = Nom | Acc | Gen | Dat | Abl
data Number = Sing | Plu

-- Constants/Parameters
asciiLongChar = '_'

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

genderLetter :: Gender -> String
genderLetter Masc = "M"
genderLetter Fem = "F"
genderLetter Neut = "N"

hint :: Case -> (String -> String)
hint Nom = ("The "++).(++"... ")
hint Acc = ("... the "++)
hint Gen = ("of the "++)
hint Dat = ("by the "++)
hint Abl = ("on the "++)

numberHint :: Number -> String
numberHint Sing = "(sing.)"
numberHint Plu = "(plu.)"

-- Utility Functions

-- Random selection of declension
randomCase = randomChoice [Nom, Acc, Gen, Dat, Abl]
randomNumber = randomChoice [Sing, Plu]
randomPureDeclension = randomChoice fullPureDeclensions
randomChoice xs = fmap (xs!!) $ randomRIO (0, length xs - 1)

-- Decline a noun!
get :: Number -> Case -> Declension -> String
get Sing  c (_, _, s, _) = get' c s
get Plu c (_, _, _, p) = get' c p

get' Nom (x, _, _, _, _) = x
get' Acc (_, x, _, _, _) = x
get' Gen (_, _, x, _, _) = x
get' Dat (_, _, _, x, _) = x
get' Abl (_, _, _, _, x) = x

-- Alter a declension
set :: Number -> Case -> String -> Declension -> Declension
set Sing  c x (t, g, s, p) = (t, g, set' c x s, p)
set Plu c x (t, g, s, p) = (t, g, s, set' c x p)

set' Nom x (nom, acc, gen, dat, abl) = (x, acc, gen, dat, abl)
set' Acc x (nom, acc, gen, dat, abl) = (nom, x, gen, dat, abl)
set' Gen x (nom, acc, gen, dat, abl) = (nom, acc, x, dat, abl)
set' Dat x (nom, acc, gen, dat, abl) = (nom, acc, gen, x, abl)
set' Abl x (nom, acc, gen, dat, abl) = (nom, acc, gen, dat, x)


-- Map over declension
declensionMap :: (String -> String)
                 -> Declension
                 -> Declension
declensionMap f (t, g, sing, plu) = (t, g, caseMap f sing, caseMap f plu)

-- Map over a declension's number
caseMap :: (a -> b)
           -> Cases a
           -> Cases b
caseMap f (nom, acc, gen, dat, abl) = (f nom, f acc, f gen, f dat, f abl)

-- Convert a declension's number to a list
caseList :: Cases a -> [a]
caseList (nom, acc, gen, dat, abl) = [nom, acc, gen, dat, abl]

-- Create a declension by adding endings to a common stem
buildFromStem :: Stem -> Endings -> Declension
buildFromStem stem = declensionMap (stem++)

-- e.g. "mensa_" -> "mensā"
asciiToLongChars :: String -> Maybe String
asciiToLongChars s = (fmap concat . sequence) (f s)
  where f "" = [Just ""]
        f ['_'] = [Nothing]
        f (c:'_':xs) = res : f xs
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
          | [c] == charLongA = "a" ++ [asciiLongChar]
          | [c] == charLongE = "e" ++ [asciiLongChar]
          | [c] == charLongI = "i" ++ [asciiLongChar]
          | [c] == charLongO = "o" ++ [asciiLongChar]
          | [c] == charLongU = "u" ++ [asciiLongChar]
          | otherwise        = [c]

-- Create string of five lines containing the whole declension
--   in a table format
declensionTable :: Declension -> String
declensionTable (_, _, sing, plu) = concat lines
  where lines = map (\ (s, p) -> s ++ p ++ "\n") $
                  zip (map (padded singWidth) (caseList sing)) (caseList plu)
        singWidth = longestSing + 2
        longestSing = maximum $ caseList $ caseMap length sing
        padded n s = if l >= n then s else s ++ replicate (n-l) ' '
          where l = length s

-- Output the noun as it might appeaar in a dictionary, including translation
dictionaryEntry :: Declension -> String
dictionaryEntry d@(t, g, _, _) = get Sing Nom d ++ ", " ++ get Sing Gen d ++ ", "
                                 ++ genderLetter g ++ ". (" ++ t ++ ")"

-- Challenge user on a particular declination
ask :: Number -> Case -> Declension -> IO ()
ask num c d@(t, _, _, _) = do
  let nHint = numberHint num
  putStr $ hint c t  ++ " " ++ nHint ++ "? "
  raw <- getLine
  let response = asciiToLongChars raw
  let answer = get num c d
  if isNothing response
     then putStr "Malformed input!\n"
     else if answer == fromJust response
             then putStr "Correct!\n"
             else putStr $ "Wrong! Response: " ++ fromJust response ++ ", Answer: " ++ answer ++ "\n"
  
quiz :: IO ()
quiz = do
  c <- randomCase
  n <- randomNumber
  d <- randomPureDeclension
  ask n c d
  quiz


-- Data
firstDeclension = buildFromStem "mens" ("table",
                                        Fem,
                                        ("a", "am", "ae", "ae", charLongA),
                                        ("ae", endingLongAS, endingLongARUM, endingLongIS, endingLongIS))

secondDeclension1 = buildFromStem "serv" ("servant",
                                          Masc,
                                          ("us", "um", charLongI, charLongO, charLongO),
                                          (charLongI, endingLongOS, endingLongORUM, endingLongIS, endingLongIS))

secondDeclension2 = buildFromStem "verb" ("word",
                                          Neut,
                                          ("um", "um", charLongI, charLongO, charLongO),
                                          ("a", "a", endingLongORUM, endingLongIS, endingLongIS))

thirdDeclension1 = buildFromStem "sol" ("word",
                                        Masc,
                                        ("", "em", "is", charLongI, "e"),
                                        (endingLongES, endingLongES, "um", "ibus", "ibus"))

thirdDeclension2 = set Sing Nom "nomen" . set Sing Acc "nomen" $
                   buildFromStem "nomin" ("name",
                                          Neut,
                                          ("?", "?", "is", charLongI, "e"),
                                          ("a", "a", "um", "ibus", "ibus"))

fourthDeclension1 = buildFromStem "cas" ("fall",
                                         Masc,
                                         ("us", "um", endingLongUS, endingLongUI, charLongU),
                                         (endingLongUS, endingLongUS, "uum", "ibus", "ibus"))

fourthDeclension2 = buildFromStem "corn" ("fall",
                                          Masc,
                                          (charLongU, charLongU, endingLongUS, charLongU, charLongU),
                                          ("ua", "ua", "uum", "ibus", "ibus"))

fifthDeclension = buildFromStem "di" ("day",
                                      Neut,
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


                                          
