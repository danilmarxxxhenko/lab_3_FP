-- Лабораторна робота №3, варіант 11
-- У кожному реченні тексту видалити підрядок максимальної довжини,
-- що починається і закінчується заданими символами.

module Main (main) where

import Prelude hiding (Word)

import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.Char (isAlphaNum, isSpace)
import Data.List (elemIndex, elemIndices)
import System.Directory (doesFileExist)
import System.IO

------------------------------------------------------------
-- Типи
------------------------------------------------------------

-- символ тексту
newtype Symbol = Symbol Char

-- слово складається з символів
newtype Word = Word [Symbol]

-- розділовий знак
newtype Punctuation = Punctuation Char

-- з чого складається речення: слова, розділові знаки і пробіли
data Element = EWord Word | EPunct Punctuation | ESpace

newtype Sentence = Sentence [Element]

newtype Text = Text [Sentence]

-- перетворення назад у рядок
class Render a where
  render :: a -> String

instance Render Symbol where
  render (Symbol c) = [c]

instance Render Word where
  render (Word cs) = concatMap render cs

instance Render Punctuation where
  render (Punctuation c) = [c]

instance Render Element where
  render (EWord w)  = render w
  render (EPunct p) = render p
  render ESpace     = " "

instance Render Sentence where
  render (Sentence es) = concatMap render es

instance Render Text where
  render (Text ss) = unwords (map render ss)

------------------------------------------------------------
-- Розбір тексту
------------------------------------------------------------

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c `elem` "_'’ʼ"

-- табуляції, переходи на новий рядок і кілька пробілів поспіль
-- стають одним ESpace
parseElements :: String -> [Element]
parseElements [] = []
parseElements s@(c : cs)
  | isSpace c    = ESpace : parseElements (dropWhile isSpace cs)
  | isWordChar c = let (w, rest) = span isWordChar s
                   in EWord (Word (map Symbol w)) : parseElements rest
  | otherwise    = EPunct (Punctuation c) : parseElements cs

isSpaceEl :: Element -> Bool
isSpaceEl ESpace = True
isSpaceEl _      = False

isSentenceEnd :: Element -> Bool
isSentenceEnd (EPunct (Punctuation c)) = c `elem` ".!?…"
isSentenceEnd _                        = False

trim :: [Element] -> [Element]
trim = reverse . dropWhile isSpaceEl . reverse . dropWhile isSpaceEl

-- речення закінчується на . ! ? (або кілька поспіль), після яких
-- іде пробіл або кінець тексту; крапка всередині "Data.List"
-- чи "3.14" речення не закінчує
takeSentence :: [Element] -> ([Element], [Element])
takeSentence [] = ([], [])
takeSentence (e : es)
  | isSentenceEnd e =
      let (ends, rest) = span isSentenceEnd es
      in case rest of
           []           -> (e : ends, [])
           (ESpace : _) -> (e : ends, rest)
           _            -> let (s, r) = takeSentence rest
                           in (e : ends ++ s, r)
  | otherwise = let (s, r) = takeSentence es in (e : s, r)

splitSentences :: [Element] -> [Sentence]
splitSentences els = case dropWhile isSpaceEl els of
  [] -> []
  xs -> let (s, rest) = takeSentence xs
        in Sentence (trim s) : splitSentences rest

parseText :: String -> Text
parseText = Text . splitSentences . parseElements . filter (/= '\xFEFF')

------------------------------------------------------------
-- Завдання
------------------------------------------------------------

-- найдовший підрядок від символу a до символу b:
-- перше входження a і останнє входження b після нього
longestSpan :: Char -> Char -> String -> Maybe (Int, Int)
longestSpan a b s = do
  i <- elemIndex a s
  let js = filter (> i) (elemIndices b s)
  if null js then Nothing else Just (i, last js)

-- після видалення може лишитися пробіл перед розділовим знаком,
-- прибираємо його
fixSpaces :: [Element] -> [Element]
fixSpaces (ESpace : p@(EPunct (Punctuation c)) : rest)
  | c `elem` ".,;:!?)…" = fixSpaces (p : rest)
fixSpaces (e : rest) = e : fixSpaces rest
fixSpaces [] = []

-- повертає нове речення і видалений підрядок (якщо щось видалили)
processSentence :: Char -> Char -> Sentence -> (Sentence, Maybe String)
processSentence a b sent =
  case longestSpan a b str of
    Nothing     -> (sent, Nothing)
    Just (i, j) ->
      let removed = take (j - i + 1) (drop i str)
          left    = take i str ++ drop (j + 1) str
          new     = trim (fixSpaces (parseElements left))
      in (Sentence new, Just removed)
  where
    str = render sent

isEmptySentence :: Sentence -> Bool
isEmptySentence (Sentence es) = null es

------------------------------------------------------------
-- Введення / виведення
------------------------------------------------------------

ask :: String -> IO String
ask msg = do
  putStr (msg ++ ": ")
  hFlush stdout
  getLine

askChar :: String -> IO Char
askChar msg = do
  s <- ask msg
  case s of
    (c : _) -> return c
    []      -> putStrLn "Потрібно ввести символ." >> askChar msg

readUtf8 :: FilePath -> IO String
readUtf8 path = withFile path ReadMode $ \h -> do
  hSetEncoding h utf8
  s <- hGetContents h
  _ <- evaluate (length s)
  return s

writeUtf8 :: FilePath -> String -> IO ()
writeUtf8 path s = withFile path WriteMode $ \h -> do
  hSetEncoding h utf8
  hPutStrLn h s

askFile :: IO FilePath
askFile = do
  name <- ask "Файл з текстом (Enter - textbook.txt)"
  let file = if null name then "textbook.txt" else name
  ok <- doesFileExist file
  if ok then return file else putStrLn "Файл не знайдено." >> askFile

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  hSetBuffering stdout NoBuffering

  file <- askFile
  Text sentences <- parseText <$> readUtf8 file
  putStrLn ("Речень у тексті: " ++ show (length sentences))

  a <- askChar "Початковий символ"
  b <- askChar "Кінцевий символ"

  let results = map (processSentence a b) sentences
      changed = length [() | (_, Just _) <- results]

  forM_ (zip3 [1 :: Int ..] sentences results) $
    \(k, old, (new, removed)) -> case removed of
      Nothing  -> putStrLn (show k ++ ". (без змін) " ++ render old)
      Just sub -> do
        putStrLn (show k ++ ". " ++ render old)
        putStrLn ("   видалено: " ++ sub)
        putStrLn ("   стало:    " ++ render new)

  putStrLn ("\nЗмінено речень: " ++ show changed
            ++ " з " ++ show (length sentences))
  let result = Text [s | (s, _) <- results, not (isEmptySentence s)]
  unless (changed == 0) $ putStrLn ("\nНовий текст:\n" ++ render result)
  writeUtf8 "result.txt" (render result)
  putStrLn "Результат записано у файл result.txt"
