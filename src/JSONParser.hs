{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module JSONParser where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (replicateM)
import Data.Bits (Bits (shiftL))
import Data.Char (chr, digitToInt, isDigit, isHexDigit, ord)
import Data.Functor (($>))
import Data.List (intercalate)
import GHC.Generics (Generic)
import Numeric (showHex)

-- define value types
data JValue
  = JNull
  | JBool Bool
  | JString String
  | JNumber {int :: Integer, frac :: [Int], exponent :: Integer}
  | JArray [JValue]
  | JObject [(String, JValue)]
  deriving (Eq, Generic)

-- define Show for types
instance Show JValue where
  show value = case value of
    JNull -> "null"
    JBool True -> "true"
    JBool False -> "false"
    JString s -> showJSONString s
    JNumber s [] 0 -> show s
    JNumber s f 0 -> show s ++ "." ++ concatMap show f
    JNumber s [] e -> show s ++ "e" ++ show e
    JNumber s f e -> show s ++ "." ++ concatMap show f ++ "e" ++ show e
    JArray a -> "[" ++ intercalate ", " (map show a) ++ "]"
    JObject o -> "{" ++ intercalate ", " (map showKV o) ++ "}"
    where
      showKV (k, v) = showJSONString k ++ ": " ++ show v

showJSONString :: String -> String
showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""

showJSONChar :: Char -> String
showJSONChar c = case c of
  '\'' -> "'"
  '\"' -> "\\\""
  '\\' -> "\\\\"
  '/' -> "\\/"
  '\b' -> "\\b"
  '\f' -> "\\f"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  _ | isControl c -> "\\u" ++ showJSONNonASCIIChar c
  _ -> [c]
  where
    showJSONNonASCIIChar c =
      let a = "0000" ++ showHex (ord c) "" in drop (length a - 4) a

isControl :: Char -> Bool
isControl c = c `elem` ['\0' .. '\31']

-- parser
newtype Parser i o
  = Parser {runParser :: i -> Maybe (i, o)}

char1 :: Char -> Parser String Char
char1 c = Parser $ \case
  (x : xs) | x == c -> Just (xs, x)
  _ -> Nothing

satisfy :: (a -> Bool) -> Parser [a] a
satisfy predicate = Parser $ \case
  (x : xs) | predicate x -> Just (xs, x)
  _ -> Nothing

char :: Char -> Parser String Char
char c = satisfy (== c)

digit1 :: Parser String Int
-- (satisfy isDigit)がParserでrunParserで呼び出し
digit1 = Parser $ \i -> case runParser (satisfy isDigit) i of
  Nothing -> Nothing
  Just (i', o) -> Just (i', digitToInt o)

digit2 :: Parser String Int
digit2 = Parser $ \i -> case runParser (satisfy isDigit) i of
  Nothing -> Nothing
  Just (i', o) -> Just . fmap digitToInt $ (i', o)

digit3 :: Parser String Int
digit3 = Parser $ \i -> fmap (fmap digitToInt) . runParser (satisfy isDigit) $ i

instance Functor (Parser i) where
  fmap f parser = Parser $ fmap (fmap f) . runParser parser

digit :: Parser String Int
digit = digitToInt <$> satisfy isDigit

string1 :: String -> Parser String String
string1 s = case s of
  "" -> Parser $ \i -> Just (i, "")
  (c : cs) -> Parser $ \i -> case runParser (char c) i of
    Nothing -> Nothing
    Just (rest, _) -> case runParser (string1 cs) rest of
      Nothing -> Nothing
      Just (rest', _) -> Just (rest', c : cs)

string2 :: String -> Parser String String
string2 s = case s of
  "" -> Parser $ pure . (,"")
  (c : cs) -> Parser $ \i -> case runParser (char c) i of
    Nothing -> Nothing
    Just (rest, c) -> fmap (c :) <$> runParser (string2 cs) rest

instance Applicative (Parser i) where
  pure x = Parser $ pure . (,x)
  pf <*> po = Parser $ \input -> case runParser pf input of
    Nothing -> Nothing
    Just (rest, f) -> fmap f <$> runParser po rest

string :: String -> Parser String String
string "" = pure ""
string (c : cs) = (:) <$> char c <*> string cs

jNull :: Parser String JValue
jNull = string "null" $> JNull

instance Alternative (Parser i) where
  empty = Parser $ const empty
  p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

jBool :: Parser String JValue
jBool = string "true" $> JBool True <|> string "false" $> JBool False

jsonChar :: Parser String Char
jsonChar =   string "\\\"" $> '"'
         <|> string "\\\\" $> '\\'
         <|> string "\\/"  $> '/'
         <|> string "\\b"  $> '\b'
         <|> string "\\f"  $> '\f'
         <|> string "\\n"  $> '\n'
         <|> string "\\r"  $> '\r'
         <|> string "\\t"  $> '\t'
         <|> unicodeChar
         <|> satisfy (\c -> not (c == '\"' || c == '\\' || isControl c))
  where
    unicodeChar =
      chr . fromIntegral . digitsToNumber 16 0
        <$> (string "\\u" *> replicateM 4 hexDigit)

    hexDigit = digitToInt <$> satisfy isHexDigit

digitsToNumber :: Int -> Integer -> [Int] -> Integer
digitsToNumber base =
  foldl (\num d -> num * fromIntegral base + fromIntegral d)

instance Monad (Parser i) where
  p >>= f = Parser $ \input -> case runParser p input of
    Nothing -> Nothing
    Just (rest, o) -> runParser (f o) rest

jString :: Parser String JValue
jString = JString <$> (char '"' *> jString')                   -- 1
  where
    jString' = do
      optFirst <- optional jsonChar                            -- 2
      case optFirst of
        Nothing -> "" <$ char '"'                              -- 3
        Just first | not (isSurrogate first) ->                -- 4
          (first:) <$> jString'                                -- 5
        Just first -> do                                       -- 6
          second <- jsonChar                                   -- 7
          if isHighSurrogate first && isLowSurrogate second    -- 8
          then (combineSurrogates first second :) <$> jString' -- 9
          else empty

highSurrogateLowerBound, highSurrogateUpperBound :: Int
highSurrogateLowerBound = 0xD800
highSurrogateUpperBound = 0xDBFF

lowSurrogateLowerBound, lowSurrogateUpperBound :: Int
lowSurrogateLowerBound = 0xDC00
lowSurrogateUpperBound = 0xDFFF

isHighSurrogate, isLowSurrogate, isSurrogate :: Char -> Bool
isHighSurrogate a =
  ord a >= highSurrogateLowerBound && ord a <= highSurrogateUpperBound
isLowSurrogate a =
  ord a >= lowSurrogateLowerBound && ord a <= lowSurrogateUpperBound
isSurrogate a = isHighSurrogate a || isLowSurrogate a

combineSurrogates :: Char -> Char -> Char
combineSurrogates a b =
  chr $
    ((ord a - highSurrogateLowerBound) `shiftL` 10)
      + (ord b - lowSurrogateLowerBound)
      + 0x10000

jUInt :: Parser String Integer
jUInt = (\d ds -> digitsToNumber 10 0 (d : ds)) <$> digit19 <*> digits <|> fromIntegral <$> digit

digit19 :: Parser String Int
digit19 = digitToInt <$> satisfy (\x -> isDigit x && x /= '0')

digits :: Parser String [Int]
digits = some digit

jInt' :: Parser String Integer
jInt' = signInt <$> optional (char '-') <*> jUInt

signInt :: Maybe Char -> Integer -> Integer
signInt (Just '-') i = negate i
signInt _ i = i

jFrac :: Parser String [Int]
jFrac = char '.' *> digits

jExp :: Parser String Integer
jExp =
  (char 'e' <|> char 'E')
    *> (signInt <$> optional (char '+' <|> char '-') <*> jUInt)

jInt :: Parser String JValue
jInt = JNumber <$> jInt' <*> pure [] <*> pure 0

jIntExp :: Parser String JValue
jIntExp = JNumber <$> jInt' <*> pure [] <*> jExp

jIntFrac :: Parser String JValue
jIntFrac = (\i f -> JNumber i f 0) <$> jInt' <*> jFrac

jIntFracExp :: Parser String JValue
jIntFracExp = (\ ~(JNumber i f _) e -> JNumber i f e) <$> jIntFrac <*> jExp

-- alternatives
jNumber :: Parser String JValue
jNumber = jIntFracExp <|> jIntExp <|> jIntFrac <|> jInt

surroundedBy ::
  Parser String a -> Parser String b -> Parser String a
surroundedBy p1 p2 = p2 *> p1 <* p2

separatedBy :: Parser i v -> Parser i s -> Parser i [v]
separatedBy v s =
  (:)
    <$> v
    <*> many (s *> v)
      <|> pure []

spaces :: Parser String String
spaces = many (char ' ' <|> char '\n' <|> char '\r' <|> char '\t')

jArray :: Parser String JValue
jArray =
  JArray
    <$> ( char '['
            *> (jValue `separatedBy` char ',' `surroundedBy` spaces)
            <* char ']'
        )

jObject :: Parser String JValue
jObject =
  JObject
    <$> (char '{' *> pair `separatedBy` char ',' `surroundedBy` spaces <* char '}')
  where
    pair =
      (\ ~(JString s) j -> (s, j))
        <$> (jString `surroundedBy` spaces)
        <* char ':'
        <*> jValue

jValue :: Parser String JValue
jValue = jValue' `surroundedBy` spaces
  where
    jValue' =
      jNull
        <|> jBool
        <|> jString
        <|> jNumber
        <|> jArray
        <|> jObject

parseJSON :: String -> Maybe JValue
parseJSON s = case runParser jValue s of
  Just ("", j) -> Just j
  _ -> Nothing
