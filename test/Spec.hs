import Data.Char (isSpace)
import Data.List (intercalate)
import JSONParser
import Test.QuickCheck

main :: IO ()
main = putStrLn "Test suite not yet implemented"

jNullGen :: Gen JValue
jNullGen = pure JNull

jBoolGen :: Gen JValue
jBoolGen = JBool <$> arbitrary

jNumberGen :: Gen JValue
jNumberGen = JNumber <$> arbitrary <*> listOf (choose (0, 9)) <*> arbitrary

jsonStringGen :: Gen String
jsonStringGen =
  concat
    <$> listOf
      ( oneof
          [ vectorOf 1 arbitraryUnicodeChar,
            escapedUnicodeChar
          ]
      )
  where
    escapedUnicodeChar = ("\\u" ++) <$> vectorOf 4 (elements hexDigitLetters)
    hexDigitLetters = ['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']

jStringGen :: Gen JValue
jStringGen = JString <$> jsonStringGen

jArrayGen :: Int -> Gen JValue
jArrayGen = fmap JArray . scale (`div` 2) . listOf . jValueGen . (`div` 2)

jObjectGen :: Int -> Gen JValue
jObjectGen = fmap JObject . scale (`div` 2) . listOf . objKV . (`div` 2)
  where
    objKV n = (,) <$> jsonStringGen <*> jValueGen n

jValueGen :: Int -> Gen JValue
jValueGen n =
  if n < 5
    then frequency [(4, oneof scalarGens), (1, oneof (compositeGens n))]
    else frequency [(1, oneof scalarGens), (4, oneof (compositeGens n))]
  where
    scalarGens = [jNullGen, jBoolGen, jNumberGen, jStringGen]
    compositeGens n = [jArrayGen n, jObjectGen n]

instance Arbitrary JValue where
  arbitrary = sized jValueGen
  shrink = genericShrink

jsonWhitespaceGen :: Gen String
jsonWhitespaceGen =
  scale (round . sqrt . fromIntegral)
    . listOf
    . elements
    $ [' ', '\n', '\r', '\t']

stringify :: JValue -> Gen String
stringify = pad . go
  where
    surround l r j = l ++ j ++ r
    pad gen = surround <$> jsonWhitespaceGen <*> jsonWhitespaceGen <*> gen
    commaSeparated = pad . pure . intercalate ","

    go value = case value of
      JArray elements ->
        mapM (pad . stringify) elements
          >>= fmap (surround "[" "]") . commaSeparated
      JObject kvs ->
        mapM stringifyKV kvs >>= fmap (surround "{" "}") . commaSeparated
      _ -> return $ show value

    stringifyKV (k, v) =
      surround <$> pad (pure $ showJSONString k) <*> stringify v <*> pure ":"

prop_genParseJString :: Property
prop_genParseJString =
  forAllShrink jStringGen shrink $ \js ->
    case runParser jString (show js) of
      Nothing -> False
      Just (_, o) -> o == js

prop_genParseJNumber :: Property
prop_genParseJNumber =
  forAllShrink jNumberGen shrink $ \jn ->
    case runParser jNumber (show jn) of
      Nothing -> False
      Just (_, o) -> o == jn

prop_genParseJArray :: Property
prop_genParseJArray =
  forAllShrink (sized jArrayGen) shrink $ \ja -> do
    jas <- dropWhile isSpace <$> stringify ja
    return . counterexample (show jas) $ case runParser jArray jas of
      Nothing -> False
      Just (_, o) -> o == ja

prop_genParseJObject :: Property
prop_genParseJObject =
  forAllShrink (sized jObjectGen) shrink $ \jo -> do
    jos <- dropWhile isSpace <$> stringify jo
    return . counterexample (show jos) $ case runParser jObject jos of
      Nothing -> False
      Just (_, o) -> o == jo

prop_genParseJSON :: Property
prop_genParseJSON = forAllShrink (sized jValueGen) shrink $ \value -> do
  json <- stringify value
  return . counterexample (show json) . (== Just value) . parseJSON $ json

runTests :: IO ()
runTests = do
  putStrLn "== prop_genParseJString =="
  quickCheck prop_genParseJString

  putStrLn "== prop_genParseJNumber =="
  quickCheck prop_genParseJNumber

  putStrLn "== prop_genParseJArray =="
  quickCheck prop_genParseJArray

  putStrLn "== prop_genParseJObject =="
  quickCheck prop_genParseJObject

  putStrLn "== prop_genParseJSON =="
  quickCheck prop_genParseJSON
