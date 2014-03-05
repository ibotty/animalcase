-- This Module converts between camelCase and snake_case.
--
-- The suffixes specify the base type the functions are working on.
-- L uses lazy 'TL.Text', S uses 'String' and B8 uses 'B8.ByteString'.
--
-- Beware, that you should not use 'B8.ByteString's for serious work, as
-- they only work with 8 bit characters. Do only use when you are
-- absolutely sure, there is no wide character in the string!
{-# LANGUAGE OverloadedStrings #-}
module Text.AnimalCase
  ( toCamelCase
  , toSnakeCase
  , toCamelCaseS
  , toSnakeCaseS
  , toCamelCaseL
  , toSnakeCaseL
  , toCamelCaseB8
  , toSnakeCaseB8
  ) where

import Control.Arrow (first)
import Data.Char (isUpper)
import Data.Monoid ((<>))
import Data.Text.Lazy.Encoding (decodeLatin1, encodeUtf8)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

useLazy :: (TL.Text -> TL.Text) -> T.Text -> T.Text
useLazy f = TL.toStrict .  f . TL.fromStrict

useLazyS :: (TL.Text -> TL.Text) -> String -> String
useLazyS f = TL.unpack . f . TL.pack

useLazyB8 :: (TL.Text -> TL.Text) -> B8.ByteString -> B8.ByteString
useLazyB8 f = BL.toStrict . encodeUtf8 . f . decodeLatin1 . BL.fromStrict

onFirstLetter :: (Char -> TL.Text) -> TL.Text -> TL.Text
onFirstLetter f = maybe "" (uncurry (<>) . first f) . TL.uncons

toCamelCase :: T.Text -> T.Text
toCamelCase = useLazy toCamelCaseL

toCamelCaseL :: TL.Text -> TL.Text
toCamelCaseL = TL.concat . map capitalizeWord . TL.splitOn "_"
  where capitalizeWord = onFirstLetter (TL.toUpper . TL.singleton)

toCamelCaseS :: String -> String
toCamelCaseS = useLazyS toCamelCaseL

toCamelCaseB8 :: B8.ByteString -> B8.ByteString
toCamelCaseB8 = useLazyB8 toCamelCaseL

toSnakeCase :: T.Text -> T.Text
toSnakeCase = useLazy toSnakeCaseL

toSnakeCaseL :: TL.Text -> TL.Text
toSnakeCaseL = TL.concatMap f
  where f c
          | isUpper c = "_" <> TL.toLower (TL.singleton c)
          | otherwise = TL.singleton c

toSnakeCaseS :: String -> String
toSnakeCaseS = useLazyS toSnakeCaseL

toSnakeCaseB8 :: B8.ByteString -> B8.ByteString
toSnakeCaseB8 = useLazyB8 toSnakeCaseL
