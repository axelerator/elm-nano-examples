module JsonUtils 
  ( textToByteString
  , toJsonText
  , fromJsonText
  ) where


import qualified Data.Text as T
import Data.Aeson

import Utils

toJsonText :: ToJSON a => a -> T.Text
toJsonText = byteStringToText . encode

fromJsonText :: FromJSON a => T.Text -> Either String a
fromJsonText = eitherDecode . textToByteString

