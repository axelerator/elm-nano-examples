module Utils
  ( shuffle
  , withoutNth
  , replaceNth
  , rootLogger
  , Utils.infoM
  , Utils.errorM
  , textToByteString
  , byteStringToText
  , byteStringToString
  , textToString
  ) where

-- for list suffle
import System.Random (randomRIO)
import Data.Array.IO (IOArray, readArray, writeArray, newListArray)
import Control.Monad (forM)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as DTL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8
import System.Log.Logger as L

-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

withoutNth :: [a] -> Int -> (a, [a])
withoutNth es i =
  let
    (h, t) = splitAt i es
    c = head t
  in
    (c, h ++ (drop 1 t))

replaceNth :: [a] -> Int -> a -> [a]
replaceNth es i e =
  let
    (h, t) = splitAt i es
  in
    h ++ (e:(drop 1 t))

rootLogger = "GameServer"

infoM = L.infoM rootLogger
errorM = L.errorM rootLogger

byteStringToText :: BSL.ByteString -> T.Text
byteStringToText  = T.pack . C8.unpack . BSL.toStrict

textToByteString :: T.Text -> BSL.ByteString
textToByteString t = Data.ByteString.Lazy.Char8.pack (T.unpack t)

byteStringToString :: BSL.ByteString -> String
byteStringToString  = C8.unpack . BSL.toStrict

textToString :: T.Text -> String
textToString = T.unpack

