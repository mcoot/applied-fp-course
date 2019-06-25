{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString as BS

import           Data.Text                  (Text, pack)

import qualified Data.Bifunctor as B
import           Data.Monoid                (Last (Last))
import           Control.Monad.IO.Class             (liftIO)

import           Control.Exception          (try)

import qualified Data.Attoparsec.ByteString as AB

import           Waargonaut                 (Json)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))
import Waargonaut.Decode.Runners (decodeFromByteString)

import           Level06.AppM               (AppM, liftEither)
import           Level06.Types              (ConfigError (BadConfFile, ConfigIOError),
                                             PartialConf (PartialConf), partialConfDecoder)
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> readConfFile "badFileName.no"
-- Left (undefined "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile fp = do
  c <- liftIO $ try $ BS.readFile fp
  liftEither $ B.first ConfigIOError c

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile fp = do
  c <- readConfFile fp
  decoded <- decodeFromByteString AB.parseOnly partialConfDecoder c
  liftEither $ B.first (BadConfFile . fst) decoded

-- Go to 'src/Level06/Conf.hs' next.
