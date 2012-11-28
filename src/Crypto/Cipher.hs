module Crypto.Cipher where

import Data.ByteString
import qualified Data.ByteString as B

class Encipher c where
	encipher :: c -> B.ByteString -> Maybe B.ByteString

class Decipher c where
	decipher :: c -> B.ByteString -> Maybe B.ByteString
