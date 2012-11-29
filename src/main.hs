module Main where
import Numeric
import System.Random

import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64

import Crypto.Cipher
import Crypto.Asymmetric.RSA

modulus :: PublicKey -> Integer
modulus (PublicKey n e) = n

prettyPrint :: B.ByteString -> String
prettyPrint = concat . map (flip showHex "") . B.unpack

showBytes :: Maybe B.ByteString -> String
showBytes Nothing = "Nothing"
showBytes (Just a) = show $ B64.encode a

main = do
	putStrLn $ show pub
	putStrLn $ show priv
	putStrLn $ "plaintext=" ++ (showBytes $ Just plain)
	putStrLn $ "ciphertext=" ++ (showBytes cipher)
	putStrLn $ "decrypted=" ++ (showBytes decrypted)
	where
		stdGen = mkStdGen 0
		(pub, priv) = generateKeyPair stdGen 2048
		plain = toStrict $ encode (23852 :: Integer)
		cipher = encipher pub plain
		decrypted = case cipher of
			Nothing -> Nothing
			Just n -> decipher priv n
