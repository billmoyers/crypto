module Main where
import Numeric
import System.Random

import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Crypto.Cipher
import Crypto.Asymmetric.RSA

modulus :: PublicKey -> Integer
modulus (PublicKey n e) = n

prettyPrint :: B.ByteString -> String
prettyPrint = concat . map (flip showHex "") . B.unpack

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

main = do
	putStrLn $ show pub
	putStrLn $ show priv
	putStrLn $ "plaintext=" ++ (show plain)
	putStrLn $ "ciphertext=" ++ (show cipher)
	putStrLn $ "decrypted=" ++ (show decrypted)
	where
		(pub, priv) = generateKeyPair (mkStdGen 0) 1024
		plain = 71
		cipher = encrypt pub plain
		decrypted = case cipher of
			Nothing -> Nothing
			Just n -> decrypt priv n
