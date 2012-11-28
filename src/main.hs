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

main = do
	putStrLn $ show pub
	putStrLn $ show priv
	putStrLn $ "plaintext=" ++ (show plain)
	putStrLn $ "ciphertext=" ++ (show cipher)
	putStrLn $ "decrypted=" ++ (show decrypted)
	where
		stdGen = mkStdGen 0
		(pub, priv) = generateKeyPair stdGen 1024
		plain = toStrict $ encode (23852 :: Integer)
		cipher = encipher pub plain
		decrypted = case cipher of
			Nothing -> Nothing
			Just n -> decipher priv n
