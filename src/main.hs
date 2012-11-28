module Main where
import Crypto.Cipher
import Crypto.Asymmetric.RSA
import Data.Binary
import System.Random

modulus :: PublicKey -> Integer
modulus (PublicKey n e) = n

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
		decrypted = decrypt priv cipher
