module Main where
import Crypto.Cipher
import Crypto.Asymmetric.RSA
import Data.Binary

main = do
	putStrLn $ show pub
	putStrLn $ show priv
	putStrLn $ show plain
	putStrLn $ show cipher
	putStrLn $ show decrypted
	putStrLn $ show $ prettyPrint $ toStrict (encode decrypted)
	where
		(pub, priv) = generateKeyPair
		plain = 65
		cipher = encrypt pub plain
		decrypted = decrypt priv cipher
