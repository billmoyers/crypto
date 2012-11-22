module Crypto.PubKey.RSA where
import Numeric
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- Types
data PublicKey = PublicKey Integer Integer

showPublicKey :: PublicKey -> String
showPublicKey (PublicKey n e) = "PublicKey(n=" ++ (show n) ++ ", e=" ++ (show e) ++ ")"

instance Show PublicKey where show = showPublicKey

data PrivateKey = PrivateKey Integer Integer

showPrivateKey :: PrivateKey -> String
showPrivateKey (PrivateKey n d) = "PrivateKey(n=" ++ (show n) ++ ", d=" ++ (show d) ++ ")"

instance Show PrivateKey where show = showPrivateKey

-- Primality test.
prime :: Integer -> Bool
prime a = True

-- Generator for all primes in the given range.
primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b = filter prime [a..b]

-- Using Extended Euclidean Algorithm, returning the multiplicative inverses
moduloMultiplicativeInverse :: Integer -> Integer -> (Integer, Integer)
moduloMultiplicativeInverse a b = 
	case b of
		0 -> (1, 0)
		_ -> let 
				q = a `quot` b
				r = a `rem` b
				(s, t) = moduloMultiplicativeInverse b r
			in (t, s - (q * t))

-- Generate a key pair.
totient :: Integer -> Integer -> Integer
totient p q = (p - 1) * (q - 1)

generateKeyPair :: (PublicKey, PrivateKey)
generateKeyPair = (PublicKey n e, PrivateKey n d)
	where
		p = 61
		q = 53
		n = p * q
		phi = totient p q
		coprimes = (primesInRange 17 phi)
		e = (take 1 coprimes) !! 0
		mmi = fst $ moduloMultiplicativeInverse e phi
		d = if mmi < 0 then phi + mmi else mmi

encrypt :: PublicKey -> Integer -> Integer
encrypt (PublicKey n e) c = (c ^ e) `mod` n

decrypt :: PrivateKey -> Integer -> Integer
decrypt (PrivateKey n d) c = (c ^ d) `mod` n

prettyPrint :: B.ByteString -> String
prettyPrint = concat . map (flip showHex "") . B.unpack

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

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
