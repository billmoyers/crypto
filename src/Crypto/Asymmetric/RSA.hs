module Crypto.Asymmetric.RSA where

import Numeric
import Math.NumberTheory.Primes.Testing
import System.Random
import Data.Bits
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Crypto.Cipher

-- Types
data PublicKey = PublicKey Integer Integer

showPublicKey :: PublicKey -> String
showPublicKey (PublicKey n e) = "PublicKey(n=" ++ (show n) ++ ", e=" ++ (show e) ++ ")"

instance Show PublicKey where show = showPublicKey
instance Encipher PublicKey where encipher = encipher'

data PrivateKey = PrivateKey Integer Integer

showPrivateKey :: PrivateKey -> String
showPrivateKey (PrivateKey n d) = "PrivateKey(n=" ++ (show n) ++ ", d=" ++ (show d) ++ ")"

instance Show PrivateKey where show = showPrivateKey
instance Decipher PrivateKey where decipher = decipher' 

-- Generator for all primes in the given range.
primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b = filter isPrime [a..b]

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

generatePrimes :: StdGen -> Int -> (Integer, Integer)
generatePrimes stdGen bits = (p, q)
	where
		bitsp = (bits + 1) `quot` 2
		bitsq = bits - bitsp
		m = shiftL 1 (bits - 1)
		candidates = map (\x -> ((x .|. 1) .|. m)) (randomRs (3, m) stdGen)
		primes = filter isPrime candidates
		p = primes !! 0
		q = primes !! 1

generateKeyPair :: StdGen -> Int -> (PublicKey, PrivateKey)
generateKeyPair stdGen bits = 
	let
		(p, q) = generatePrimes stdGen bits
		n = p * q
		phi = totient p q
		coprimes = (primesInRange 17 phi)
		e = (take 1 coprimes) !! 0
		mmi = fst $ moduloMultiplicativeInverse e phi
		d = if mmi < 0 then phi + mmi else mmi
	in (PublicKey n e, PrivateKey n d)

modexp :: forall a. Integral a => a -> a -> a -> a
modexp b e n = modexp' 1 b e
    where
    modexp' :: a -> a -> a -> a
    modexp' p _ 0 = p
    modexp' p x e =
        if even e
          then modexp' p (mod (x*x) n) (div e 2)
          else modexp' (mod (p*x) n) x (pred e)

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

strictEncode i = toStrict $ encode i

encipher' :: PublicKey -> B.ByteString -> Maybe B.ByteString
encipher' (PublicKey n e) b = if ((c < 0) || (c >= n)) then Nothing else Just $ strictEncode $ modexp c e n
	where
		c = decode $ BL.fromChunks [b]

decipher' :: PrivateKey -> B.ByteString -> Maybe B.ByteString
decipher' (PrivateKey n d) b = if ((c < 0) || (c >= n)) then Nothing else Just $ strictEncode $ modexp c d n
	where
		c = decode $ BL.fromChunks [b]

