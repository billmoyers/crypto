module Crypto.Cipher where

class Encipher c where
	encipher :: c -> Integer -> Maybe Integer

class Decipher c where
	decipher :: c -> Integer -> Maybe Integer

