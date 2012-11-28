module Crypto.Cipher where

class Encipher c where
	encipher :: c -> Integer -> Integer

class Decipher c where
	decipher :: c -> Integer -> Integer

