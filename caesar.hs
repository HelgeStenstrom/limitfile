import Data.Char
import Test.QuickCheck

shouldCipher :: Char -> Bool
shouldCipher c = isLetter c && isAscii c

cipherchar :: Int -> Char -> Char
cipherchar n c 
    | shouldCipher c = chr(ord(c) + n)
    | otherwise = c


decipherchar :: Int -> Char -> Char
decipherchar n c  = bettercipherchar (-n) c

cipher n s = map (bettercipherchar n ) s
decipher n s = map (decipherchar n ) s

mytest = quickCheck ((\n -> (\s -> ((decipher n (cipher n s)) == s ))) :: Int -> String -> Bool)
vtest = verboseCheck ((\n -> (\s -> ((decipher n (cipher n s)) == s ))) :: Int -> String -> Bool)


wraparound shift c 
-- should we wrap around the alphabet, if we shift past Z?
 | isLower(c) && ord(c)+shift > ord 'z' = True
 | isUpper(c) && ord(c)+shift > ord 'Z' = True
 | otherwise = False


bettercipherchar :: Int -> Char -> Char
-- implementation of character substitution with wrapping
bettercipherchar shift c
 | shouldCipher c =  chr(ord(c) + adjustedshift)
 | otherwise      = c
 where adjustedshift = let shift' = shift `mod` 26
                       in if (wraparound shift' c)
                          then shift'-26
                          else shift'
