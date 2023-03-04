main :: IO ()
main = do
  print "Hello World"
  print $ toDigitsRev 1234
  print $ toDigits 1234
  print $ doubleEveryOther [8,7,6,5]
  print $ doubleEveryOther [1,2,3]
  print $ sumDigits [16,7,12,5]
  print $ checksum 4012888888881881
  print $ checksum 4012888888881882
  print $ validate 4012888888881881
  print $ validate 4012888888881882


toDigits :: Integer -> [Integer]
toDigits xs = reverse (toDigitsRev xs)

-- Recursive!
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = mod n 10 : toDigitsRev (div n 10)

-- note: we can remove the 2nd reverse by passing in toDigitsRev directly.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs = reverse ( zipWith (*) (reverse xs) (cycle [1,2]) )

-- note that we need toDigits here!
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = sum (concatMap toDigits xs)

checksum :: Integer -> Integer
checksum x =
    let digitSum = sumDigits (doubleEveryOther (toDigits x) )
    in mod digitSum 10

validate :: Integer -> Bool
validate x = checksum x == 0




