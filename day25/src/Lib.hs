module Lib
    ( someFunc
    ) where

discreteLogarithm :: Integer -> Integer -> Integer -> Integer
discreteLogarithm subjectNum modulus target = (fst . head . (dropWhile (\(_, n) -> n /= target)) . (zip [0..]) . (iterate (\n -> (n * subjectNum) `mod` modulus))) 1

encryptionModulusSize :: Integer
encryptionModulusSize = 20201227

initialSubjectNumber :: Integer
initialSubjectNumber = 7

inputExample :: (Integer, Integer)
inputExample = (5764801, 17807724)

input :: (Integer, Integer)
input = (9232416, 14144084)

someFunc :: IO ()
someFunc = do
  let (firstPublicKey, secondPublicKey) = input
      firstLoopSize = discreteLogarithm initialSubjectNumber encryptionModulusSize firstPublicKey
      encryptionKey = (secondPublicKey ^ firstLoopSize) `mod` encryptionModulusSize
  putStrLn $ "First loop size: " ++ show firstLoopSize
  putStrLn $ "Encryption key being established: " ++ show encryptionKey
