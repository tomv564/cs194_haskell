{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret file1 file2 = do
    contents1 <- BS.readFile file1
    contents2 <- BS.readFile file2
    return $ BS.pack $ filter (> 0) $ BS.zipWith xor contents1 contents2

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key filePath = do
    contents <- BS.readFile (filePath ++ ".enc")
    BS.writeFile filePath $ BS.pack $ BS.zipWith xor contents (BS.cycle key)
    return ()

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile filePath = do
    contents <- BS.readFile filePath
    return $ decode contents

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimFile transactionFile = do
    victims <- parseFile victimFile
    transactions <- parseFile transactionFile
    case victims of
      Nothing -> return Nothing
      Just vics ->
        case transactions of
          Nothing -> return Nothing
          Just trans -> return (Just (filter (\t -> tid t `elem` vics) trans))

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = foldr recordFlow Map.empty
  where
  recordFlow t = credit t . debit t
  credit t = Map.insertWith (+) (to t) (amount t)
  debit t = Map.insertWith (-) (from t) (amount t)

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal trans = fst $ Map.foldrWithKey largest ("", 0) trans
  where
  largest name1 bal1 (name2, bal2)
    | bal2 > bal1 = (name2, bal2)
    | otherwise   = (name1, bal1)

    -- maybeTrans <- getBadTs "clues/victims.json" "clues/transactions.json"
    -- let trans = maybe [] id maybeTrans


-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

