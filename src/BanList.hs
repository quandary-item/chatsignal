module BanList (getBanList, addrIsBanned) where

getBanList :: String -> IO [String]
getBanList path = do
  contents <- readFile path
  pure $ lines contents

addrIsBanned :: String -> [String] -> Bool
addrIsBanned = elem
