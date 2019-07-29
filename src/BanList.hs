module BanList (getBanList, addrIsBanned) where

getBanList :: String -> IO [BL.ByteString]
getBanList path = do
  contents <- readFile path
  pure $ map BL.pack $ lines contents

addrIsBanned :: BL.ByteString -> [BL.ByteString] -> Bool
addrIsBanned = elem