module NetUtils (getHostAddress, hostAddressToString) where

import Data.List
import Data.Word (Word8)
import Network.Socket


getHostAddress :: SockAddr -> Maybe HostAddress
getHostAddress sockAddr =
  case sockAddr of
    SockAddrInet _ ipv4Addr -> Just ipv4Addr
    _                       -> Nothing


tupleToIPv4 :: (Word8, Word8, Word8, Word8) -> String
tupleToIPv4 (a, b, c, d) = intercalate "." $ map show [a,b,c,d]


hostAddressToString :: HostAddress -> String
hostAddressToString = tupleToIPv4 . hostAddressToTuple
