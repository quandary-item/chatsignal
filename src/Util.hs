{-# LANGUAGE FlexibleContexts #-}

module Util ( assert
            , assertM
            , dupe
            ) where

import Control.Monad.Except (liftEither, MonadError)


assert :: String -> Bool -> Either String ()
assert _       True  = Right ()
assert message False = Left message

assertM :: (MonadError String m) => String -> Bool -> m ()
assertM message = liftEither . (assert message)

dupe :: a -> (a, a)
dupe a = (a, a)
