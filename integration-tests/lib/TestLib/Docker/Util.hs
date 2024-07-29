{-# LANGUAGE ViewPatterns #-}

module TestLib.Docker.Util where

import Control.Monad.IO.Unlift
import Data.Text as T
import Network.HTTP.Client as NH
import Network.HTTP.Types.Status as HTTP
import Test.Sandwich
import UnliftIO.Exception


-- * HTTP

is2xx :: NH.Response a -> Bool
is2xx (responseStatus -> (HTTP.Status code _)) = code >= 200 && code < 300

-- is304 :: NH.Response a -> Bool
-- is304 (responseStatus -> (HTTP.Status code _)) = code == 304

-- is400 :: NH.Response a -> Bool
-- is400 (responseStatus -> (HTTP.Status code _)) = code == 400

is403 :: NH.Response a -> Bool
is403 (responseStatus -> (HTTP.Status code _)) = code == 403

is404 :: NH.Response a -> Bool
is404 (responseStatus -> (HTTP.Status code _)) = code == 404

-- is409 :: NH.Response a -> Bool
-- is409 (responseStatus -> (HTTP.Status code _)) = code == 409

is5xx :: NH.Response a -> Bool
is5xx (responseStatus -> (HTTP.Status code _)) = code >= 500 && code < 600

-- * Util

leftOnException :: (MonadUnliftIO m) => m (Either Text a) -> m (Either Text a)
leftOnException = handleAny $ \e -> return $ Left $ T.pack $ case fromException e of
  Just (Reason _ msg) -> msg
  _ -> show e
