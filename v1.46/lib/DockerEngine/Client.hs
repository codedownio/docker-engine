{-
   Docker Engine API

   The Engine API is an HTTP API served by Docker Engine. It is the API the Docker client uses to communicate with the Engine, so everything the Docker client can do can be done with the API.  Most of the client's commands map directly to API endpoints (e.g. `docker ps` is `GET /containers/json`). The notable exception is running containers, which consists of several API calls.  # Errors  The API uses standard HTTP status codes to indicate the success or failure of the API call. The body of the response will be JSON in the following format:  ``` {   \"message\": \"page not found\" } ```  # Versioning  The API is usually changed in each release, so API calls are versioned to ensure that clients don't break. To lock to a specific version of the API, you prefix the URL with its version, for example, call `/v1.30/info` to use the v1.30 version of the `/info` endpoint. If the API version specified in the URL is not supported by the daemon, a HTTP `400 Bad Request` error message is returned.  If you omit the version-prefix, the current version of the API (v1.46) is used. For example, calling `/info` is the same as calling `/v1.46/info`. Using the API without a version-prefix is deprecated and will be removed in a future release.  Engine releases in the near future should support this version of the API, so your client will continue to work even if it is talking to a newer Engine.  The API uses an open schema model, which means server may add extra properties to responses. Likewise, the server will ignore any extra query parameters and request body properties. When you write clients, you need to ignore additional properties in responses to ensure they do not break when talking to newer daemons.   # Authentication  Authentication for registries is handled client side. The client has to send authentication details to various endpoints that need to communicate with registries, such as `POST /images/(name)/push`. These are sent as `X-Registry-Auth` header as a [base64url encoded](https://tools.ietf.org/html/rfc4648#section-5) (JSON) string with the following structure:  ``` {   \"username\": \"string\",   \"password\": \"string\",   \"email\": \"string\",   \"serveraddress\": \"string\" } ```  The `serveraddress` is a domain/IP without a protocol. Throughout this structure, double quotes are required.  If you have already got an identity token from the [`/auth` endpoint](#operation/SystemAuth), you can just pass this instead of credentials:  ``` {   \"identitytoken\": \"9cbaf023786cd7...\" } ``` 

   OpenAPI Version: 3.0.1
   Docker Engine API API version: 1.46
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : DockerEngine.Client
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module DockerEngine.Client where

import DockerEngine.Core
import DockerEngine.Logging
import DockerEngine.MimeTypes

import qualified Control.Exception.Safe as E
import qualified Control.Monad.IO.Class as P
import qualified Control.Monad as P
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Exts (IsString(..))

-- * Dispatch

-- ** Lbs

-- | send a request returning the raw http response
dispatchLbs
  :: (Produces req accept, MimeType contentType)
  => NH.Manager -- ^ http-client Connection manager
  -> DockerEngineConfig -- ^ config
  -> DockerEngineRequest req contentType res accept -- ^ request
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatchLbs manager config request  = do
  initReq <- _toInitRequest config request
  dispatchInitUnsafe manager config initReq

-- ** Mime

-- | pair of decoded http body and http response
data MimeResult res =
  MimeResult { mimeResult :: Either MimeError res -- ^ decoded http body
             , mimeResultResponse :: NH.Response BCL.ByteString -- ^ http response
             }
  deriving (Show, Functor, Foldable, Traversable)

-- | pair of unrender/parser error and http response
data MimeError =
  MimeError {
    mimeError :: String -- ^ unrender/parser error
  , mimeErrorResponse :: NH.Response BCL.ByteString -- ^ http response
  } deriving (Show)

-- | send a request returning the 'MimeResult'
dispatchMime
  :: forall req contentType res accept. (Produces req accept, MimeUnrender accept res, MimeType contentType)
  => NH.Manager -- ^ http-client Connection manager
  -> DockerEngineConfig -- ^ config
  -> DockerEngineRequest req contentType res accept -- ^ request
  -> IO (MimeResult res) -- ^ response
dispatchMime manager config request = do
  httpResponse <- dispatchLbs manager config request
  let statusCode = NH.statusCode . NH.responseStatus $ httpResponse
  parsedResult <-
    runConfigLogWithExceptions "Client" config $
    do if (statusCode >= 400 && statusCode < 600)
         then do
           let s = "error statusCode: " ++ show statusCode
           _log "Client" levelError (T.pack s)
           pure (Left (MimeError s httpResponse))
         else case mimeUnrender (P.Proxy :: P.Proxy accept) (NH.responseBody httpResponse) of
           Left s -> do
             _log "Client" levelError (T.pack s)
             pure (Left (MimeError s httpResponse))
           Right r -> pure (Right r)
  return (MimeResult parsedResult httpResponse)

-- | like 'dispatchMime', but only returns the decoded http body
dispatchMime'
  :: (Produces req accept, MimeUnrender accept res, MimeType contentType)
  => NH.Manager -- ^ http-client Connection manager
  -> DockerEngineConfig -- ^ config
  -> DockerEngineRequest req contentType res accept -- ^ request
  -> IO (Either MimeError res) -- ^ response
dispatchMime' manager config request  = do
    MimeResult parsedResult _ <- dispatchMime manager config request
    return parsedResult

-- ** Unsafe

-- | like 'dispatchReqLbs', but does not validate the operation is a 'Producer' of the "accept" 'MimeType'.  (Useful if the server's response is undocumented)
dispatchLbsUnsafe
  :: (MimeType accept, MimeType contentType)
  => NH.Manager -- ^ http-client Connection manager
  -> DockerEngineConfig -- ^ config
  -> DockerEngineRequest req contentType res accept -- ^ request
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatchLbsUnsafe manager config request  = do
  initReq <- _toInitRequest config request
  dispatchInitUnsafe manager config initReq

-- | dispatch an InitRequest
dispatchInitUnsafe
  :: NH.Manager -- ^ http-client Connection manager
  -> DockerEngineConfig -- ^ config
  -> InitRequest req contentType res accept -- ^ init request
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatchInitUnsafe manager config (InitRequest req) = do
  runConfigLogWithExceptions src config $
    do _log src levelInfo requestLogMsg
       _log src levelDebug requestDbgLogMsg
       res <- P.liftIO $ NH.httpLbs req manager
       _log src levelInfo (responseLogMsg res)
       _log src levelDebug ((T.pack . show) res)
       return res
  where
    src = "Client"
    endpoint =
      T.pack $
      BC.unpack $
      NH.method req <> " " <> NH.host req <> NH.path req <> NH.queryString req
    requestLogMsg = "REQ:" <> endpoint
    requestDbgLogMsg =
      "Headers=" <> (T.pack . show) (NH.requestHeaders req) <> " Body=" <>
      (case NH.requestBody req of
         NH.RequestBodyLBS xs -> T.decodeUtf8 (BL.toStrict xs)
         _ -> "<RequestBody>")
    responseStatusCode = (T.pack . show) . NH.statusCode . NH.responseStatus
    responseLogMsg res =
      "RES:statusCode=" <> responseStatusCode res <> " (" <> endpoint <> ")"

-- * InitRequest

-- | wraps an http-client 'Request' with request/response type parameters
newtype InitRequest req contentType res accept = InitRequest
  { unInitRequest :: NH.Request
  } deriving (Show)

-- |  Build an http-client 'Request' record from the supplied config and request
_toInitRequest
  :: (MimeType accept, MimeType contentType)
  => DockerEngineConfig -- ^ config
  -> DockerEngineRequest req contentType res accept -- ^ request
  -> IO (InitRequest req contentType res accept) -- ^ initialized request
_toInitRequest config req0  =
  runConfigLogWithExceptions "Client" config $ do
    parsedReq <- P.liftIO $ NH.parseRequest $ BCL.unpack $ BCL.append (configHost config) (BCL.concat (rUrlPath req0))
    req1 <- P.liftIO $ _applyAuthMethods req0 config
    P.when
        (configValidateAuthMethods config && (not . null . rAuthTypes) req1)
        (E.throw $ AuthMethodException $ "AuthMethod not configured: " <> (show . head . rAuthTypes) req1)
    let req2 = req1 & _setContentTypeHeader & _setAcceptHeader
        params = rParams req2
        reqHeaders = ("User-Agent", WH.toHeader (configUserAgent config)) : paramsHeaders params
        reqQuery = let query = paramsQuery params
                       queryExtraUnreserved = configQueryExtraUnreserved config
                   in if B.null queryExtraUnreserved
                        then NH.renderQuery True query
                        else NH.renderQueryPartialEscape True (toPartialEscapeQuery queryExtraUnreserved query)
        pReq = parsedReq { NH.method = rMethod req2
                        , NH.requestHeaders = reqHeaders
                        , NH.queryString = reqQuery
                        }
    outReq <- case paramsBody params of
        ParamBodyNone -> pure (pReq { NH.requestBody = mempty })
        ParamBodyB bs -> pure (pReq { NH.requestBody = NH.RequestBodyBS bs })
        ParamBodyBL bl -> pure (pReq { NH.requestBody = NH.RequestBodyLBS bl })
        ParamBodyFormUrlEncoded form -> pure (pReq { NH.requestBody = NH.RequestBodyLBS (WH.urlEncodeForm form) })
        ParamBodyMultipartFormData parts -> NH.formDataBody parts pReq

    pure (InitRequest outReq)

-- | modify the underlying Request
modifyInitRequest :: InitRequest req contentType res accept -> (NH.Request -> NH.Request) -> InitRequest req contentType res accept
modifyInitRequest (InitRequest req) f = InitRequest (f req)

-- | modify the underlying Request (monadic)
modifyInitRequestM :: Monad m => InitRequest req contentType res accept -> (NH.Request -> m NH.Request) -> m (InitRequest req contentType res accept)
modifyInitRequestM (InitRequest req) f = fmap InitRequest (f req)

-- ** Logging

-- | Run a block using the configured logger instance
runConfigLog
  :: P.MonadIO m
  => DockerEngineConfig -> LogExec m a
runConfigLog config = configLogExecWithContext config (configLogContext config)

-- | Run a block using the configured logger instance (logs exceptions)
runConfigLogWithExceptions
  :: (E.MonadCatch m, P.MonadIO m)
  => T.Text -> DockerEngineConfig -> LogExec m a
runConfigLogWithExceptions src config = runConfigLog config . logExceptions src
