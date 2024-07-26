{-
   Docker Engine API

   The Engine API is an HTTP API served by Docker Engine. It is the API the Docker client uses to communicate with the Engine, so everything the Docker client can do can be done with the API.  Most of the client's commands map directly to API endpoints (e.g. `docker ps` is `GET /containers/json`). The notable exception is running containers, which consists of several API calls.  # Errors  The API uses standard HTTP status codes to indicate the success or failure of the API call. The body of the response will be JSON in the following format:  ``` {   \"message\": \"page not found\" } ```  # Versioning  The API is usually changed in each release, so API calls are versioned to ensure that clients don't break. To lock to a specific version of the API, you prefix the URL with its version, for example, call `/v1.30/info` to use the v1.30 version of the `/info` endpoint. If the API version specified in the URL is not supported by the daemon, a HTTP `400 Bad Request` error message is returned.  If you omit the version-prefix, the current version of the API (v1.44) is used. For example, calling `/info` is the same as calling `/v1.44/info`. Using the API without a version-prefix is deprecated and will be removed in a future release.  Engine releases in the near future should support this version of the API, so your client will continue to work even if it is talking to a newer Engine.  The API uses an open schema model, which means server may add extra properties to responses. Likewise, the server will ignore any extra query parameters and request body properties. When you write clients, you need to ignore additional properties in responses to ensure they do not break when talking to newer daemons.   # Authentication  Authentication for registries is handled client side. The client has to send authentication details to various endpoints that need to communicate with registries, such as `POST /images/(name)/push`. These are sent as `X-Registry-Auth` header as a [base64url encoded](https://tools.ietf.org/html/rfc4648#section-5) (JSON) string with the following structure:  ``` {   \"username\": \"string\",   \"password\": \"string\",   \"email\": \"string\",   \"serveraddress\": \"string\" } ```  The `serveraddress` is a domain/IP without a protocol. Throughout this structure, double quotes are required.  If you have already got an identity token from the [`/auth` endpoint](#operation/SystemAuth), you can just pass this instead of credentials:  ``` {   \"identitytoken\": \"9cbaf023786cd7...\" } ``` 

   OpenAPI Version: 3.0.1
   Docker Engine API API version: 1.44
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : DockerEngine.LoggingMonadLogger
monad-logger Logging functions
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DockerEngine.LoggingMonadLogger where

import qualified Control.Exception.Safe as E
import qualified Control.Monad.IO.Class as P
import qualified Data.Text as T
import qualified Data.Time as TI

import Data.Text (Text)

import qualified Control.Monad.Logger as LG

-- * Type Aliases (for compatibility)

-- | Runs a monad-logger  block with the filter predicate
type LogExecWithContext = forall m a. P.MonadIO m =>
                                      LogContext -> LogExec m a

-- | A monad-logger block
type LogExec m a = LG.LoggingT m a -> m a

-- | A monad-logger filter predicate
type LogContext = LG.LogSource -> LG.LogLevel -> Bool

-- | A monad-logger log level
type LogLevel = LG.LogLevel

-- * default logger

-- | the default log environment
initLogContext :: IO LogContext
initLogContext = pure infoLevelFilter

-- | Runs a monad-logger block with the filter predicate
runDefaultLogExecWithContext :: LogExecWithContext
runDefaultLogExecWithContext = runNullLogExec

-- * stdout logger

-- | Runs a monad-logger block targeting stdout, with the filter predicate
stdoutLoggingExec :: LogExecWithContext
stdoutLoggingExec cxt = LG.runStdoutLoggingT . LG.filterLogger cxt

-- | @pure@
stdoutLoggingContext :: LogContext -> IO LogContext
stdoutLoggingContext = pure

-- * stderr logger

-- | Runs a monad-logger block targeting stderr, with the filter predicate
stderrLoggingExec :: LogExecWithContext
stderrLoggingExec cxt = LG.runStderrLoggingT . LG.filterLogger cxt

-- | @pure@
stderrLoggingContext :: LogContext -> IO LogContext
stderrLoggingContext = pure

-- * Null logger

-- | Disables monad-logger logging
runNullLogExec :: LogExecWithContext
runNullLogExec = const (`LG.runLoggingT` nullLogger)

-- | monad-logger which does nothing
nullLogger :: LG.Loc -> LG.LogSource -> LG.LogLevel -> LG.LogStr -> IO ()
nullLogger _ _ _ _ = return ()

-- * Log Msg

-- | Log a message using the current time
_log :: (P.MonadIO m, LG.MonadLogger m) => Text -> LG.LogLevel -> Text -> m ()
_log src level msg = do
  now <- P.liftIO (formatTimeLog <$> TI.getCurrentTime)
  LG.logOtherNS ("DockerEngine." <> src) level ("[" <> now <> "] " <> msg)
 where
  formatTimeLog =
    T.pack . TI.formatTime TI.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z"

-- * Log Exceptions

-- | re-throws exceptions after logging them
logExceptions
   :: (LG.MonadLogger m, E.MonadCatch m, P.MonadIO m)
   => Text -> m a -> m a
logExceptions src =
   E.handle
     (\(e :: E.SomeException) -> do
        _log src LG.LevelError ((T.pack . show) e)
        E.throw e)

-- * Log Level

levelInfo :: LogLevel
levelInfo = LG.LevelInfo

levelError :: LogLevel
levelError = LG.LevelError

levelDebug :: LogLevel
levelDebug = LG.LevelDebug

-- * Level Filter

minLevelFilter :: LG.LogLevel -> LG.LogSource -> LG.LogLevel -> Bool
minLevelFilter l _ l' = l' >= l

infoLevelFilter :: LG.LogSource -> LG.LogLevel -> Bool
infoLevelFilter = minLevelFilter LG.LevelInfo
