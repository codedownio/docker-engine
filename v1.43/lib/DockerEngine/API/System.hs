{-
   Docker Engine API

   The Engine API is an HTTP API served by Docker Engine. It is the API the Docker client uses to communicate with the Engine, so everything the Docker client can do can be done with the API.  Most of the client's commands map directly to API endpoints (e.g. `docker ps` is `GET /containers/json`). The notable exception is running containers, which consists of several API calls.  # Errors  The API uses standard HTTP status codes to indicate the success or failure of the API call. The body of the response will be JSON in the following format:  ``` {   \"message\": \"page not found\" } ```  # Versioning  The API is usually changed in each release, so API calls are versioned to ensure that clients don't break. To lock to a specific version of the API, you prefix the URL with its version, for example, call `/v1.30/info` to use the v1.30 version of the `/info` endpoint. If the API version specified in the URL is not supported by the daemon, a HTTP `400 Bad Request` error message is returned.  If you omit the version-prefix, the current version of the API (v1.43) is used. For example, calling `/info` is the same as calling `/v1.43/info`. Using the API without a version-prefix is deprecated and will be removed in a future release.  Engine releases in the near future should support this version of the API, so your client will continue to work even if it is talking to a newer Engine.  The API uses an open schema model, which means server may add extra properties to responses. Likewise, the server will ignore any extra query parameters and request body properties. When you write clients, you need to ignore additional properties in responses to ensure they do not break when talking to newer daemons.   # Authentication  Authentication for registries is handled client side. The client has to send authentication details to various endpoints that need to communicate with registries, such as `POST /images/(name)/push`. These are sent as `X-Registry-Auth` header as a [base64url encoded](https://tools.ietf.org/html/rfc4648#section-5) (JSON) string with the following structure:  ``` {   \"username\": \"string\",   \"password\": \"string\",   \"email\": \"string\",   \"serveraddress\": \"string\" } ```  The `serveraddress` is a domain/IP without a protocol. Throughout this structure, double quotes are required.  If you have already got an identity token from the [`/auth` endpoint](#operation/SystemAuth), you can just pass this instead of credentials:  ``` {   \"identitytoken\": \"9cbaf023786cd7...\" } ``` 

   OpenAPI Version: 3.0.1
   Docker Engine API API version: 1.43
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : DockerEngine.API.System
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module DockerEngine.API.System where

import DockerEngine.Core
import DockerEngine.MimeTypes
import DockerEngine.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** System

-- *** systemAuth

-- | @POST \/auth@
-- 
-- Check auth configuration
-- 
-- Validate credentials for a registry and, if available, get an identity token for accessing the registry without password. 
-- 
systemAuth
  :: (Consumes SystemAuth MimeJSON)
  => DockerEngineRequest SystemAuth MimeJSON SystemAuthResponse MimeJSON
systemAuth =
  _mkRequest "POST" ["/auth"]

data SystemAuth 

-- | /Body Param/ "authConfig" - Authentication to check
instance HasBodyParam SystemAuth AuthConfig 

-- | @application/json@
instance Consumes SystemAuth MimeJSON

-- | @application/json@
instance Produces SystemAuth MimeJSON


-- *** systemDataUsage

-- | @GET \/system\/df@
-- 
-- Get data usage information
-- 
systemDataUsage
  :: Accept accept -- ^ request accept ('MimeType')
  -> DockerEngineRequest SystemDataUsage MimeNoContent SystemDataUsageResponse accept
systemDataUsage  _ =
  _mkRequest "GET" ["/system/df"]

data SystemDataUsage  

-- | /Optional Param/ "type" - Object types, for which to compute and return data. 
instance HasOptionalParam SystemDataUsage ParamType where
  applyOptionalParam req (ParamType xs) =
    req `addQuery` toQueryColl MultiParamArray ("type", Just xs)
-- | @application/json@
instance Produces SystemDataUsage MimeJSON
-- | @text/plain@
instance Produces SystemDataUsage MimePlainText


-- *** systemEvents

-- | @GET \/events@
-- 
-- Monitor events
-- 
-- Stream real-time events from the server.  Various objects within Docker report events when something happens to them.  Containers report these events: `attach`, `commit`, `copy`, `create`, `destroy`, `detach`, `die`, `exec_create`, `exec_detach`, `exec_start`, `exec_die`, `export`, `health_status`, `kill`, `oom`, `pause`, `rename`, `resize`, `restart`, `start`, `stop`, `top`, `unpause`, `update`, and `prune`  Images report these events: `delete`, `import`, `load`, `pull`, `push`, `save`, `tag`, `untag`, and `prune`  Volumes report these events: `create`, `mount`, `unmount`, `destroy`, and `prune`  Networks report these events: `create`, `connect`, `disconnect`, `destroy`, `update`, `remove`, and `prune`  The Docker daemon reports these events: `reload`  Services report these events: `create`, `update`, and `remove`  Nodes report these events: `create`, `update`, and `remove`  Secrets report these events: `create`, `update`, and `remove`  Configs report these events: `create`, `update`, and `remove`  The Builder reports `prune` events 
-- 
systemEvents
  :: DockerEngineRequest SystemEvents MimeNoContent EventMessage MimeJSON
systemEvents =
  _mkRequest "GET" ["/events"]

data SystemEvents  

-- | /Optional Param/ "since" - Show events created since this timestamp then stream new events.
instance HasOptionalParam SystemEvents SinceText where
  applyOptionalParam req (SinceText xs) =
    req `addQuery` toQuery ("since", Just xs)

-- | /Optional Param/ "until" - Show events created until this timestamp then stop streaming.
instance HasOptionalParam SystemEvents UntilText where
  applyOptionalParam req (UntilText xs) =
    req `addQuery` toQuery ("until", Just xs)

-- | /Optional Param/ "filters" - A JSON encoded value of filters (a `map[string][]string`) to process on the event list. Available filters:  - `config=<string>` config name or ID - `container=<string>` container name or ID - `daemon=<string>` daemon name or ID - `event=<string>` event type - `image=<string>` image name or ID - `label=<string>` image or container label - `network=<string>` network name or ID - `node=<string>` node ID - `plugin`=<string> plugin name or ID - `scope`=<string> local or swarm - `secret=<string>` secret name or ID - `service=<string>` service name or ID - `type=<string>` object to filter by, one of `container`, `image`, `volume`, `network`, `daemon`, `plugin`, `node`, `service`, `secret` or `config` - `volume=<string>` volume name 
instance HasOptionalParam SystemEvents Filters where
  applyOptionalParam req (Filters xs) =
    req `addQuery` toQuery ("filters", Just xs)
-- | @application/json@
instance Produces SystemEvents MimeJSON


-- *** systemInfo0

-- | @GET \/info@
-- 
-- Get system information
-- 
systemInfo0
  :: DockerEngineRequest SystemInfo0 MimeNoContent SystemInfo MimeJSON
systemInfo0 =
  _mkRequest "GET" ["/info"]

data SystemInfo0  
-- | @application/json@
instance Produces SystemInfo0 MimeJSON


-- *** systemPing

-- | @GET \/_ping@
-- 
-- Ping
-- 
-- This is a dummy endpoint you can use to test if the server is accessible.
-- 
systemPing
  :: DockerEngineRequest SystemPing MimeNoContent Text MimePlainText
systemPing =
  _mkRequest "GET" ["/_ping"]

data SystemPing  
-- | @text/plain@
instance Produces SystemPing MimePlainText


-- *** systemPingHead

-- | @HEAD \/_ping@
-- 
-- Ping
-- 
-- This is a dummy endpoint you can use to test if the server is accessible.
-- 
systemPingHead
  :: DockerEngineRequest SystemPingHead MimeNoContent Text MimePlainText
systemPingHead =
  _mkRequest "HEAD" ["/_ping"]

data SystemPingHead  
-- | @text/plain@
instance Produces SystemPingHead MimePlainText


-- *** systemVersion0

-- | @GET \/version@
-- 
-- Get version
-- 
-- Returns the version of Docker that is running and various information about the system that Docker is running on.
-- 
systemVersion0
  :: DockerEngineRequest SystemVersion0 MimeNoContent SystemVersion MimeJSON
systemVersion0 =
  _mkRequest "GET" ["/version"]

data SystemVersion0  
-- | @application/json@
instance Produces SystemVersion0 MimeJSON

