{-
   Docker Engine API

   The Engine API is an HTTP API served by Docker Engine. It is the API the Docker client uses to communicate with the Engine, so everything the Docker client can do can be done with the API.  Most of the client's commands map directly to API endpoints (e.g. `docker ps` is `GET /containers/json`). The notable exception is running containers, which consists of several API calls.  # Errors  The API uses standard HTTP status codes to indicate the success or failure of the API call. The body of the response will be JSON in the following format:  ``` {   \"message\": \"page not found\" } ```  # Versioning  The API is usually changed in each release, so API calls are versioned to ensure that clients don't break. To lock to a specific version of the API, you prefix the URL with its version, for example, call `/v1.30/info` to use the v1.30 version of the `/info` endpoint. If the API version specified in the URL is not supported by the daemon, a HTTP `400 Bad Request` error message is returned.  If you omit the version-prefix, the current version of the API (v1.36) is used. For example, calling `/info` is the same as calling `/v1.36/info`. Using the API without a version-prefix is deprecated and will be removed in a future release.  Engine releases in the near future should support this version of the API, so your client will continue to work even if it is talking to a newer Engine.  The API uses an open schema model, which means server may add extra properties to responses. Likewise, the server will ignore any extra query parameters and request body properties. When you write clients, you need to ignore additional properties in responses to ensure they do not break when talking to newer daemons.   # Authentication  Authentication for registries is handled client side. The client has to send authentication details to various endpoints that need to communicate with registries, such as `POST /images/(name)/push`. These are sent as `X-Registry-Auth` header as a Base64 encoded (JSON) string with the following structure:  ``` {   \"username\": \"string\",   \"password\": \"string\",   \"email\": \"string\",   \"serveraddress\": \"string\" } ```  The `serveraddress` is a domain/IP without a protocol. Throughout this structure, double quotes are required.  If you have already got an identity token from the [`/auth` endpoint](#operation/SystemAuth), you can just pass this instead of credentials:  ``` {   \"identitytoken\": \"9cbaf023786cd7...\" } ``` 

   OpenAPI Version: 3.0.1
   Docker Engine API API version: 1.36
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : DockerEngine.API.Network
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module DockerEngine.API.Network where

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


-- ** Network

-- *** networkConnect

-- | @POST \/networks\/{id}\/connect@
-- 
-- Connect a container to a network
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
networkConnect 
  :: (Consumes NetworkConnect MimeJSON, MimeRender MimeJSON NetworkConnectConfig)
  => Accept accept -- ^ request accept ('MimeType')
  -> NetworkConnectConfig -- ^ "container"
  -> Id -- ^ "id" -  Network ID or name
  -> DockerEngineRequest NetworkConnect MimeJSON res accept
networkConnect  _ container (Id id) =
  _mkRequest "POST" ["/networks/",toPath id,"/connect"]
    `setBodyParam` container

data NetworkConnect 
instance HasBodyParam NetworkConnect NetworkConnectConfig 

-- | @application/json@
instance Consumes NetworkConnect MimeJSON

-- | @application/json@
instance Produces NetworkConnect MimeJSON
-- | @text/plain@
instance Produces NetworkConnect MimePlainText


-- *** networkCreate

-- | @POST \/networks\/create@
-- 
-- Create a network
-- 
networkCreate 
  :: (Consumes NetworkCreate MimeJSON, MimeRender MimeJSON NetworkConfig)
  => NetworkConfig -- ^ "networkConfig"
  -> DockerEngineRequest NetworkCreate MimeJSON NetworkCreateResponse MimeJSON
networkCreate networkConfig =
  _mkRequest "POST" ["/networks/create"]
    `setBodyParam` networkConfig

data NetworkCreate 
instance HasBodyParam NetworkCreate NetworkConfig 

-- | @application/json@
instance Consumes NetworkCreate MimeJSON

-- | @application/json@
instance Produces NetworkCreate MimeJSON


-- *** networkDelete

-- | @DELETE \/networks\/{id}@
-- 
-- Remove a network
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
networkDelete 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  Network ID or name
  -> DockerEngineRequest NetworkDelete MimeNoContent res accept
networkDelete  _ (Id id) =
  _mkRequest "DELETE" ["/networks/",toPath id]

data NetworkDelete  
-- | @application/json@
instance Produces NetworkDelete MimeJSON
-- | @text/plain@
instance Produces NetworkDelete MimePlainText


-- *** networkDisconnect

-- | @POST \/networks\/{id}\/disconnect@
-- 
-- Disconnect a container from a network
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
networkDisconnect 
  :: (Consumes NetworkDisconnect MimeJSON, MimeRender MimeJSON NetworkDisconnectConfig)
  => Accept accept -- ^ request accept ('MimeType')
  -> NetworkDisconnectConfig -- ^ "container"
  -> Id -- ^ "id" -  Network ID or name
  -> DockerEngineRequest NetworkDisconnect MimeJSON res accept
networkDisconnect  _ container (Id id) =
  _mkRequest "POST" ["/networks/",toPath id,"/disconnect"]
    `setBodyParam` container

data NetworkDisconnect 
instance HasBodyParam NetworkDisconnect NetworkDisconnectConfig 

-- | @application/json@
instance Consumes NetworkDisconnect MimeJSON

-- | @application/json@
instance Produces NetworkDisconnect MimeJSON
-- | @text/plain@
instance Produces NetworkDisconnect MimePlainText


-- *** networkInspect

-- | @GET \/networks\/{id}@
-- 
-- Inspect a network
-- 
networkInspect 
  :: Id -- ^ "id" -  Network ID or name
  -> DockerEngineRequest NetworkInspect MimeNoContent Network MimeJSON
networkInspect (Id id) =
  _mkRequest "GET" ["/networks/",toPath id]

data NetworkInspect  

-- | /Optional Param/ "verbose" - Detailed inspect output for troubleshooting
instance HasOptionalParam NetworkInspect Verbose where
  applyOptionalParam req (Verbose xs) =
    req `setQuery` toQuery ("verbose", Just xs)

-- | /Optional Param/ "scope" - Filter the network by scope (swarm, global, or local)
instance HasOptionalParam NetworkInspect Scope where
  applyOptionalParam req (Scope xs) =
    req `setQuery` toQuery ("scope", Just xs)
-- | @application/json@
instance Produces NetworkInspect MimeJSON


-- *** networkList

-- | @GET \/networks@
-- 
-- List networks
-- 
-- Returns a list of networks. For details on the format, see [the network inspect endpoint](#operation/NetworkInspect).  Note that it uses a different, smaller representation of a network than inspecting a single network. For example, the list of containers attached to the network is not propagated in API versions 1.28 and up. 
-- 
networkList 
  :: DockerEngineRequest NetworkList MimeNoContent [Network] MimeJSON
networkList =
  _mkRequest "GET" ["/networks"]

data NetworkList  

-- | /Optional Param/ "filters" - JSON encoded value of the filters (a `map[string][]string`) to process on the networks list. Available filters:  - `driver=<driver-name>` Matches a network's driver. - `id=<network-id>` Matches all or part of a network ID. - `label=<key>` or `label=<key>=<value>` of a network label. - `name=<network-name>` Matches all or part of a network name. - `scope=[\"swarm\"|\"global\"|\"local\"]` Filters networks by scope (`swarm`, `global`, or `local`). - `type=[\"custom\"|\"builtin\"]` Filters networks by type. The `custom` keyword returns all user-defined networks. 
instance HasOptionalParam NetworkList Filters where
  applyOptionalParam req (Filters xs) =
    req `setQuery` toQuery ("filters", Just xs)
-- | @application/json@
instance Produces NetworkList MimeJSON


-- *** networkPrune

-- | @POST \/networks\/prune@
-- 
-- Delete unused networks
-- 
networkPrune 
  :: DockerEngineRequest NetworkPrune MimeNoContent NetworkPruneResponse MimeJSON
networkPrune =
  _mkRequest "POST" ["/networks/prune"]

data NetworkPrune  

-- | /Optional Param/ "filters" - Filters to process on the prune list, encoded as JSON (a `map[string][]string`).  Available filters: - `until=<timestamp>` Prune networks created before this timestamp. The `<timestamp>` can be Unix timestamps, date formatted timestamps, or Go duration strings (e.g. `10m`, `1h30m`) computed relative to the daemon machine’s time. - `label` (`label=<key>`, `label=<key>=<value>`, `label!=<key>`, or `label!=<key>=<value>`) Prune networks with (or without, in case `label!=...` is used) the specified labels. 
instance HasOptionalParam NetworkPrune Filters where
  applyOptionalParam req (Filters xs) =
    req `setQuery` toQuery ("filters", Just xs)
-- | @application/json@
instance Produces NetworkPrune MimeJSON

