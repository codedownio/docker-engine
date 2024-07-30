{-
   Docker Engine API

   The Engine API is an HTTP API served by Docker Engine. It is the API the Docker client uses to communicate with the Engine, so everything the Docker client can do can be done with the API.  Most of the client's commands map directly to API endpoints (e.g. `docker ps` is `GET /containers/json`). The notable exception is running containers, which consists of several API calls.  # Errors  The API uses standard HTTP status codes to indicate the success or failure of the API call. The body of the response will be JSON in the following format:  ``` {   \"message\": \"page not found\" } ```  # Versioning  The API is usually changed in each release, so API calls are versioned to ensure that clients don't break. To lock to a specific version of the API, you prefix the URL with its version, for example, call `/v1.30/info` to use the v1.30 version of the `/info` endpoint. If the API version specified in the URL is not supported by the daemon, a HTTP `400 Bad Request` error message is returned.  If you omit the version-prefix, the current version of the API (v1.46) is used. For example, calling `/info` is the same as calling `/v1.46/info`. Using the API without a version-prefix is deprecated and will be removed in a future release.  Engine releases in the near future should support this version of the API, so your client will continue to work even if it is talking to a newer Engine.  The API uses an open schema model, which means server may add extra properties to responses. Likewise, the server will ignore any extra query parameters and request body properties. When you write clients, you need to ignore additional properties in responses to ensure they do not break when talking to newer daemons.   # Authentication  Authentication for registries is handled client side. The client has to send authentication details to various endpoints that need to communicate with registries, such as `POST /images/(name)/push`. These are sent as `X-Registry-Auth` header as a [base64url encoded](https://tools.ietf.org/html/rfc4648#section-5) (JSON) string with the following structure:  ``` {   \"username\": \"string\",   \"password\": \"string\",   \"email\": \"string\",   \"serveraddress\": \"string\" } ```  The `serveraddress` is a domain/IP without a protocol. Throughout this structure, double quotes are required.  If you have already got an identity token from the [`/auth` endpoint](#operation/SystemAuth), you can just pass this instead of credentials:  ``` {   \"identitytoken\": \"9cbaf023786cd7...\" } ``` 

   OpenAPI Version: 3.0.1
   Docker Engine API API version: 1.46
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : DockerEngine.API.Service
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module DockerEngine.API.Service where

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


-- ** Service

-- *** serviceCreate

-- | @POST \/services\/create@
-- 
-- Create a service
-- 
serviceCreate
  :: (Consumes ServiceCreate MimeJSON, MimeRender MimeJSON ServiceCreateRequest)
  => ServiceCreateRequest -- ^ "body"
  -> DockerEngineRequest ServiceCreate MimeJSON ServiceCreateResponse MimeJSON
serviceCreate body =
  _mkRequest "POST" ["/services/create"]
    `setBodyParam` body

data ServiceCreate 
instance HasBodyParam ServiceCreate ServiceCreateRequest 

-- | /Optional Param/ "X-Registry-Auth" - A base64url-encoded auth configuration for pulling from private registries.  Refer to the [authentication section](#section/Authentication) for details. 
instance HasOptionalParam ServiceCreate XRegistryAuth where
  applyOptionalParam req (XRegistryAuth xs) =
    req `addHeader` toHeader ("X-Registry-Auth", xs)

-- | @application/json@
instance Consumes ServiceCreate MimeJSON

-- | @application/json@
instance Produces ServiceCreate MimeJSON


-- *** serviceDelete

-- | @DELETE \/services\/{id}@
-- 
-- Delete a service
-- 
serviceDelete
  :: Id -- ^ "id" -  ID or name of service.
  -> DockerEngineRequest ServiceDelete MimeNoContent NoContent MimeNoContent
serviceDelete (Id id) =
  _mkRequest "DELETE" ["/services/",toPath id]

data ServiceDelete  
instance Produces ServiceDelete MimeNoContent


-- *** serviceInspect

-- | @GET \/services\/{id}@
-- 
-- Inspect a service
-- 
serviceInspect
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of service.
  -> DockerEngineRequest ServiceInspect MimeNoContent Service accept
serviceInspect  _ (Id id) =
  _mkRequest "GET" ["/services/",toPath id]

data ServiceInspect  

-- | /Optional Param/ "insertDefaults" - Fill empty fields with default values.
instance HasOptionalParam ServiceInspect InsertDefaults where
  applyOptionalParam req (InsertDefaults xs) =
    req `addQuery` toQuery ("insertDefaults", Just xs)
-- | @application/json@
instance Produces ServiceInspect MimeJSON
-- | @text/plain@
instance Produces ServiceInspect MimePlainText


-- *** serviceList

-- | @GET \/services@
-- 
-- List services
-- 
serviceList
  :: Accept accept -- ^ request accept ('MimeType')
  -> DockerEngineRequest ServiceList MimeNoContent [Service] accept
serviceList  _ =
  _mkRequest "GET" ["/services"]

data ServiceList  

-- | /Optional Param/ "filters" - A JSON encoded value of the filters (a `map[string][]string`) to process on the services list.  Available filters:  - `id=<service id>` - `label=<service label>` - `mode=[\"replicated\"|\"global\"]` - `name=<service name>` 
instance HasOptionalParam ServiceList Filters where
  applyOptionalParam req (Filters xs) =
    req `addQuery` toQuery ("filters", Just xs)

-- | /Optional Param/ "status" - Include service status, with count of running and desired tasks. 
instance HasOptionalParam ServiceList Status where
  applyOptionalParam req (Status xs) =
    req `addQuery` toQuery ("status", Just xs)
-- | @application/json@
instance Produces ServiceList MimeJSON
-- | @text/plain@
instance Produces ServiceList MimePlainText


-- *** serviceLogs

-- | @GET \/services\/{id}\/logs@
-- 
-- Get service logs
-- 
-- Get `stdout` and `stderr` logs from a service. See also [`/containers/{id}/logs`](#operation/ContainerLogs).  **Note**: This endpoint works only for services with the `local`, `json-file` or `journald` logging drivers. 
-- 
serviceLogs
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the service
  -> DockerEngineRequest ServiceLogs MimeNoContent FilePath accept
serviceLogs  _ (Id id) =
  _mkRequest "GET" ["/services/",toPath id,"/logs"]

data ServiceLogs  

-- | /Optional Param/ "details" - Show service context and extra details provided to logs.
instance HasOptionalParam ServiceLogs Details where
  applyOptionalParam req (Details xs) =
    req `addQuery` toQuery ("details", Just xs)

-- | /Optional Param/ "follow" - Keep connection after returning logs.
instance HasOptionalParam ServiceLogs Follow where
  applyOptionalParam req (Follow xs) =
    req `addQuery` toQuery ("follow", Just xs)

-- | /Optional Param/ "stdout" - Return logs from `stdout`
instance HasOptionalParam ServiceLogs Stdout where
  applyOptionalParam req (Stdout xs) =
    req `addQuery` toQuery ("stdout", Just xs)

-- | /Optional Param/ "stderr" - Return logs from `stderr`
instance HasOptionalParam ServiceLogs Stderr where
  applyOptionalParam req (Stderr xs) =
    req `addQuery` toQuery ("stderr", Just xs)

-- | /Optional Param/ "since" - Only return logs since this time, as a UNIX timestamp
instance HasOptionalParam ServiceLogs Since where
  applyOptionalParam req (Since xs) =
    req `addQuery` toQuery ("since", Just xs)

-- | /Optional Param/ "timestamps" - Add timestamps to every log line
instance HasOptionalParam ServiceLogs Timestamps where
  applyOptionalParam req (Timestamps xs) =
    req `addQuery` toQuery ("timestamps", Just xs)

-- | /Optional Param/ "tail" - Only return this number of log lines from the end of the logs. Specify as an integer or `all` to output all log lines. 
instance HasOptionalParam ServiceLogs Tail where
  applyOptionalParam req (Tail xs) =
    req `addQuery` toQuery ("tail", Just xs)
-- | @application/json@
instance Produces ServiceLogs MimeJSON
-- | @application/vnd.docker.raw-stream@
instance Produces ServiceLogs MimeVndDockerRawStream
-- | @application/vnd.docker.multiplexed-stream@
instance Produces ServiceLogs MimeVndDockerMultiplexedStream


-- *** serviceUpdate

-- | @POST \/services\/{id}\/update@
-- 
-- Update a service
-- 
serviceUpdate
  :: (Consumes ServiceUpdate MimeJSON, MimeRender MimeJSON ServiceUpdateRequest)
  => ServiceUpdateRequest -- ^ "body"
  -> Id -- ^ "id" -  ID or name of service.
  -> VersionInt -- ^ "version" -  The version number of the service object being updated. This is required to avoid conflicting writes. This version number should be the value as currently set on the service *before* the update. You can find the current version by calling `GET /services/{id}` 
  -> DockerEngineRequest ServiceUpdate MimeJSON ServiceUpdateResponse MimeJSON
serviceUpdate body (Id id) (VersionInt version) =
  _mkRequest "POST" ["/services/",toPath id,"/update"]
    `setBodyParam` body
    `addQuery` toQuery ("version", Just version)

data ServiceUpdate 
instance HasBodyParam ServiceUpdate ServiceUpdateRequest 

-- | /Optional Param/ "registryAuthFrom" - If the `X-Registry-Auth` header is not specified, this parameter indicates where to find registry authorization credentials. 
instance HasOptionalParam ServiceUpdate RegistryAuthFrom where
  applyOptionalParam req (RegistryAuthFrom xs) =
    req `addQuery` toQuery ("registryAuthFrom", Just xs)

-- | /Optional Param/ "rollback" - Set to this parameter to `previous` to cause a server-side rollback to the previous service spec. The supplied spec will be ignored in this case. 
instance HasOptionalParam ServiceUpdate Rollback where
  applyOptionalParam req (Rollback xs) =
    req `addQuery` toQuery ("rollback", Just xs)

-- | /Optional Param/ "X-Registry-Auth" - A base64url-encoded auth configuration for pulling from private registries.  Refer to the [authentication section](#section/Authentication) for details. 
instance HasOptionalParam ServiceUpdate XRegistryAuth where
  applyOptionalParam req (XRegistryAuth xs) =
    req `addHeader` toHeader ("X-Registry-Auth", xs)

-- | @application/json@
instance Consumes ServiceUpdate MimeJSON

-- | @application/json@
instance Produces ServiceUpdate MimeJSON

