{-
   Docker Engine API

   The Engine API is an HTTP API served by Docker Engine. It is the API the Docker client uses to communicate with the Engine, so everything the Docker client can do can be done with the API.  Most of the client's commands map directly to API endpoints (e.g. `docker ps` is `GET /containers/json`). The notable exception is running containers, which consists of several API calls.  # Errors  The API uses standard HTTP status codes to indicate the success or failure of the API call. The body of the response will be JSON in the following format:  ``` {   \"message\": \"page not found\" } ```  # Versioning  The API is usually changed in each release, so API calls are versioned to ensure that clients don't break. To lock to a specific version of the API, you prefix the URL with its version, for example, call `/v1.30/info` to use the v1.30 version of the `/info` endpoint. If the API version specified in the URL is not supported by the daemon, a HTTP `400 Bad Request` error message is returned.  If you omit the version-prefix, the current version of the API (v1.43) is used. For example, calling `/info` is the same as calling `/v1.43/info`. Using the API without a version-prefix is deprecated and will be removed in a future release.  Engine releases in the near future should support this version of the API, so your client will continue to work even if it is talking to a newer Engine.  The API uses an open schema model, which means server may add extra properties to responses. Likewise, the server will ignore any extra query parameters and request body properties. When you write clients, you need to ignore additional properties in responses to ensure they do not break when talking to newer daemons.   # Authentication  Authentication for registries is handled client side. The client has to send authentication details to various endpoints that need to communicate with registries, such as `POST /images/(name)/push`. These are sent as `X-Registry-Auth` header as a [base64url encoded](https://tools.ietf.org/html/rfc4648#section-5) (JSON) string with the following structure:  ``` {   \"username\": \"string\",   \"password\": \"string\",   \"email\": \"string\",   \"serveraddress\": \"string\" } ```  The `serveraddress` is a domain/IP without a protocol. Throughout this structure, double quotes are required.  If you have already got an identity token from the [`/auth` endpoint](#operation/SystemAuth), you can just pass this instead of credentials:  ``` {   \"identitytoken\": \"9cbaf023786cd7...\" } ``` 

   OpenAPI Version: 3.0.1
   Docker Engine API API version: 1.43
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : DockerEngine.API.Config
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module DockerEngine.API.Config where

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


-- ** Config

-- *** configCreate

-- | @POST \/configs\/create@
-- 
-- Create a config
-- 
configCreate
  :: (Consumes ConfigCreate MimeJSON)
  => DockerEngineRequest ConfigCreate MimeJSON IdResponse MimeJSON
configCreate =
  _mkRequest "POST" ["/configs/create"]

data ConfigCreate 
instance HasBodyParam ConfigCreate ConfigCreateRequest 

-- | @application/json@
instance Consumes ConfigCreate MimeJSON

-- | @application/json@
instance Produces ConfigCreate MimeJSON


-- *** configDelete

-- | @DELETE \/configs\/{id}@
-- 
-- Delete a config
-- 
configDelete
  :: Id -- ^ "id" -  ID of the config
  -> DockerEngineRequest ConfigDelete MimeNoContent NoContent MimeNoContent
configDelete (Id id) =
  _mkRequest "DELETE" ["/configs/",toPath id]

data ConfigDelete  
instance Produces ConfigDelete MimeNoContent


-- *** configInspect

-- | @GET \/configs\/{id}@
-- 
-- Inspect a config
-- 
configInspect
  :: Id -- ^ "id" -  ID of the config
  -> DockerEngineRequest ConfigInspect MimeNoContent Config MimeJSON
configInspect (Id id) =
  _mkRequest "GET" ["/configs/",toPath id]

data ConfigInspect  
-- | @application/json@
instance Produces ConfigInspect MimeJSON


-- *** configList

-- | @GET \/configs@
-- 
-- List configs
-- 
configList
  :: DockerEngineRequest ConfigList MimeNoContent [Config] MimeJSON
configList =
  _mkRequest "GET" ["/configs"]

data ConfigList  

-- | /Optional Param/ "filters" - A JSON encoded value of the filters (a `map[string][]string`) to process on the configs list.  Available filters:  - `id=<config id>` - `label=<key> or label=<key>=value` - `name=<config name>` - `names=<config name>` 
instance HasOptionalParam ConfigList Filters where
  applyOptionalParam req (Filters xs) =
    req `addQuery` toQuery ("filters", Just xs)
-- | @application/json@
instance Produces ConfigList MimeJSON


-- *** configUpdate

-- | @POST \/configs\/{id}\/update@
-- 
-- Update a Config
-- 
configUpdate
  :: (Consumes ConfigUpdate contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Id -- ^ "id" -  The ID or name of the config
  -> VersionInteger -- ^ "version" -  The version number of the config object being updated. This is required to avoid conflicting writes. 
  -> DockerEngineRequest ConfigUpdate contentType NoContent MimeNoContent
configUpdate _ (Id id) (VersionInteger version) =
  _mkRequest "POST" ["/configs/",toPath id,"/update"]
    `addQuery` toQuery ("version", Just version)

data ConfigUpdate 

-- | /Body Param/ "body" - The spec of the config to update. Currently, only the Labels field can be updated. All other fields must remain unchanged from the [ConfigInspect endpoint](#operation/ConfigInspect) response values. 
instance HasBodyParam ConfigUpdate ConfigSpec 

-- | @application/json@
instance Consumes ConfigUpdate MimeJSON
-- | @text/plain@
instance Consumes ConfigUpdate MimePlainText

instance Produces ConfigUpdate MimeNoContent

