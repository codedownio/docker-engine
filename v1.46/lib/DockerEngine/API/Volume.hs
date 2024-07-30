{-
   Docker Engine API

   The Engine API is an HTTP API served by Docker Engine. It is the API the Docker client uses to communicate with the Engine, so everything the Docker client can do can be done with the API.  Most of the client's commands map directly to API endpoints (e.g. `docker ps` is `GET /containers/json`). The notable exception is running containers, which consists of several API calls.  # Errors  The API uses standard HTTP status codes to indicate the success or failure of the API call. The body of the response will be JSON in the following format:  ``` {   \"message\": \"page not found\" } ```  # Versioning  The API is usually changed in each release, so API calls are versioned to ensure that clients don't break. To lock to a specific version of the API, you prefix the URL with its version, for example, call `/v1.30/info` to use the v1.30 version of the `/info` endpoint. If the API version specified in the URL is not supported by the daemon, a HTTP `400 Bad Request` error message is returned.  If you omit the version-prefix, the current version of the API (v1.46) is used. For example, calling `/info` is the same as calling `/v1.46/info`. Using the API without a version-prefix is deprecated and will be removed in a future release.  Engine releases in the near future should support this version of the API, so your client will continue to work even if it is talking to a newer Engine.  The API uses an open schema model, which means server may add extra properties to responses. Likewise, the server will ignore any extra query parameters and request body properties. When you write clients, you need to ignore additional properties in responses to ensure they do not break when talking to newer daemons.   # Authentication  Authentication for registries is handled client side. The client has to send authentication details to various endpoints that need to communicate with registries, such as `POST /images/(name)/push`. These are sent as `X-Registry-Auth` header as a [base64url encoded](https://tools.ietf.org/html/rfc4648#section-5) (JSON) string with the following structure:  ``` {   \"username\": \"string\",   \"password\": \"string\",   \"email\": \"string\",   \"serveraddress\": \"string\" } ```  The `serveraddress` is a domain/IP without a protocol. Throughout this structure, double quotes are required.  If you have already got an identity token from the [`/auth` endpoint](#operation/SystemAuth), you can just pass this instead of credentials:  ``` {   \"identitytoken\": \"9cbaf023786cd7...\" } ``` 

   OpenAPI Version: 3.0.1
   Docker Engine API API version: 1.46
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : DockerEngine.API.Volume
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module DockerEngine.API.Volume where

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


-- ** Volume

-- *** volumeCreate

-- | @POST \/volumes\/create@
-- 
-- Create a volume
-- 
volumeCreate
  :: (Consumes VolumeCreate MimeJSON, MimeRender MimeJSON VolumeCreateOptions)
  => VolumeCreateOptions -- ^ "volumeConfig" -  Volume configuration
  -> DockerEngineRequest VolumeCreate MimeJSON Volume MimeJSON
volumeCreate volumeConfig =
  _mkRequest "POST" ["/volumes/create"]
    `setBodyParam` volumeConfig

data VolumeCreate 

-- | /Body Param/ "volumeConfig" - Volume configuration
instance HasBodyParam VolumeCreate VolumeCreateOptions 

-- | @application/json@
instance Consumes VolumeCreate MimeJSON

-- | @application/json@
instance Produces VolumeCreate MimeJSON


-- *** volumeDelete

-- | @DELETE \/volumes\/{name}@
-- 
-- Remove a volume
-- 
-- Instruct the driver to remove the volume.
-- 
volumeDelete
  :: Name -- ^ "name" -  Volume name or ID
  -> DockerEngineRequest VolumeDelete MimeNoContent NoContent MimeNoContent
volumeDelete (Name name) =
  _mkRequest "DELETE" ["/volumes/",toPath name]

data VolumeDelete  

-- | /Optional Param/ "force" - Force the removal of the volume
instance HasOptionalParam VolumeDelete Force where
  applyOptionalParam req (Force xs) =
    req `addQuery` toQuery ("force", Just xs)
instance Produces VolumeDelete MimeNoContent


-- *** volumeInspect

-- | @GET \/volumes\/{name}@
-- 
-- Inspect a volume
-- 
volumeInspect
  :: Name -- ^ "name" -  Volume name or ID
  -> DockerEngineRequest VolumeInspect MimeNoContent Volume MimeJSON
volumeInspect (Name name) =
  _mkRequest "GET" ["/volumes/",toPath name]

data VolumeInspect  
-- | @application/json@
instance Produces VolumeInspect MimeJSON


-- *** volumeList

-- | @GET \/volumes@
-- 
-- List volumes
-- 
volumeList
  :: DockerEngineRequest VolumeList MimeNoContent VolumeListResponse MimeJSON
volumeList =
  _mkRequest "GET" ["/volumes"]

data VolumeList  

-- | /Optional Param/ "filters" - JSON encoded value of the filters (a `map[string][]string`) to process on the volumes list. Available filters:  - `dangling=<boolean>` When set to `true` (or `1`), returns all    volumes that are not in use by a container. When set to `false`    (or `0`), only volumes that are in use by one or more    containers are returned. - `driver=<volume-driver-name>` Matches volumes based on their driver. - `label=<key>` or `label=<key>:<value>` Matches volumes based on    the presence of a `label` alone or a `label` and a value. - `name=<volume-name>` Matches all or part of a volume name. 
instance HasOptionalParam VolumeList Filters where
  applyOptionalParam req (Filters xs) =
    req `addQuery` toQuery ("filters", Just xs)
-- | @application/json@
instance Produces VolumeList MimeJSON


-- *** volumePrune

-- | @POST \/volumes\/prune@
-- 
-- Delete unused volumes
-- 
volumePrune
  :: DockerEngineRequest VolumePrune MimeNoContent VolumePruneResponse MimeJSON
volumePrune =
  _mkRequest "POST" ["/volumes/prune"]

data VolumePrune  

-- | /Optional Param/ "filters" - Filters to process on the prune list, encoded as JSON (a `map[string][]string`).  Available filters: - `label` (`label=<key>`, `label=<key>=<value>`, `label!=<key>`, or `label!=<key>=<value>`) Prune volumes with (or without, in case `label!=...` is used) the specified labels. - `all` (`all=true`) - Consider all (local) volumes for pruning and not just anonymous volumes. 
instance HasOptionalParam VolumePrune Filters where
  applyOptionalParam req (Filters xs) =
    req `addQuery` toQuery ("filters", Just xs)
-- | @application/json@
instance Produces VolumePrune MimeJSON


-- *** volumeUpdate

-- | @PUT \/volumes\/{name}@
-- 
-- \"Update a volume. Valid only for Swarm cluster volumes\" 
-- 
volumeUpdate
  :: (Consumes VolumeUpdate MimeJSON)
  => Name -- ^ "name" -  The name or ID of the volume
  -> VersionInteger -- ^ "version" -  The version number of the volume being updated. This is required to avoid conflicting writes. Found in the volume's `ClusterVolume` field. 
  -> DockerEngineRequest VolumeUpdate MimeJSON NoContent MimeNoContent
volumeUpdate (Name name) (VersionInteger version) =
  _mkRequest "PUT" ["/volumes/",toPath name]
    `addQuery` toQuery ("version", Just version)

data VolumeUpdate 

-- | /Body Param/ "body" - The spec of the volume to update. Currently, only Availability may change. All other fields must remain unchanged. 
instance HasBodyParam VolumeUpdate VolumeUpdateRequest 

-- | @application/json@
instance Consumes VolumeUpdate MimeJSON

instance Produces VolumeUpdate MimeNoContent

