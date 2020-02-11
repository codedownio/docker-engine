{-
   Docker Engine API

   The Engine API is an HTTP API served by Docker Engine. It is the API the Docker client uses to communicate with the Engine, so everything the Docker client can do can be done with the API.  Most of the client's commands map directly to API endpoints (e.g. `docker ps` is `GET /containers/json`). The notable exception is running containers, which consists of several API calls.  # Errors  The API uses standard HTTP status codes to indicate the success or failure of the API call. The body of the response will be JSON in the following format:  ``` {   \"message\": \"page not found\" } ```  # Versioning  The API is usually changed in each release of Docker, so API calls are versioned to ensure that clients don't break.  For Docker Engine 17.06, the API version is 1.30. To lock to this version, you prefix the URL with `/v1.30`. For example, calling `/info` is the same as calling `/v1.30/info`.  Engine releases in the near future should support this version of the API, so your client will continue to work even if it is talking to a newer Engine.  In previous versions of Docker, it was possible to access the API without providing a version. This behaviour is now deprecated will be removed in a future version of Docker.  The API uses an open schema model, which means server may add extra properties to responses. Likewise, the server will ignore any extra query parameters and request body properties. When you write clients, you need to ignore additional properties in responses to ensure they do not break when talking to newer Docker daemons.  This documentation is for version 1.30 of the API, which was introduced with Docker 17.06. Use this table to find documentation for previous versions of the API:  Docker version  | API version | Changes ----------------|-------------|--------- 17.05.x | [1.29](https://docs.docker.com/engine/api/v1.29/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-29-api-changes) 17.04.x | [1.28](https://docs.docker.com/engine/api/v1.28/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-28-api-changes) 17.03.1 | [1.27](https://docs.docker.com/engine/api/v1.27/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-27-api-changes) 1.13.1 & 17.03.0 | [1.26](https://docs.docker.com/engine/api/v1.26/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-26-api-changes) 1.13.0 | [1.25](https://docs.docker.com/engine/api/v1.25/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-25-api-changes) 1.12.x | [1.24](https://docs.docker.com/engine/api/v1.24/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-24-api-changes) 1.11.x | [1.23](https://docs.docker.com/engine/api/v1.23/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-23-api-changes) 1.10.x | [1.22](https://docs.docker.com/engine/api/v1.22/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-22-api-changes) 1.9.x | [1.21](https://docs.docker.com/engine/api/v1.21/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-21-api-changes) 1.8.x | [1.20](https://docs.docker.com/engine/api/v1.20/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-20-api-changes) 1.7.x | [1.19](https://docs.docker.com/engine/api/v1.19/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-19-api-changes) 1.6.x | [1.18](https://docs.docker.com/engine/api/v1.18/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-18-api-changes)  # Authentication  Authentication for registries is handled client side. The client has to send authentication details to various endpoints that need to communicate with registries, such as `POST /images/(name)/push`. These are sent as `X-Registry-Auth` header as a Base64 encoded (JSON) string with the following structure:  ``` {   \"username\": \"string\",   \"password\": \"string\",   \"email\": \"string\",   \"serveraddress\": \"string\" } ```  The `serveraddress` is a domain/IP without a protocol. Throughout this structure, double quotes are required.  If you have already got an identity token from the [`/auth` endpoint](#operation/SystemAuth), you can just pass this instead of credentials:  ``` {   \"identitytoken\": \"9cbaf023786cd7...\" } ``` 

   OpenAPI Version: 3.0.1
   Docker Engine API API version: 1.30
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : DockerEngine.API.Swarm
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module DockerEngine.API.Swarm where

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


-- ** Swarm

-- *** swarmInit

-- | @POST \/swarm\/init@
-- 
-- Initialize a new swarm
-- 
swarmInit 
  :: (Consumes SwarmInit contentType, MimeRender contentType InlineObject6)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> InlineObject6 -- ^ "body"
  -> DockerEngineRequest SwarmInit contentType Text accept
swarmInit _  _ body =
  _mkRequest "POST" ["/swarm/init"]
    `setBodyParam` body

data SwarmInit 
instance HasBodyParam SwarmInit InlineObject6 

-- | @application/json@
instance Consumes SwarmInit MimeJSON
-- | @text/plain@
instance Consumes SwarmInit MimePlainText

-- | @application/json@
instance Produces SwarmInit MimeJSON
-- | @text/plain@
instance Produces SwarmInit MimePlainText


-- *** swarmInspect

-- | @GET \/swarm@
-- 
-- Inspect swarm
-- 
swarmInspect 
  :: Accept accept -- ^ request accept ('MimeType')
  -> DockerEngineRequest SwarmInspect MimeNoContent InlineResponse20019 accept
swarmInspect  _ =
  _mkRequest "GET" ["/swarm"]

data SwarmInspect  
-- | @application/json@
instance Produces SwarmInspect MimeJSON
-- | @text/plain@
instance Produces SwarmInspect MimePlainText


-- *** swarmJoin

-- | @POST \/swarm\/join@
-- 
-- Join an existing swarm
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
swarmJoin 
  :: (Consumes SwarmJoin contentType, MimeRender contentType InlineObject7)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> InlineObject7 -- ^ "body"
  -> DockerEngineRequest SwarmJoin contentType res accept
swarmJoin _  _ body =
  _mkRequest "POST" ["/swarm/join"]
    `setBodyParam` body

data SwarmJoin 
instance HasBodyParam SwarmJoin InlineObject7 

-- | @application/json@
instance Consumes SwarmJoin MimeJSON
-- | @text/plain@
instance Consumes SwarmJoin MimePlainText

-- | @application/json@
instance Produces SwarmJoin MimeJSON
-- | @text/plain@
instance Produces SwarmJoin MimePlainText


-- *** swarmLeave

-- | @POST \/swarm\/leave@
-- 
-- Leave a swarm
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
swarmLeave 
  :: Accept accept -- ^ request accept ('MimeType')
  -> DockerEngineRequest SwarmLeave MimeNoContent res accept
swarmLeave  _ =
  _mkRequest "POST" ["/swarm/leave"]

data SwarmLeave  

-- | /Optional Param/ "force" - Force leave swarm, even if this is the last manager or that it will break the cluster.
instance HasOptionalParam SwarmLeave Force where
  applyOptionalParam req (Force xs) =
    req `setQuery` toQuery ("force", Just xs)
-- | @application/json@
instance Produces SwarmLeave MimeJSON
-- | @text/plain@
instance Produces SwarmLeave MimePlainText


-- *** swarmUnlock

-- | @POST \/swarm\/unlock@
-- 
-- Unlock a locked manager
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
swarmUnlock 
  :: (Consumes SwarmUnlock MimeJSON, MimeRender MimeJSON InlineObject8)
  => InlineObject8 -- ^ "body"
  -> DockerEngineRequest SwarmUnlock MimeJSON res MimeJSON
swarmUnlock body =
  _mkRequest "POST" ["/swarm/unlock"]
    `setBodyParam` body

data SwarmUnlock 
instance HasBodyParam SwarmUnlock InlineObject8 

-- | @application/json@
instance Consumes SwarmUnlock MimeJSON

-- | @application/json@
instance Produces SwarmUnlock MimeJSON


-- *** swarmUnlockkey

-- | @GET \/swarm\/unlockkey@
-- 
-- Get the unlock key
-- 
swarmUnlockkey 
  :: Accept accept -- ^ request accept ('MimeType')
  -> DockerEngineRequest SwarmUnlockkey MimeNoContent InlineResponse20020 accept
swarmUnlockkey  _ =
  _mkRequest "GET" ["/swarm/unlockkey"]

data SwarmUnlockkey  
-- | @application/json@
instance Produces SwarmUnlockkey MimeJSON
-- | @text/plain@
instance Produces SwarmUnlockkey MimePlainText


-- *** swarmUpdate

-- | @POST \/swarm\/update@
-- 
-- Update a swarm
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
swarmUpdate 
  :: (Consumes SwarmUpdate contentType, MimeRender contentType SwarmSpec)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> SwarmSpec -- ^ "body"
  -> Version -- ^ "version" -  The version number of the swarm object being updated. This is required to avoid conflicting writes.
  -> DockerEngineRequest SwarmUpdate contentType res accept
swarmUpdate _  _ body (Version version) =
  _mkRequest "POST" ["/swarm/update"]
    `setBodyParam` body
    `setQuery` toQuery ("version", Just version)

data SwarmUpdate 
instance HasBodyParam SwarmUpdate SwarmSpec 

-- | /Optional Param/ "rotateWorkerToken" - Rotate the worker join token.
instance HasOptionalParam SwarmUpdate RotateWorkerToken where
  applyOptionalParam req (RotateWorkerToken xs) =
    req `setQuery` toQuery ("rotateWorkerToken", Just xs)

-- | /Optional Param/ "rotateManagerToken" - Rotate the manager join token.
instance HasOptionalParam SwarmUpdate RotateManagerToken where
  applyOptionalParam req (RotateManagerToken xs) =
    req `setQuery` toQuery ("rotateManagerToken", Just xs)

-- | /Optional Param/ "rotateManagerUnlockKey" - Rotate the manager unlock key.
instance HasOptionalParam SwarmUpdate RotateManagerUnlockKey where
  applyOptionalParam req (RotateManagerUnlockKey xs) =
    req `setQuery` toQuery ("rotateManagerUnlockKey", Just xs)

-- | @application/json@
instance Consumes SwarmUpdate MimeJSON
-- | @text/plain@
instance Consumes SwarmUpdate MimePlainText

-- | @application/json@
instance Produces SwarmUpdate MimeJSON
-- | @text/plain@
instance Produces SwarmUpdate MimePlainText

