{-
   Docker Engine API

   The Engine API is an HTTP API served by Docker Engine. It is the API the Docker client uses to communicate with the Engine, so everything the Docker client can do can be done with the API.  Most of the client's commands map directly to API endpoints (e.g. `docker ps` is `GET /containers/json`). The notable exception is running containers, which consists of several API calls.  # Errors  The API uses standard HTTP status codes to indicate the success or failure of the API call. The body of the response will be JSON in the following format:  ``` {   \"message\": \"page not found\" } ```  # Versioning  The API is usually changed in each release, so API calls are versioned to ensure that clients don't break. To lock to a specific version of the API, you prefix the URL with its version, for example, call `/v1.30/info` to use the v1.30 version of the `/info` endpoint. If the API version specified in the URL is not supported by the daemon, a HTTP `400 Bad Request` error message is returned.  If you omit the version-prefix, the current version of the API (v1.39) is used. For example, calling `/info` is the same as calling `/v1.39/info`. Using the API without a version-prefix is deprecated and will be removed in a future release.  Engine releases in the near future should support this version of the API, so your client will continue to work even if it is talking to a newer Engine.  The API uses an open schema model, which means server may add extra properties to responses. Likewise, the server will ignore any extra query parameters and request body properties. When you write clients, you need to ignore additional properties in responses to ensure they do not break when talking to newer daemons.   # Authentication  Authentication for registries is handled client side. The client has to send authentication details to various endpoints that need to communicate with registries, such as `POST /images/(name)/push`. These are sent as `X-Registry-Auth` header as a [base64url encoded](https://tools.ietf.org/html/rfc4648#section-5) (JSON) string with the following structure:  ``` {   \"username\": \"string\",   \"password\": \"string\",   \"email\": \"string\",   \"serveraddress\": \"string\" } ```  The `serveraddress` is a domain/IP without a protocol. Throughout this structure, double quotes are required.  If you have already got an identity token from the [`/auth` endpoint](#operation/SystemAuth), you can just pass this instead of credentials:  ``` {   \"identitytoken\": \"9cbaf023786cd7...\" } ``` 

   OpenAPI Version: 3.0.1
   Docker Engine API API version: 1.39
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
  :: (Consumes SwarmInit contentType, MimeRender contentType SwarmInitRequest)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> SwarmInitRequest -- ^ "body"
  -> DockerEngineRequest SwarmInit contentType Text accept
swarmInit _  _ body =
  _mkRequest "POST" ["/swarm/init"]
    `setBodyParam` body

data SwarmInit 
instance HasBodyParam SwarmInit SwarmInitRequest 

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
  -> DockerEngineRequest SwarmInspect MimeNoContent Swarm accept
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
swarmJoin
  :: (Consumes SwarmJoin contentType, MimeRender contentType SwarmJoinRequest)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> SwarmJoinRequest -- ^ "body"
  -> DockerEngineRequest SwarmJoin contentType NoContent MimeNoContent
swarmJoin _ body =
  _mkRequest "POST" ["/swarm/join"]
    `setBodyParam` body

data SwarmJoin 
instance HasBodyParam SwarmJoin SwarmJoinRequest 

-- | @application/json@
instance Consumes SwarmJoin MimeJSON
-- | @text/plain@
instance Consumes SwarmJoin MimePlainText

instance Produces SwarmJoin MimeNoContent


-- *** swarmLeave

-- | @POST \/swarm\/leave@
-- 
-- Leave a swarm
-- 
swarmLeave
  :: DockerEngineRequest SwarmLeave MimeNoContent NoContent MimeNoContent
swarmLeave =
  _mkRequest "POST" ["/swarm/leave"]

data SwarmLeave  

-- | /Optional Param/ "force" - Force leave swarm, even if this is the last manager or that it will break the cluster. 
instance HasOptionalParam SwarmLeave Force where
  applyOptionalParam req (Force xs) =
    req `addQuery` toQuery ("force", Just xs)
instance Produces SwarmLeave MimeNoContent


-- *** swarmUnlock

-- | @POST \/swarm\/unlock@
-- 
-- Unlock a locked manager
-- 
swarmUnlock
  :: (Consumes SwarmUnlock MimeJSON, MimeRender MimeJSON SwarmUnlockRequest)
  => SwarmUnlockRequest -- ^ "body"
  -> DockerEngineRequest SwarmUnlock MimeJSON NoContent MimeNoContent
swarmUnlock body =
  _mkRequest "POST" ["/swarm/unlock"]
    `setBodyParam` body

data SwarmUnlock 
instance HasBodyParam SwarmUnlock SwarmUnlockRequest 

-- | @application/json@
instance Consumes SwarmUnlock MimeJSON

instance Produces SwarmUnlock MimeNoContent


-- *** swarmUnlockkey

-- | @GET \/swarm\/unlockkey@
-- 
-- Get the unlock key
-- 
swarmUnlockkey
  :: Accept accept -- ^ request accept ('MimeType')
  -> DockerEngineRequest SwarmUnlockkey MimeNoContent UnlockKeyResponse accept
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
swarmUpdate
  :: (Consumes SwarmUpdate contentType, MimeRender contentType SwarmSpec)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> SwarmSpec -- ^ "body"
  -> VersionInteger -- ^ "version" -  The version number of the swarm object being updated. This is required to avoid conflicting writes. 
  -> DockerEngineRequest SwarmUpdate contentType NoContent MimeNoContent
swarmUpdate _ body (VersionInteger version) =
  _mkRequest "POST" ["/swarm/update"]
    `setBodyParam` body
    `addQuery` toQuery ("version", Just version)

data SwarmUpdate 
instance HasBodyParam SwarmUpdate SwarmSpec 

-- | /Optional Param/ "rotateWorkerToken" - Rotate the worker join token.
instance HasOptionalParam SwarmUpdate RotateWorkerToken where
  applyOptionalParam req (RotateWorkerToken xs) =
    req `addQuery` toQuery ("rotateWorkerToken", Just xs)

-- | /Optional Param/ "rotateManagerToken" - Rotate the manager join token.
instance HasOptionalParam SwarmUpdate RotateManagerToken where
  applyOptionalParam req (RotateManagerToken xs) =
    req `addQuery` toQuery ("rotateManagerToken", Just xs)

-- | /Optional Param/ "rotateManagerUnlockKey" - Rotate the manager unlock key.
instance HasOptionalParam SwarmUpdate RotateManagerUnlockKey where
  applyOptionalParam req (RotateManagerUnlockKey xs) =
    req `addQuery` toQuery ("rotateManagerUnlockKey", Just xs)

-- | @application/json@
instance Consumes SwarmUpdate MimeJSON
-- | @text/plain@
instance Consumes SwarmUpdate MimePlainText

instance Produces SwarmUpdate MimeNoContent

