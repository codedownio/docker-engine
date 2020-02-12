{-
   Docker Engine API

   The Engine API is an HTTP API served by Docker Engine. It is the API the Docker client uses to communicate with the Engine, so everything the Docker client can do can be done with the API.  Most of the client's commands map directly to API endpoints (e.g. `docker ps` is `GET /containers/json`). The notable exception is running containers, which consists of several API calls.  # Errors  The API uses standard HTTP status codes to indicate the success or failure of the API call. The body of the response will be JSON in the following format:  ``` {   \"message\": \"page not found\" } ```  # Versioning  The API is usually changed in each release, so API calls are versioned to ensure that clients don't break. To lock to a specific version of the API, you prefix the URL with its version, for example, call `/v1.30/info` to use the v1.30 version of the `/info` endpoint. If the API version specified in the URL is not supported by the daemon, a HTTP `400 Bad Request` error message is returned.  If you omit the version-prefix, the current version of the API (v1.36) is used. For example, calling `/info` is the same as calling `/v1.36/info`. Using the API without a version-prefix is deprecated and will be removed in a future release.  Engine releases in the near future should support this version of the API, so your client will continue to work even if it is talking to a newer Engine.  The API uses an open schema model, which means server may add extra properties to responses. Likewise, the server will ignore any extra query parameters and request body properties. When you write clients, you need to ignore additional properties in responses to ensure they do not break when talking to newer daemons.   # Authentication  Authentication for registries is handled client side. The client has to send authentication details to various endpoints that need to communicate with registries, such as `POST /images/(name)/push`. These are sent as `X-Registry-Auth` header as a Base64 encoded (JSON) string with the following structure:  ``` {   \"username\": \"string\",   \"password\": \"string\",   \"email\": \"string\",   \"serveraddress\": \"string\" } ```  The `serveraddress` is a domain/IP without a protocol. Throughout this structure, double quotes are required.  If you have already got an identity token from the [`/auth` endpoint](#operation/SystemAuth), you can just pass this instead of credentials:  ``` {   \"identitytoken\": \"9cbaf023786cd7...\" } ``` 

   OpenAPI Version: 3.0.1
   Docker Engine API API version: 1.36
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : DockerEngine.API.Node
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module DockerEngine.API.Node where

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


-- ** Node

-- *** nodeDelete

-- | @DELETE \/nodes\/{id}@
-- 
-- Delete a node
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
nodeDelete 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  The ID or name of the node
  -> DockerEngineRequest NodeDelete MimeNoContent res accept
nodeDelete  _ (Id id) =
  _mkRequest "DELETE" ["/nodes/",toPath id]

data NodeDelete  

-- | /Optional Param/ "force" - Force remove a node from the swarm
instance HasOptionalParam NodeDelete Force where
  applyOptionalParam req (Force xs) =
    req `setQuery` toQuery ("force", Just xs)
-- | @application/json@
instance Produces NodeDelete MimeJSON
-- | @text/plain@
instance Produces NodeDelete MimePlainText


-- *** nodeInspect

-- | @GET \/nodes\/{id}@
-- 
-- Inspect a node
-- 
nodeInspect 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  The ID or name of the node
  -> DockerEngineRequest NodeInspect MimeNoContent Node accept
nodeInspect  _ (Id id) =
  _mkRequest "GET" ["/nodes/",toPath id]

data NodeInspect  
-- | @application/json@
instance Produces NodeInspect MimeJSON
-- | @text/plain@
instance Produces NodeInspect MimePlainText


-- *** nodeList

-- | @GET \/nodes@
-- 
-- List nodes
-- 
nodeList 
  :: Accept accept -- ^ request accept ('MimeType')
  -> DockerEngineRequest NodeList MimeNoContent [Node] accept
nodeList  _ =
  _mkRequest "GET" ["/nodes"]

data NodeList  

-- | /Optional Param/ "filters" - Filters to process on the nodes list, encoded as JSON (a `map[string][]string`).  Available filters: - `id=<node id>` - `label=<engine label>` - `membership=`(`accepted`|`pending`)` - `name=<node name>` - `role=`(`manager`|`worker`)` 
instance HasOptionalParam NodeList Filters where
  applyOptionalParam req (Filters xs) =
    req `setQuery` toQuery ("filters", Just xs)
-- | @application/json@
instance Produces NodeList MimeJSON
-- | @text/plain@
instance Produces NodeList MimePlainText


-- *** nodeUpdate

-- | @POST \/nodes\/{id}\/update@
-- 
-- Update a node
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
nodeUpdate 
  :: (Consumes NodeUpdate contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  The ID of the node
  -> Version -- ^ "version" -  The version number of the node object being updated. This is required to avoid conflicting writes.
  -> DockerEngineRequest NodeUpdate contentType res accept
nodeUpdate _  _ (Id id) (Version version) =
  _mkRequest "POST" ["/nodes/",toPath id,"/update"]
    `setQuery` toQuery ("version", Just version)

data NodeUpdate 
instance HasBodyParam NodeUpdate NodeSpec 

-- | @application/json@
instance Consumes NodeUpdate MimeJSON
-- | @text/plain@
instance Consumes NodeUpdate MimePlainText

-- | @application/json@
instance Produces NodeUpdate MimeJSON
-- | @text/plain@
instance Produces NodeUpdate MimePlainText

