{-
   Docker Engine API

   The Engine API is an HTTP API served by Docker Engine. It is the API the Docker client uses to communicate with the Engine, so everything the Docker client can do can be done with the API.  Most of the client's commands map directly to API endpoints (e.g. `docker ps` is `GET /containers/json`). The notable exception is running containers, which consists of several API calls.  # Errors  The API uses standard HTTP status codes to indicate the success or failure of the API call. The body of the response will be JSON in the following format:  ``` {   \"message\": \"page not found\" } ```  # Versioning  The API is usually changed in each release, so API calls are versioned to ensure that clients don't break. To lock to a specific version of the API, you prefix the URL with its version, for example, call `/v1.30/info` to use the v1.30 version of the `/info` endpoint. If the API version specified in the URL is not supported by the daemon, a HTTP `400 Bad Request` error message is returned.  If you omit the version-prefix, the current version of the API (v1.46) is used. For example, calling `/info` is the same as calling `/v1.46/info`. Using the API without a version-prefix is deprecated and will be removed in a future release.  Engine releases in the near future should support this version of the API, so your client will continue to work even if it is talking to a newer Engine.  The API uses an open schema model, which means server may add extra properties to responses. Likewise, the server will ignore any extra query parameters and request body properties. When you write clients, you need to ignore additional properties in responses to ensure they do not break when talking to newer daemons.   # Authentication  Authentication for registries is handled client side. The client has to send authentication details to various endpoints that need to communicate with registries, such as `POST /images/(name)/push`. These are sent as `X-Registry-Auth` header as a [base64url encoded](https://tools.ietf.org/html/rfc4648#section-5) (JSON) string with the following structure:  ``` {   \"username\": \"string\",   \"password\": \"string\",   \"email\": \"string\",   \"serveraddress\": \"string\" } ```  The `serveraddress` is a domain/IP without a protocol. Throughout this structure, double quotes are required.  If you have already got an identity token from the [`/auth` endpoint](#operation/SystemAuth), you can just pass this instead of credentials:  ``` {   \"identitytoken\": \"9cbaf023786cd7...\" } ``` 

   OpenAPI Version: 3.0.1
   Docker Engine API API version: 1.46
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : DockerEngine.API.Exec
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module DockerEngine.API.Exec where

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


-- ** Exec

-- *** containerExec

-- | @POST \/containers\/{id}\/exec@
-- 
-- Create an exec instance
-- 
-- Run a command inside a running container.
-- 
containerExec
  :: (Consumes ContainerExec MimeJSON, MimeRender MimeJSON ExecConfig)
  => ExecConfig -- ^ "execConfig" -  Exec configuration
  -> Id -- ^ "id" -  ID or name of container
  -> DockerEngineRequest ContainerExec MimeJSON IdResponse MimeJSON
containerExec execConfig (Id id) =
  _mkRequest "POST" ["/containers/",toPath id,"/exec"]
    `setBodyParam` execConfig

data ContainerExec 

-- | /Body Param/ "execConfig" - Exec configuration
instance HasBodyParam ContainerExec ExecConfig 

-- | @application/json@
instance Consumes ContainerExec MimeJSON

-- | @application/json@
instance Produces ContainerExec MimeJSON


-- *** execInspect

-- | @GET \/exec\/{id}\/json@
-- 
-- Inspect an exec instance
-- 
-- Return low-level information about an exec instance.
-- 
execInspect
  :: Id -- ^ "id" -  Exec instance ID
  -> DockerEngineRequest ExecInspect MimeNoContent ExecInspectResponse MimeJSON
execInspect (Id id) =
  _mkRequest "GET" ["/exec/",toPath id,"/json"]

data ExecInspect  
-- | @application/json@
instance Produces ExecInspect MimeJSON


-- *** execResize

-- | @POST \/exec\/{id}\/resize@
-- 
-- Resize an exec instance
-- 
-- Resize the TTY session used by an exec instance. This endpoint only works if `tty` was specified as part of creating and starting the exec instance. 
-- 
execResize
  :: Id -- ^ "id" -  Exec instance ID
  -> DockerEngineRequest ExecResize MimeNoContent NoContent MimeNoContent
execResize (Id id) =
  _mkRequest "POST" ["/exec/",toPath id,"/resize"]

data ExecResize  

-- | /Optional Param/ "h" - Height of the TTY session in characters
instance HasOptionalParam ExecResize H where
  applyOptionalParam req (H xs) =
    req `addQuery` toQuery ("h", Just xs)

-- | /Optional Param/ "w" - Width of the TTY session in characters
instance HasOptionalParam ExecResize W where
  applyOptionalParam req (W xs) =
    req `addQuery` toQuery ("w", Just xs)
instance Produces ExecResize MimeNoContent


-- *** execStart

-- | @POST \/exec\/{id}\/start@
-- 
-- Start an exec instance
-- 
-- Starts a previously set up exec instance. If detach is true, this endpoint returns immediately after starting the command. Otherwise, it sets up an interactive session with the command. 
-- 
execStart
  :: (Consumes ExecStart MimeJSON)
  => Id -- ^ "id" -  Exec instance ID
  -> DockerEngineRequest ExecStart MimeJSON NoContent MimeNoContent
execStart (Id id) =
  _mkRequest "POST" ["/exec/",toPath id,"/start"]

data ExecStart 
instance HasBodyParam ExecStart ExecStartConfig 

-- | @application/json@
instance Consumes ExecStart MimeJSON

instance Produces ExecStart MimeNoContent

