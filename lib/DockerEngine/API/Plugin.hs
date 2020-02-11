{-
   Docker Engine API

   The Engine API is an HTTP API served by Docker Engine. It is the API the Docker client uses to communicate with the Engine, so everything the Docker client can do can be done with the API.  Most of the client's commands map directly to API endpoints (e.g. `docker ps` is `GET /containers/json`). The notable exception is running containers, which consists of several API calls.  # Errors  The API uses standard HTTP status codes to indicate the success or failure of the API call. The body of the response will be JSON in the following format:  ``` {   \"message\": \"page not found\" } ```  # Versioning  The API is usually changed in each release of Docker, so API calls are versioned to ensure that clients don't break.  For Docker Engine 17.06, the API version is 1.30. To lock to this version, you prefix the URL with `/v1.30`. For example, calling `/info` is the same as calling `/v1.30/info`.  Engine releases in the near future should support this version of the API, so your client will continue to work even if it is talking to a newer Engine.  In previous versions of Docker, it was possible to access the API without providing a version. This behaviour is now deprecated will be removed in a future version of Docker.  The API uses an open schema model, which means server may add extra properties to responses. Likewise, the server will ignore any extra query parameters and request body properties. When you write clients, you need to ignore additional properties in responses to ensure they do not break when talking to newer Docker daemons.  This documentation is for version 1.30 of the API, which was introduced with Docker 17.06. Use this table to find documentation for previous versions of the API:  Docker version  | API version | Changes ----------------|-------------|--------- 17.05.x | [1.29](https://docs.docker.com/engine/api/v1.29/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-29-api-changes) 17.04.x | [1.28](https://docs.docker.com/engine/api/v1.28/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-28-api-changes) 17.03.1 | [1.27](https://docs.docker.com/engine/api/v1.27/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-27-api-changes) 1.13.1 & 17.03.0 | [1.26](https://docs.docker.com/engine/api/v1.26/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-26-api-changes) 1.13.0 | [1.25](https://docs.docker.com/engine/api/v1.25/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-25-api-changes) 1.12.x | [1.24](https://docs.docker.com/engine/api/v1.24/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-24-api-changes) 1.11.x | [1.23](https://docs.docker.com/engine/api/v1.23/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-23-api-changes) 1.10.x | [1.22](https://docs.docker.com/engine/api/v1.22/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-22-api-changes) 1.9.x | [1.21](https://docs.docker.com/engine/api/v1.21/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-21-api-changes) 1.8.x | [1.20](https://docs.docker.com/engine/api/v1.20/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-20-api-changes) 1.7.x | [1.19](https://docs.docker.com/engine/api/v1.19/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-19-api-changes) 1.6.x | [1.18](https://docs.docker.com/engine/api/v1.18/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-18-api-changes)  # Authentication  Authentication for registries is handled client side. The client has to send authentication details to various endpoints that need to communicate with registries, such as `POST /images/(name)/push`. These are sent as `X-Registry-Auth` header as a Base64 encoded (JSON) string with the following structure:  ``` {   \"username\": \"string\",   \"password\": \"string\",   \"email\": \"string\",   \"serveraddress\": \"string\" } ```  The `serveraddress` is a domain/IP without a protocol. Throughout this structure, double quotes are required.  If you have already got an identity token from the [`/auth` endpoint](#operation/SystemAuth), you can just pass this instead of credentials:  ``` {   \"identitytoken\": \"9cbaf023786cd7...\" } ``` 

   OpenAPI Version: 3.0.1
   Docker Engine API API version: 1.30
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : DockerEngine.API.Plugin
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module DockerEngine.API.Plugin where

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


-- ** Plugin

-- *** getPluginPrivileges

-- | @GET \/plugins\/privileges@
-- 
-- Get plugin privileges
-- 
getPluginPrivileges 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Remote -- ^ "remote" -  The name of the plugin. The `:latest` tag is optional, and is the default if omitted.
  -> DockerEngineRequest GetPluginPrivileges MimeNoContent [InlineResponse20018] accept
getPluginPrivileges  _ (Remote remote) =
  _mkRequest "GET" ["/plugins/privileges"]
    `setQuery` toQuery ("remote", Just remote)

data GetPluginPrivileges  
-- | @application/json@
instance Produces GetPluginPrivileges MimeJSON
-- | @text/plain@
instance Produces GetPluginPrivileges MimePlainText


-- *** pluginCreate

-- | @POST \/plugins\/create@
-- 
-- Create a plugin
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
pluginCreate 
  :: (Consumes PluginCreate MimeXTar)
  => Accept accept -- ^ request accept ('MimeType')
  -> Name -- ^ "name" -  The name of the plugin. The `:latest` tag is optional, and is the default if omitted.
  -> DockerEngineRequest PluginCreate MimeXTar res accept
pluginCreate  _ (Name name) =
  _mkRequest "POST" ["/plugins/create"]
    `setQuery` toQuery ("name", Just name)

data PluginCreate 

-- | /Body Param/ "tarContext" - Path to tar containing plugin rootfs and manifest
instance HasBodyParam PluginCreate TarContext 

-- | @application/x-tar@
instance Consumes PluginCreate MimeXTar

-- | @application/json@
instance Produces PluginCreate MimeJSON
-- | @text/plain@
instance Produces PluginCreate MimePlainText


-- *** pluginDelete

-- | @DELETE \/plugins\/{name}@
-- 
-- Remove a plugin
-- 
pluginDelete 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Name -- ^ "name" -  The name of the plugin. The `:latest` tag is optional, and is the default if omitted.
  -> DockerEngineRequest PluginDelete MimeNoContent Plugin accept
pluginDelete  _ (Name name) =
  _mkRequest "DELETE" ["/plugins/",toPath name]

data PluginDelete  

-- | /Optional Param/ "force" - Disable the plugin before removing. This may result in issues if the plugin is in use by a container.
instance HasOptionalParam PluginDelete Force where
  applyOptionalParam req (Force xs) =
    req `setQuery` toQuery ("force", Just xs)
-- | @application/json@
instance Produces PluginDelete MimeJSON
-- | @text/plain@
instance Produces PluginDelete MimePlainText


-- *** pluginDisable

-- | @POST \/plugins\/{name}\/disable@
-- 
-- Disable a plugin
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
pluginDisable 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Name -- ^ "name" -  The name of the plugin. The `:latest` tag is optional, and is the default if omitted.
  -> DockerEngineRequest PluginDisable MimeNoContent res accept
pluginDisable  _ (Name name) =
  _mkRequest "POST" ["/plugins/",toPath name,"/disable"]

data PluginDisable  
-- | @application/json@
instance Produces PluginDisable MimeJSON
-- | @text/plain@
instance Produces PluginDisable MimePlainText


-- *** pluginEnable

-- | @POST \/plugins\/{name}\/enable@
-- 
-- Enable a plugin
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
pluginEnable 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Name -- ^ "name" -  The name of the plugin. The `:latest` tag is optional, and is the default if omitted.
  -> DockerEngineRequest PluginEnable MimeNoContent res accept
pluginEnable  _ (Name name) =
  _mkRequest "POST" ["/plugins/",toPath name,"/enable"]

data PluginEnable  

-- | /Optional Param/ "timeout" - Set the HTTP client timeout (in seconds)
instance HasOptionalParam PluginEnable Timeout where
  applyOptionalParam req (Timeout xs) =
    req `setQuery` toQuery ("timeout", Just xs)
-- | @application/json@
instance Produces PluginEnable MimeJSON
-- | @text/plain@
instance Produces PluginEnable MimePlainText


-- *** pluginInspect

-- | @GET \/plugins\/{name}\/json@
-- 
-- Inspect a plugin
-- 
pluginInspect 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Name -- ^ "name" -  The name of the plugin. The `:latest` tag is optional, and is the default if omitted.
  -> DockerEngineRequest PluginInspect MimeNoContent Plugin accept
pluginInspect  _ (Name name) =
  _mkRequest "GET" ["/plugins/",toPath name,"/json"]

data PluginInspect  
-- | @application/json@
instance Produces PluginInspect MimeJSON
-- | @text/plain@
instance Produces PluginInspect MimePlainText


-- *** pluginList

-- | @GET \/plugins@
-- 
-- List plugins
-- 
-- Returns information about installed plugins.
-- 
pluginList 
  :: DockerEngineRequest PluginList MimeNoContent [Plugin] MimeJSON
pluginList =
  _mkRequest "GET" ["/plugins"]

data PluginList  

-- | /Optional Param/ "filters" - A JSON encoded value of the filters (a `map[string][]string`) to process on the plugin list. Available filters:  - `capability=<capability name>` - `enable=<true>|<false>` 
instance HasOptionalParam PluginList Filters where
  applyOptionalParam req (Filters xs) =
    req `setQuery` toQuery ("filters", Just xs)
-- | @application/json@
instance Produces PluginList MimeJSON


-- *** pluginPull

-- | @POST \/plugins\/pull@
-- 
-- Install a plugin
-- 
-- Pulls and installs a plugin. After the plugin is installed, it can be enabled using the [`POST /plugins/{name}/enable` endpoint](#operation/PostPluginsEnable). 
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
pluginPull 
  :: (Consumes PluginPull contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Remote -- ^ "remote" -  Remote reference for plugin to install.  The `:latest` tag is optional, and is used as the default if omitted. 
  -> DockerEngineRequest PluginPull contentType res MimeJSON
pluginPull _ (Remote remote) =
  _mkRequest "POST" ["/plugins/pull"]
    `setQuery` toQuery ("remote", Just remote)

data PluginPull 
instance HasBodyParam PluginPull Body 

-- | /Optional Param/ "name" - Local name for the pulled plugin.  The `:latest` tag is optional, and is used as the default if omitted. 
instance HasOptionalParam PluginPull Name where
  applyOptionalParam req (Name xs) =
    req `setQuery` toQuery ("name", Just xs)

-- | /Optional Param/ "X-Registry-Auth" - A base64-encoded auth configuration to use when pulling a plugin from a registry. [See the authentication section for details.](#section/Authentication)
instance HasOptionalParam PluginPull XRegistryAuth where
  applyOptionalParam req (XRegistryAuth xs) =
    req `setHeader` toHeader ("X-Registry-Auth", xs)

-- | @application/json@
instance Consumes PluginPull MimeJSON
-- | @text/plain@
instance Consumes PluginPull MimePlainText

-- | @application/json@
instance Produces PluginPull MimeJSON


-- *** pluginPush

-- | @POST \/plugins\/{name}\/push@
-- 
-- Push a plugin
-- 
-- Push a plugin to the registry. 
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
pluginPush 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Name -- ^ "name" -  The name of the plugin. The `:latest` tag is optional, and is the default if omitted.
  -> DockerEngineRequest PluginPush MimeNoContent res accept
pluginPush  _ (Name name) =
  _mkRequest "POST" ["/plugins/",toPath name,"/push"]

data PluginPush  
-- | @application/json@
instance Produces PluginPush MimeJSON
-- | @text/plain@
instance Produces PluginPush MimePlainText


-- *** pluginSet

-- | @POST \/plugins\/{name}\/set@
-- 
-- Configure a plugin
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
pluginSet 
  :: (Consumes PluginSet MimeJSON)
  => Accept accept -- ^ request accept ('MimeType')
  -> Name -- ^ "name" -  The name of the plugin. The `:latest` tag is optional, and is the default if omitted.
  -> DockerEngineRequest PluginSet MimeJSON res accept
pluginSet  _ (Name name) =
  _mkRequest "POST" ["/plugins/",toPath name,"/set"]

data PluginSet 
instance HasBodyParam PluginSet BodyText 

-- | @application/json@
instance Consumes PluginSet MimeJSON

-- | @application/json@
instance Produces PluginSet MimeJSON
-- | @text/plain@
instance Produces PluginSet MimePlainText


-- *** pluginUpgrade

-- | @POST \/plugins\/{name}\/upgrade@
-- 
-- Upgrade a plugin
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
pluginUpgrade 
  :: (Consumes PluginUpgrade contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> Name -- ^ "name" -  The name of the plugin. The `:latest` tag is optional, and is the default if omitted.
  -> Remote -- ^ "remote" -  Remote reference to upgrade to.  The `:latest` tag is optional, and is used as the default if omitted. 
  -> DockerEngineRequest PluginUpgrade contentType res accept
pluginUpgrade _  _ (Name name) (Remote remote) =
  _mkRequest "POST" ["/plugins/",toPath name,"/upgrade"]
    `setQuery` toQuery ("remote", Just remote)

data PluginUpgrade 
instance HasBodyParam PluginUpgrade Body 

-- | /Optional Param/ "X-Registry-Auth" - A base64-encoded auth configuration to use when pulling a plugin from a registry. [See the authentication section for details.](#section/Authentication)
instance HasOptionalParam PluginUpgrade XRegistryAuth where
  applyOptionalParam req (XRegistryAuth xs) =
    req `setHeader` toHeader ("X-Registry-Auth", xs)

-- | @application/json@
instance Consumes PluginUpgrade MimeJSON
-- | @text/plain@
instance Consumes PluginUpgrade MimePlainText

-- | @application/json@
instance Produces PluginUpgrade MimeJSON
-- | @text/plain@
instance Produces PluginUpgrade MimePlainText

