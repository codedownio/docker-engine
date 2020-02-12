{-
   Docker Engine API

   The Engine API is an HTTP API served by Docker Engine. It is the API the Docker client uses to communicate with the Engine, so everything the Docker client can do can be done with the API.  Most of the client's commands map directly to API endpoints (e.g. `docker ps` is `GET /containers/json`). The notable exception is running containers, which consists of several API calls.  # Errors  The API uses standard HTTP status codes to indicate the success or failure of the API call. The body of the response will be JSON in the following format:  ``` {   \"message\": \"page not found\" } ```  # Versioning  The API is usually changed in each release of Docker, so API calls are versioned to ensure that clients don't break.  For Docker Engine 17.06, the API version is 1.30. To lock to this version, you prefix the URL with `/v1.30`. For example, calling `/info` is the same as calling `/v1.30/info`.  Engine releases in the near future should support this version of the API, so your client will continue to work even if it is talking to a newer Engine.  In previous versions of Docker, it was possible to access the API without providing a version. This behaviour is now deprecated will be removed in a future version of Docker.  The API uses an open schema model, which means server may add extra properties to responses. Likewise, the server will ignore any extra query parameters and request body properties. When you write clients, you need to ignore additional properties in responses to ensure they do not break when talking to newer Docker daemons.  This documentation is for version 1.30 of the API, which was introduced with Docker 17.06. Use this table to find documentation for previous versions of the API:  Docker version  | API version | Changes ----------------|-------------|--------- 17.05.x | [1.29](https://docs.docker.com/engine/api/v1.29/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-29-api-changes) 17.04.x | [1.28](https://docs.docker.com/engine/api/v1.28/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-28-api-changes) 17.03.1 | [1.27](https://docs.docker.com/engine/api/v1.27/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-27-api-changes) 1.13.1 & 17.03.0 | [1.26](https://docs.docker.com/engine/api/v1.26/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-26-api-changes) 1.13.0 | [1.25](https://docs.docker.com/engine/api/v1.25/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-25-api-changes) 1.12.x | [1.24](https://docs.docker.com/engine/api/v1.24/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-24-api-changes) 1.11.x | [1.23](https://docs.docker.com/engine/api/v1.23/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-23-api-changes) 1.10.x | [1.22](https://docs.docker.com/engine/api/v1.22/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-22-api-changes) 1.9.x | [1.21](https://docs.docker.com/engine/api/v1.21/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-21-api-changes) 1.8.x | [1.20](https://docs.docker.com/engine/api/v1.20/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-20-api-changes) 1.7.x | [1.19](https://docs.docker.com/engine/api/v1.19/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-19-api-changes) 1.6.x | [1.18](https://docs.docker.com/engine/api/v1.18/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-18-api-changes)  # Authentication  Authentication for registries is handled client side. The client has to send authentication details to various endpoints that need to communicate with registries, such as `POST /images/(name)/push`. These are sent as `X-Registry-Auth` header as a Base64 encoded (JSON) string with the following structure:  ``` {   \"username\": \"string\",   \"password\": \"string\",   \"email\": \"string\",   \"serveraddress\": \"string\" } ```  The `serveraddress` is a domain/IP without a protocol. Throughout this structure, double quotes are required.  If you have already got an identity token from the [`/auth` endpoint](#operation/SystemAuth), you can just pass this instead of credentials:  ``` {   \"identitytoken\": \"9cbaf023786cd7...\" } ``` 

   OpenAPI Version: 3.0.1
   Docker Engine API API version: 1.30
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : DockerEngine.API.Image
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module DockerEngine.API.Image where

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


-- ** Image

-- *** imageBuild

-- | @POST \/build@
-- 
-- Build an image
-- 
-- Build an image from a tar archive with a `Dockerfile` in it.  The `Dockerfile` specifies how the image is built from the tar archive. It is typically in the archive's root, but can be at a different path or have a different name by specifying the `dockerfile` parameter. [See the `Dockerfile` reference for more information](https://docs.docker.com/engine/reference/builder/).  The Docker daemon performs a preliminary validation of the `Dockerfile` before starting the build, and returns an error if the syntax is incorrect. After that, each instruction is run one-by-one until the ID of the new image is output.  The build is canceled if the client drops the connection by quitting or being killed. 
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
imageBuild 
  :: (Consumes ImageBuild MimeOctetStream)
  => DockerEngineRequest ImageBuild MimeOctetStream res MimeJSON
imageBuild =
  _mkRequest "POST" ["/build"]

data ImageBuild 

-- | /Body Param/ "inputStream" - A tar archive compressed with one of the following algorithms: identity (no compression), gzip, bzip2, xz.
instance HasBodyParam ImageBuild InputStreamFilePath 

-- | /Optional Param/ "dockerfile" - Path within the build context to the `Dockerfile`. This is ignored if `remote` is specified and points to an external `Dockerfile`.
instance HasOptionalParam ImageBuild Dockerfile where
  applyOptionalParam req (Dockerfile xs) =
    req `setQuery` toQuery ("dockerfile", Just xs)

-- | /Optional Param/ "t" - A name and optional tag to apply to the image in the `name:tag` format. If you omit the tag the default `latest` value is assumed. You can provide several `t` parameters.
instance HasOptionalParam ImageBuild TText where
  applyOptionalParam req (TText xs) =
    req `setQuery` toQuery ("t", Just xs)

-- | /Optional Param/ "extrahosts" - Extra hosts to add to /etc/hosts
instance HasOptionalParam ImageBuild Extrahosts where
  applyOptionalParam req (Extrahosts xs) =
    req `setQuery` toQuery ("extrahosts", Just xs)

-- | /Optional Param/ "remote" - A Git repository URI or HTTP/HTTPS context URI. If the URI points to a single text file, the file’s contents are placed into a file called `Dockerfile` and the image is built from that file. If the URI points to a tarball, the file is downloaded by the daemon and the contents therein used as the context for the build. If the URI points to a tarball and the `dockerfile` parameter is also specified, there must be a file with the corresponding path inside the tarball.
instance HasOptionalParam ImageBuild Remote where
  applyOptionalParam req (Remote xs) =
    req `setQuery` toQuery ("remote", Just xs)

-- | /Optional Param/ "q" - Suppress verbose build output.
instance HasOptionalParam ImageBuild Q where
  applyOptionalParam req (Q xs) =
    req `setQuery` toQuery ("q", Just xs)

-- | /Optional Param/ "nocache" - Do not use the cache when building the image.
instance HasOptionalParam ImageBuild Nocache where
  applyOptionalParam req (Nocache xs) =
    req `setQuery` toQuery ("nocache", Just xs)

-- | /Optional Param/ "cachefrom" - JSON array of images used for build cache resolution.
instance HasOptionalParam ImageBuild Cachefrom where
  applyOptionalParam req (Cachefrom xs) =
    req `setQuery` toQuery ("cachefrom", Just xs)

-- | /Optional Param/ "pull" - Attempt to pull the image even if an older image exists locally.
instance HasOptionalParam ImageBuild Pull where
  applyOptionalParam req (Pull xs) =
    req `setQuery` toQuery ("pull", Just xs)

-- | /Optional Param/ "rm" - Remove intermediate containers after a successful build.
instance HasOptionalParam ImageBuild Rm where
  applyOptionalParam req (Rm xs) =
    req `setQuery` toQuery ("rm", Just xs)

-- | /Optional Param/ "forcerm" - Always remove intermediate containers, even upon failure.
instance HasOptionalParam ImageBuild Forcerm where
  applyOptionalParam req (Forcerm xs) =
    req `setQuery` toQuery ("forcerm", Just xs)

-- | /Optional Param/ "memory" - Set memory limit for build.
instance HasOptionalParam ImageBuild Memory where
  applyOptionalParam req (Memory xs) =
    req `setQuery` toQuery ("memory", Just xs)

-- | /Optional Param/ "memswap" - Total memory (memory + swap). Set as `-1` to disable swap.
instance HasOptionalParam ImageBuild Memswap where
  applyOptionalParam req (Memswap xs) =
    req `setQuery` toQuery ("memswap", Just xs)

-- | /Optional Param/ "cpushares" - CPU shares (relative weight).
instance HasOptionalParam ImageBuild Cpushares where
  applyOptionalParam req (Cpushares xs) =
    req `setQuery` toQuery ("cpushares", Just xs)

-- | /Optional Param/ "cpusetcpus" - CPUs in which to allow execution (e.g., `0-3`, `0,1`).
instance HasOptionalParam ImageBuild Cpusetcpus where
  applyOptionalParam req (Cpusetcpus xs) =
    req `setQuery` toQuery ("cpusetcpus", Just xs)

-- | /Optional Param/ "cpuperiod" - The length of a CPU period in microseconds.
instance HasOptionalParam ImageBuild Cpuperiod where
  applyOptionalParam req (Cpuperiod xs) =
    req `setQuery` toQuery ("cpuperiod", Just xs)

-- | /Optional Param/ "cpuquota" - Microseconds of CPU time that the container can get in a CPU period.
instance HasOptionalParam ImageBuild Cpuquota where
  applyOptionalParam req (Cpuquota xs) =
    req `setQuery` toQuery ("cpuquota", Just xs)

-- | /Optional Param/ "buildargs" - JSON map of string pairs for build-time variables. Users pass these values at build-time. Docker uses the buildargs as the environment context for commands run via the `Dockerfile` RUN instruction, or for variable expansion in other `Dockerfile` instructions. This is not meant for passing secret values. [Read more about the buildargs instruction.](https://docs.docker.com/engine/reference/builder/#arg)
instance HasOptionalParam ImageBuild Buildargs where
  applyOptionalParam req (Buildargs xs) =
    req `setQuery` toQuery ("buildargs", Just xs)

-- | /Optional Param/ "shmsize" - Size of `/dev/shm` in bytes. The size must be greater than 0. If omitted the system uses 64MB.
instance HasOptionalParam ImageBuild Shmsize where
  applyOptionalParam req (Shmsize xs) =
    req `setQuery` toQuery ("shmsize", Just xs)

-- | /Optional Param/ "squash" - Squash the resulting images layers into a single layer. *(Experimental release only.)*
instance HasOptionalParam ImageBuild Squash where
  applyOptionalParam req (Squash xs) =
    req `setQuery` toQuery ("squash", Just xs)

-- | /Optional Param/ "labels" - Arbitrary key/value labels to set on the image, as a JSON map of string pairs.
instance HasOptionalParam ImageBuild Labels where
  applyOptionalParam req (Labels xs) =
    req `setQuery` toQuery ("labels", Just xs)

-- | /Optional Param/ "networkmode" - Sets the networking mode for the run commands during build. Supported standard values are: `bridge`, `host`, `none`, and `container:<name|id>`. Any other value is taken as a custom network's name to which this container should connect to.
instance HasOptionalParam ImageBuild Networkmode where
  applyOptionalParam req (Networkmode xs) =
    req `setQuery` toQuery ("networkmode", Just xs)
instance HasOptionalParam ImageBuild ParamContentType where
  applyOptionalParam req (ParamContentType xs) =
    req `setHeader` toHeader ("Content-type", xs)

-- | /Optional Param/ "X-Registry-Config" - This is a base64-encoded JSON object with auth configurations for multiple registries that a build may refer to.  The key is a registry URL, and the value is an auth configuration object, [as described in the authentication section](#section/Authentication). For example:  ``` {   \"docker.example.com\": {     \"username\": \"janedoe\",     \"password\": \"hunter2\"   },   \"https://index.docker.io/v1/\": {     \"username\": \"mobydock\",     \"password\": \"conta1n3rize14\"   } } ```  Only the registry domain name (and port if not the default 443) are required. However, for legacy reasons, the Docker Hub registry must be specified with both a `https://` prefix and a `/v1/` suffix even though Docker will prefer to use the v2 registry API. 
instance HasOptionalParam ImageBuild XRegistryConfig where
  applyOptionalParam req (XRegistryConfig xs) =
    req `setHeader` toHeader ("X-Registry-Config", xs)

-- | @application/octet-stream@
instance Consumes ImageBuild MimeOctetStream

-- | @application/json@
instance Produces ImageBuild MimeJSON


-- *** imageCommit

-- | @POST \/commit@
-- 
-- Create a new image from a container
-- 
imageCommit 
  :: (Consumes ImageCommit MimeJSON)
  => DockerEngineRequest ImageCommit MimeJSON IdResponse MimeJSON
imageCommit =
  _mkRequest "POST" ["/commit"]

data ImageCommit 

-- | /Body Param/ "containerConfig" - The container configuration
instance HasBodyParam ImageCommit ContainerConfig 

-- | /Optional Param/ "container" - The ID or name of the container to commit
instance HasOptionalParam ImageCommit Container where
  applyOptionalParam req (Container xs) =
    req `setQuery` toQuery ("container", Just xs)

-- | /Optional Param/ "repo" - Repository name for the created image
instance HasOptionalParam ImageCommit Repo where
  applyOptionalParam req (Repo xs) =
    req `setQuery` toQuery ("repo", Just xs)

-- | /Optional Param/ "tag" - Tag name for the create image
instance HasOptionalParam ImageCommit Tag where
  applyOptionalParam req (Tag xs) =
    req `setQuery` toQuery ("tag", Just xs)

-- | /Optional Param/ "comment" - Commit message
instance HasOptionalParam ImageCommit Comment where
  applyOptionalParam req (Comment xs) =
    req `setQuery` toQuery ("comment", Just xs)

-- | /Optional Param/ "author" - Author of the image (e.g., `John Hannibal Smith <hannibal@a-team.com>`)
instance HasOptionalParam ImageCommit Author where
  applyOptionalParam req (Author xs) =
    req `setQuery` toQuery ("author", Just xs)

-- | /Optional Param/ "pause" - Whether to pause the container before committing
instance HasOptionalParam ImageCommit Pause where
  applyOptionalParam req (Pause xs) =
    req `setQuery` toQuery ("pause", Just xs)

-- | /Optional Param/ "changes" - `Dockerfile` instructions to apply while committing
instance HasOptionalParam ImageCommit Changes where
  applyOptionalParam req (Changes xs) =
    req `setQuery` toQuery ("changes", Just xs)

-- | @application/json@
instance Consumes ImageCommit MimeJSON

-- | @application/json@
instance Produces ImageCommit MimeJSON


-- *** imageCreate

-- | @POST \/images\/create@
-- 
-- Create an image
-- 
-- Create an image by either pulling it from a registry or importing it.
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
imageCreate 
  :: (Consumes ImageCreate contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> DockerEngineRequest ImageCreate contentType res MimeJSON
imageCreate _ =
  _mkRequest "POST" ["/images/create"]

data ImageCreate 

-- | /Body Param/ "inputImage" - Image content if the value `-` has been specified in fromSrc query parameter
instance HasBodyParam ImageCreate InputImage 

-- | /Optional Param/ "fromImage" - Name of the image to pull. The name may include a tag or digest. This parameter may only be used when pulling an image. The pull is cancelled if the HTTP connection is closed.
instance HasOptionalParam ImageCreate FromImage where
  applyOptionalParam req (FromImage xs) =
    req `setQuery` toQuery ("fromImage", Just xs)

-- | /Optional Param/ "fromSrc" - Source to import. The value may be a URL from which the image can be retrieved or `-` to read the image from the request body. This parameter may only be used when importing an image.
instance HasOptionalParam ImageCreate FromSrc where
  applyOptionalParam req (FromSrc xs) =
    req `setQuery` toQuery ("fromSrc", Just xs)

-- | /Optional Param/ "repo" - Repository name given to an image when it is imported. The repo may include a tag. This parameter may only be used when importing an image.
instance HasOptionalParam ImageCreate Repo where
  applyOptionalParam req (Repo xs) =
    req `setQuery` toQuery ("repo", Just xs)

-- | /Optional Param/ "tag" - Tag or digest. If empty when pulling an image, this causes all tags for the given image to be pulled.
instance HasOptionalParam ImageCreate Tag where
  applyOptionalParam req (Tag xs) =
    req `setQuery` toQuery ("tag", Just xs)

-- | /Optional Param/ "X-Registry-Auth" - A base64-encoded auth configuration. [See the authentication section for details.](#section/Authentication)
instance HasOptionalParam ImageCreate XRegistryAuth where
  applyOptionalParam req (XRegistryAuth xs) =
    req `setHeader` toHeader ("X-Registry-Auth", xs)

-- | @application/octet-stream@
instance Consumes ImageCreate MimeOctetStream
-- | @text/plain@
instance Consumes ImageCreate MimePlainText

-- | @application/json@
instance Produces ImageCreate MimeJSON


-- *** imageDelete

-- | @DELETE \/images\/{name}@
-- 
-- Remove an image
-- 
-- Remove an image, along with any untagged parent images that were referenced by that image.  Images can't be removed if they have descendant images, are being used by a running container or are being used by a build. 
-- 
imageDelete 
  :: Name -- ^ "name" -  Image name or ID
  -> DockerEngineRequest ImageDelete MimeNoContent [ImageDeleteResponseItem] MimeJSON
imageDelete (Name name) =
  _mkRequest "DELETE" ["/images/",toPath name]

data ImageDelete  

-- | /Optional Param/ "force" - Remove the image even if it is being used by stopped containers or has other tags
instance HasOptionalParam ImageDelete Force where
  applyOptionalParam req (Force xs) =
    req `setQuery` toQuery ("force", Just xs)

-- | /Optional Param/ "noprune" - Do not delete untagged parent images
instance HasOptionalParam ImageDelete Noprune where
  applyOptionalParam req (Noprune xs) =
    req `setQuery` toQuery ("noprune", Just xs)
-- | @application/json@
instance Produces ImageDelete MimeJSON


-- *** imageGet

-- | @GET \/images\/{name}\/get@
-- 
-- Export an image
-- 
-- Get a tarball containing all images and metadata for a repository.  If `name` is a specific name and tag (e.g. `ubuntu:latest`), then only that image (and its parents) are returned. If `name` is an image ID, similarly only that image (and its parents) are returned, but with the exclusion of the `repositories` file in the tarball, as there were no image names referenced.  ### Image tarball format  An image tarball contains one directory per image layer (named using its long ID), each containing these files:  - `VERSION`: currently `1.0` - the file format version - `json`: detailed layer information, similar to `docker inspect layer_id` - `layer.tar`: A tarfile containing the filesystem changes in this layer  The `layer.tar` file contains `aufs` style `.wh..wh.aufs` files and directories for storing attribute changes and deletions.  If the tarball defines a repository, the tarball should also include a `repositories` file at the root that contains a list of repository and tag names mapped to layer IDs.  ```json {   \"hello-world\": {     \"latest\": \"565a9d68a73f6706862bfe8409a7f659776d4d60a8d096eb4a3cbce6999cc2a1\"   } } ``` 
-- 
imageGet 
  :: Name -- ^ "name" -  Image name or ID
  -> DockerEngineRequest ImageGet MimeNoContent FilePath MimeXTar
imageGet (Name name) =
  _mkRequest "GET" ["/images/",toPath name,"/get"]

data ImageGet  
-- | @application/x-tar@
instance Produces ImageGet MimeXTar


-- *** imageGetAll

-- | @GET \/images\/get@
-- 
-- Export several images
-- 
-- Get a tarball containing all images and metadata for several image repositories.  For each value of the `names` parameter: if it is a specific name and tag (e.g. `ubuntu:latest`), then only that image (and its parents) are returned; if it is an image ID, similarly only that image (and its parents) are returned and there would be no names referenced in the 'repositories' file for this image ID.  For details on the format, see [the export image endpoint](#operation/ImageGet). 
-- 
imageGetAll 
  :: DockerEngineRequest ImageGetAll MimeNoContent FilePath MimeXTar
imageGetAll =
  _mkRequest "GET" ["/images/get"]

data ImageGetAll  

-- | /Optional Param/ "names" - Image names to filter by
instance HasOptionalParam ImageGetAll Names where
  applyOptionalParam req (Names xs) =
    req `setQuery` toQueryColl CommaSeparated ("names", Just xs)
-- | @application/x-tar@
instance Produces ImageGetAll MimeXTar


-- *** imageHistory

-- | @GET \/images\/{name}\/history@
-- 
-- Get the history of an image
-- 
-- Return parent layers of an image.
-- 
imageHistory 
  :: Name -- ^ "name" -  Image name or ID
  -> DockerEngineRequest ImageHistory MimeNoContent [InlineResponse2005] MimeJSON
imageHistory (Name name) =
  _mkRequest "GET" ["/images/",toPath name,"/history"]

data ImageHistory  
-- | @application/json@
instance Produces ImageHistory MimeJSON


-- *** imageInspect

-- | @GET \/images\/{name}\/json@
-- 
-- Inspect an image
-- 
-- Return low-level information about an image.
-- 
imageInspect 
  :: Name -- ^ "name" -  Image name or id
  -> DockerEngineRequest ImageInspect MimeNoContent Image MimeJSON
imageInspect (Name name) =
  _mkRequest "GET" ["/images/",toPath name,"/json"]

data ImageInspect  
-- | @application/json@
instance Produces ImageInspect MimeJSON


-- *** imageList

-- | @GET \/images\/json@
-- 
-- List Images
-- 
-- Returns a list of images on the server. Note that it uses a different, smaller representation of an image than inspecting a single image.
-- 
imageList 
  :: DockerEngineRequest ImageList MimeNoContent [ImageSummary] MimeJSON
imageList =
  _mkRequest "GET" ["/images/json"]

data ImageList  

-- | /Optional Param/ "all" - Show all images. Only images from a final layer (no children) are shown by default.
instance HasOptionalParam ImageList All where
  applyOptionalParam req (All xs) =
    req `setQuery` toQuery ("all", Just xs)

-- | /Optional Param/ "filters" - A JSON encoded value of the filters (a `map[string][]string`) to process on the images list. Available filters:  - `before`=(`<image-name>[:<tag>]`,  `<image id>` or `<image@digest>`) - `dangling=true` - `label=key` or `label=\"key=value\"` of an image label - `reference`=(`<image-name>[:<tag>]`) - `since`=(`<image-name>[:<tag>]`,  `<image id>` or `<image@digest>`) 
instance HasOptionalParam ImageList Filters where
  applyOptionalParam req (Filters xs) =
    req `setQuery` toQuery ("filters", Just xs)

-- | /Optional Param/ "digests" - Show digest information as a `RepoDigests` field on each image.
instance HasOptionalParam ImageList Digests where
  applyOptionalParam req (Digests xs) =
    req `setQuery` toQuery ("digests", Just xs)
-- | @application/json@
instance Produces ImageList MimeJSON


-- *** imageLoad

-- | @POST \/images\/load@
-- 
-- Import images
-- 
-- Load a set of images and tags into a repository.  For details on the format, see [the export image endpoint](#operation/ImageGet). 
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
imageLoad 
  :: (Consumes ImageLoad MimeXTar)
  => DockerEngineRequest ImageLoad MimeXTar res MimeJSON
imageLoad =
  _mkRequest "POST" ["/images/load"]

data ImageLoad 

-- | /Body Param/ "imagesTarball" - Tar archive containing images
instance HasBodyParam ImageLoad ImagesTarball 

-- | /Optional Param/ "quiet" - Suppress progress details during load.
instance HasOptionalParam ImageLoad Quiet where
  applyOptionalParam req (Quiet xs) =
    req `setQuery` toQuery ("quiet", Just xs)

-- | @application/x-tar@
instance Consumes ImageLoad MimeXTar

-- | @application/json@
instance Produces ImageLoad MimeJSON


-- *** imagePrune

-- | @POST \/images\/prune@
-- 
-- Delete unused images
-- 
imagePrune 
  :: DockerEngineRequest ImagePrune MimeNoContent InlineResponse2007 MimeJSON
imagePrune =
  _mkRequest "POST" ["/images/prune"]

data ImagePrune  

-- | /Optional Param/ "filters" - Filters to process on the prune list, encoded as JSON (a `map[string][]string`). Available filters:  - `dangling=<boolean>` When set to `true` (or `1`), prune only    unused *and* untagged images. When set to `false`    (or `0`), all unused images are pruned. - `until=<string>` Prune images created before this timestamp. The `<timestamp>` can be Unix timestamps, date formatted timestamps, or Go duration strings (e.g. `10m`, `1h30m`) computed relative to the daemon machine’s time. - `label` (`label=<key>`, `label=<key>=<value>`, `label!=<key>`, or `label!=<key>=<value>`) Prune images with (or without, in case `label!=...` is used) the specified labels. 
instance HasOptionalParam ImagePrune Filters where
  applyOptionalParam req (Filters xs) =
    req `setQuery` toQuery ("filters", Just xs)
-- | @application/json@
instance Produces ImagePrune MimeJSON


-- *** imagePush

-- | @POST \/images\/{name}\/push@
-- 
-- Push an image
-- 
-- Push an image to a registry.  If you wish to push an image on to a private registry, that image must already have a tag which references the registry. For example, `registry.example.com/myimage:latest`.  The push is cancelled if the HTTP connection is closed. 
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
imagePush 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Name -- ^ "name" -  Image name or ID.
  -> XRegistryAuth -- ^ "xRegistryAuth" -  A base64-encoded auth configuration. [See the authentication section for details.](#section/Authentication)
  -> DockerEngineRequest ImagePush MimeNoContent res accept
imagePush  _ (Name name) (XRegistryAuth xRegistryAuth) =
  _mkRequest "POST" ["/images/",toPath name,"/push"]
    `setHeader` toHeader ("X-Registry-Auth", xRegistryAuth)

data ImagePush  

-- | /Optional Param/ "tag" - The tag to associate with the image on the registry.
instance HasOptionalParam ImagePush Tag where
  applyOptionalParam req (Tag xs) =
    req `setQuery` toQuery ("tag", Just xs)
-- | @application/json@
instance Produces ImagePush MimeJSON
-- | @text/plain@
instance Produces ImagePush MimePlainText


-- *** imageSearch

-- | @GET \/images\/search@
-- 
-- Search images
-- 
-- Search for an image on Docker Hub.
-- 
imageSearch 
  :: Term -- ^ "term" -  Term to search
  -> DockerEngineRequest ImageSearch MimeNoContent [InlineResponse2006] MimeJSON
imageSearch (Term term) =
  _mkRequest "GET" ["/images/search"]
    `setQuery` toQuery ("term", Just term)

data ImageSearch  

-- | /Optional Param/ "limit" - Maximum number of results to return
instance HasOptionalParam ImageSearch Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "filters" - A JSON encoded value of the filters (a `map[string][]string`) to process on the images list. Available filters:  - `is-automated=(true|false)` - `is-official=(true|false)` - `stars=<number>` Matches images that has at least 'number' stars. 
instance HasOptionalParam ImageSearch Filters where
  applyOptionalParam req (Filters xs) =
    req `setQuery` toQuery ("filters", Just xs)
-- | @application/json@
instance Produces ImageSearch MimeJSON


-- *** imageTag

-- | @POST \/images\/{name}\/tag@
-- 
-- Tag an image
-- 
-- Tag an image so that it becomes part of a repository.
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
imageTag 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Name -- ^ "name" -  Image name or ID to tag.
  -> DockerEngineRequest ImageTag MimeNoContent res accept
imageTag  _ (Name name) =
  _mkRequest "POST" ["/images/",toPath name,"/tag"]

data ImageTag  

-- | /Optional Param/ "repo" - The repository to tag in. For example, `someuser/someimage`.
instance HasOptionalParam ImageTag Repo where
  applyOptionalParam req (Repo xs) =
    req `setQuery` toQuery ("repo", Just xs)

-- | /Optional Param/ "tag" - The name of the new tag.
instance HasOptionalParam ImageTag Tag where
  applyOptionalParam req (Tag xs) =
    req `setQuery` toQuery ("tag", Just xs)
-- | @application/json@
instance Produces ImageTag MimeJSON
-- | @text/plain@
instance Produces ImageTag MimePlainText

