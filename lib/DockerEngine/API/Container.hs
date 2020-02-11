{-
   Docker Engine API

   The Engine API is an HTTP API served by Docker Engine. It is the API the Docker client uses to communicate with the Engine, so everything the Docker client can do can be done with the API.  Most of the client's commands map directly to API endpoints (e.g. `docker ps` is `GET /containers/json`). The notable exception is running containers, which consists of several API calls.  # Errors  The API uses standard HTTP status codes to indicate the success or failure of the API call. The body of the response will be JSON in the following format:  ``` {   \"message\": \"page not found\" } ```  # Versioning  The API is usually changed in each release of Docker, so API calls are versioned to ensure that clients don't break.  For Docker Engine 17.06, the API version is 1.30. To lock to this version, you prefix the URL with `/v1.30`. For example, calling `/info` is the same as calling `/v1.30/info`.  Engine releases in the near future should support this version of the API, so your client will continue to work even if it is talking to a newer Engine.  In previous versions of Docker, it was possible to access the API without providing a version. This behaviour is now deprecated will be removed in a future version of Docker.  The API uses an open schema model, which means server may add extra properties to responses. Likewise, the server will ignore any extra query parameters and request body properties. When you write clients, you need to ignore additional properties in responses to ensure they do not break when talking to newer Docker daemons.  This documentation is for version 1.30 of the API, which was introduced with Docker 17.06. Use this table to find documentation for previous versions of the API:  Docker version  | API version | Changes ----------------|-------------|--------- 17.05.x | [1.29](https://docs.docker.com/engine/api/v1.29/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-29-api-changes) 17.04.x | [1.28](https://docs.docker.com/engine/api/v1.28/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-28-api-changes) 17.03.1 | [1.27](https://docs.docker.com/engine/api/v1.27/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-27-api-changes) 1.13.1 & 17.03.0 | [1.26](https://docs.docker.com/engine/api/v1.26/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-26-api-changes) 1.13.0 | [1.25](https://docs.docker.com/engine/api/v1.25/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-25-api-changes) 1.12.x | [1.24](https://docs.docker.com/engine/api/v1.24/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-24-api-changes) 1.11.x | [1.23](https://docs.docker.com/engine/api/v1.23/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-23-api-changes) 1.10.x | [1.22](https://docs.docker.com/engine/api/v1.22/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-22-api-changes) 1.9.x | [1.21](https://docs.docker.com/engine/api/v1.21/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-21-api-changes) 1.8.x | [1.20](https://docs.docker.com/engine/api/v1.20/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-20-api-changes) 1.7.x | [1.19](https://docs.docker.com/engine/api/v1.19/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-19-api-changes) 1.6.x | [1.18](https://docs.docker.com/engine/api/v1.18/) | [API changes](https://docs.docker.com/engine/api/version-history/#v1-18-api-changes)  # Authentication  Authentication for registries is handled client side. The client has to send authentication details to various endpoints that need to communicate with registries, such as `POST /images/(name)/push`. These are sent as `X-Registry-Auth` header as a Base64 encoded (JSON) string with the following structure:  ``` {   \"username\": \"string\",   \"password\": \"string\",   \"email\": \"string\",   \"serveraddress\": \"string\" } ```  The `serveraddress` is a domain/IP without a protocol. Throughout this structure, double quotes are required.  If you have already got an identity token from the [`/auth` endpoint](#operation/SystemAuth), you can just pass this instead of credentials:  ``` {   \"identitytoken\": \"9cbaf023786cd7...\" } ``` 

   OpenAPI Version: 3.0.1
   Docker Engine API API version: 1.30
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : DockerEngine.API.Container
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module DockerEngine.API.Container where

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


-- ** Container

-- *** containerArchive

-- | @GET \/containers\/{id}\/archive@
-- 
-- Get an archive of a filesystem resource in a container
-- 
-- Get a tar archive of a resource in the filesystem of container id.
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
containerArchive 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> Path -- ^ "path" -  Resource in the container’s filesystem to archive.
  -> DockerEngineRequest ContainerArchive MimeNoContent res accept
containerArchive  _ (Id id) (Path path) =
  _mkRequest "GET" ["/containers/",toPath id,"/archive"]
    `setQuery` toQuery ("path", Just path)

data ContainerArchive  
-- | @application/x-tar@
instance Produces ContainerArchive MimeXTar
-- | @application/json@
instance Produces ContainerArchive MimeJSON


-- *** containerArchiveInfo

-- | @HEAD \/containers\/{id}\/archive@
-- 
-- Get information about files in a container
-- 
-- A response header `X-Docker-Container-Path-Stat` is return containing a base64 - encoded JSON object with some filesystem header information about the path.
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
containerArchiveInfo 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> Path -- ^ "path" -  Resource in the container’s filesystem to archive.
  -> DockerEngineRequest ContainerArchiveInfo MimeNoContent res accept
containerArchiveInfo  _ (Id id) (Path path) =
  _mkRequest "HEAD" ["/containers/",toPath id,"/archive"]
    `setQuery` toQuery ("path", Just path)

data ContainerArchiveInfo  
-- | @application/json@
instance Produces ContainerArchiveInfo MimeJSON
-- | @text/plain@
instance Produces ContainerArchiveInfo MimePlainText


-- *** containerAttach

-- | @POST \/containers\/{id}\/attach@
-- 
-- Attach to a container
-- 
-- Attach to a container to read its output or send it input. You can attach to the same container multiple times and you can reattach to containers that have been detached.  Either the `stream` or `logs` parameter must be `true` for this endpoint to do anything.  See [the documentation for the `docker attach` command](https://docs.docker.com/engine/reference/commandline/attach/) for more details.  ### Hijacking  This endpoint hijacks the HTTP connection to transport `stdin`, `stdout`, and `stderr` on the same socket.  This is the response from the daemon for an attach request:  ``` HTTP/1.1 200 OK Content-Type: application/vnd.docker.raw-stream  [STREAM] ```  After the headers and two new lines, the TCP connection can now be used for raw, bidirectional communication between the client and server.  To hint potential proxies about connection hijacking, the Docker client can also optionally send connection upgrade headers.  For example, the client sends this request to upgrade the connection:  ``` POST /containers/16253994b7c4/attach?stream=1&stdout=1 HTTP/1.1 Upgrade: tcp Connection: Upgrade ```  The Docker daemon will respond with a `101 UPGRADED` response, and will similarly follow with the raw stream:  ``` HTTP/1.1 101 UPGRADED Content-Type: application/vnd.docker.raw-stream Connection: Upgrade Upgrade: tcp  [STREAM] ```  ### Stream format  When the TTY setting is disabled in [`POST /containers/create`](#operation/ContainerCreate), the stream over the hijacked connected is multiplexed to separate out `stdout` and `stderr`. The stream consists of a series of frames, each containing a header and a payload.  The header contains the information which the stream writes (`stdout` or `stderr`). It also contains the size of the associated frame encoded in the last four bytes (`uint32`).  It is encoded on the first eight bytes like this:  ```go header := [8]byte{STREAM_TYPE, 0, 0, 0, SIZE1, SIZE2, SIZE3, SIZE4} ```  `STREAM_TYPE` can be:  - 0: `stdin` (is written on `stdout`) - 1: `stdout` - 2: `stderr`  `SIZE1, SIZE2, SIZE3, SIZE4` are the four bytes of the `uint32` size encoded as big endian.  Following the header is the payload, which is the specified number of bytes of `STREAM_TYPE`.  The simplest way to implement this protocol is the following:  1. Read 8 bytes. 2. Choose `stdout` or `stderr` depending on the first byte. 3. Extract the frame size from the last four bytes. 4. Read the extracted size and output it on the correct output. 5. Goto 1.  ### Stream format when using a TTY  When the TTY setting is enabled in [`POST /containers/create`](#operation/ContainerCreate), the stream is not multiplexed. The data exchanged over the hijacked connection is simply the raw data from the process PTY and client's `stdin`. 
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
containerAttach 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerAttach MimeNoContent res accept
containerAttach  _ (Id id) =
  _mkRequest "POST" ["/containers/",toPath id,"/attach"]

data ContainerAttach  

-- | /Optional Param/ "detachKeys" - Override the key sequence for detaching a container.Format is a single character `[a-Z]` or `ctrl-<value>` where `<value>` is one of: `a-z`, `@`, `^`, `[`, `,` or `_`.
instance HasOptionalParam ContainerAttach DetachKeys where
  applyOptionalParam req (DetachKeys xs) =
    req `setQuery` toQuery ("detachKeys", Just xs)

-- | /Optional Param/ "logs" - Replay previous logs from the container.  This is useful for attaching to a container that has started and you want to output everything since the container started.  If `stream` is also enabled, once all the previous output has been returned, it will seamlessly transition into streaming current output. 
instance HasOptionalParam ContainerAttach Logs where
  applyOptionalParam req (Logs xs) =
    req `setQuery` toQuery ("logs", Just xs)

-- | /Optional Param/ "stream" - Stream attached streams from the time the request was made onwards
instance HasOptionalParam ContainerAttach Stream where
  applyOptionalParam req (Stream xs) =
    req `setQuery` toQuery ("stream", Just xs)

-- | /Optional Param/ "stdin" - Attach to `stdin`
instance HasOptionalParam ContainerAttach Stdin where
  applyOptionalParam req (Stdin xs) =
    req `setQuery` toQuery ("stdin", Just xs)

-- | /Optional Param/ "stdout" - Attach to `stdout`
instance HasOptionalParam ContainerAttach Stdout where
  applyOptionalParam req (Stdout xs) =
    req `setQuery` toQuery ("stdout", Just xs)

-- | /Optional Param/ "stderr" - Attach to `stderr`
instance HasOptionalParam ContainerAttach Stderr where
  applyOptionalParam req (Stderr xs) =
    req `setQuery` toQuery ("stderr", Just xs)
-- | @application/json@
instance Produces ContainerAttach MimeJSON
-- | @application/vnd.docker.raw-stream@
instance Produces ContainerAttach MimeVndDockerRawStream


-- *** containerAttachWebsocket

-- | @GET \/containers\/{id}\/attach\/ws@
-- 
-- Attach to a container via a websocket
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
containerAttachWebsocket 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerAttachWebsocket MimeNoContent res accept
containerAttachWebsocket  _ (Id id) =
  _mkRequest "GET" ["/containers/",toPath id,"/attach/ws"]

data ContainerAttachWebsocket  

-- | /Optional Param/ "detachKeys" - Override the key sequence for detaching a container.Format is a single character `[a-Z]` or `ctrl-<value>` where `<value>` is one of: `a-z`, `@`, `^`, `[`, `,`, or `_`.
instance HasOptionalParam ContainerAttachWebsocket DetachKeys where
  applyOptionalParam req (DetachKeys xs) =
    req `setQuery` toQuery ("detachKeys", Just xs)

-- | /Optional Param/ "logs" - Return logs
instance HasOptionalParam ContainerAttachWebsocket Logs where
  applyOptionalParam req (Logs xs) =
    req `setQuery` toQuery ("logs", Just xs)

-- | /Optional Param/ "stream" - Return stream
instance HasOptionalParam ContainerAttachWebsocket Stream where
  applyOptionalParam req (Stream xs) =
    req `setQuery` toQuery ("stream", Just xs)

-- | /Optional Param/ "stdin" - Attach to `stdin`
instance HasOptionalParam ContainerAttachWebsocket Stdin where
  applyOptionalParam req (Stdin xs) =
    req `setQuery` toQuery ("stdin", Just xs)

-- | /Optional Param/ "stdout" - Attach to `stdout`
instance HasOptionalParam ContainerAttachWebsocket Stdout where
  applyOptionalParam req (Stdout xs) =
    req `setQuery` toQuery ("stdout", Just xs)

-- | /Optional Param/ "stderr" - Attach to `stderr`
instance HasOptionalParam ContainerAttachWebsocket Stderr where
  applyOptionalParam req (Stderr xs) =
    req `setQuery` toQuery ("stderr", Just xs)
-- | @application/json@
instance Produces ContainerAttachWebsocket MimeJSON
-- | @text/plain@
instance Produces ContainerAttachWebsocket MimePlainText


-- *** containerChanges

-- | @GET \/containers\/{id}\/changes@
-- 
-- Get changes on a container’s filesystem
-- 
-- Returns which files in a container's filesystem have been added, deleted, or modified. The `Kind` of modification can be one of:  - `0`: Modified - `1`: Added - `2`: Deleted 
-- 
containerChanges 
  :: Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerChanges MimeNoContent [InlineResponse2002] MimeJSON
containerChanges (Id id) =
  _mkRequest "GET" ["/containers/",toPath id,"/changes"]

data ContainerChanges  
-- | @application/json@
instance Produces ContainerChanges MimeJSON


-- *** containerCreate

-- | @POST \/containers\/create@
-- 
-- Create a container
-- 
containerCreate 
  :: (Consumes ContainerCreate contentType, MimeRender contentType UNKNOWN_BASE_TYPE)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> UNKNOWN_BASE_TYPE -- ^ "body" -  Container to create
  -> DockerEngineRequest ContainerCreate contentType InlineResponse201 MimeJSON
containerCreate _ body =
  _mkRequest "POST" ["/containers/create"]
    `setBodyParam` body

data ContainerCreate 

-- | /Body Param/ "body" - Container to create
instance HasBodyParam ContainerCreate UNKNOWN_BASE_TYPE 

-- | /Optional Param/ "name" - Assign the specified name to the container. Must match `/?[a-zA-Z0-9_-]+`.
instance HasOptionalParam ContainerCreate Name where
  applyOptionalParam req (Name xs) =
    req `setQuery` toQuery ("name", Just xs)

-- | @application/octet-stream@
instance Consumes ContainerCreate MimeOctetStream
-- | @application/json@
instance Consumes ContainerCreate MimeJSON

-- | @application/json@
instance Produces ContainerCreate MimeJSON


-- *** containerDelete

-- | @DELETE \/containers\/{id}@
-- 
-- Remove a container
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
containerDelete 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerDelete MimeNoContent res accept
containerDelete  _ (Id id) =
  _mkRequest "DELETE" ["/containers/",toPath id]

data ContainerDelete  

-- | /Optional Param/ "v" - Remove the volumes associated with the container.
instance HasOptionalParam ContainerDelete V where
  applyOptionalParam req (V xs) =
    req `setQuery` toQuery ("v", Just xs)

-- | /Optional Param/ "force" - If the container is running, kill it before removing it.
instance HasOptionalParam ContainerDelete Force where
  applyOptionalParam req (Force xs) =
    req `setQuery` toQuery ("force", Just xs)

-- | /Optional Param/ "link" - Remove the specified link associated with the container.
instance HasOptionalParam ContainerDelete Link where
  applyOptionalParam req (Link xs) =
    req `setQuery` toQuery ("link", Just xs)
-- | @application/json@
instance Produces ContainerDelete MimeJSON
-- | @text/plain@
instance Produces ContainerDelete MimePlainText


-- *** containerExport

-- | @GET \/containers\/{id}\/export@
-- 
-- Export a container
-- 
-- Export the contents of a container as a tarball.
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
containerExport 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerExport MimeNoContent res accept
containerExport  _ (Id id) =
  _mkRequest "GET" ["/containers/",toPath id,"/export"]

data ContainerExport  
-- | @application/octet-stream@
instance Produces ContainerExport MimeOctetStream
-- | @application/json@
instance Produces ContainerExport MimeJSON


-- *** containerInspect

-- | @GET \/containers\/{id}\/json@
-- 
-- Inspect a container
-- 
-- Return low-level information about a container.
-- 
containerInspect 
  :: Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerInspect MimeNoContent InlineResponse200 MimeJSON
containerInspect (Id id) =
  _mkRequest "GET" ["/containers/",toPath id,"/json"]

data ContainerInspect  

-- | /Optional Param/ "size" - Return the size of container as fields `SizeRw` and `SizeRootFs`
instance HasOptionalParam ContainerInspect Size where
  applyOptionalParam req (Size xs) =
    req `setQuery` toQuery ("size", Just xs)
-- | @application/json@
instance Produces ContainerInspect MimeJSON


-- *** containerKill

-- | @POST \/containers\/{id}\/kill@
-- 
-- Kill a container
-- 
-- Send a POSIX signal to a container, defaulting to killing to the container.
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
containerKill 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerKill MimeNoContent res accept
containerKill  _ (Id id) =
  _mkRequest "POST" ["/containers/",toPath id,"/kill"]

data ContainerKill  

-- | /Optional Param/ "signal" - Signal to send to the container as an integer or string (e.g. `SIGINT`)
instance HasOptionalParam ContainerKill Signal where
  applyOptionalParam req (Signal xs) =
    req `setQuery` toQuery ("signal", Just xs)
-- | @application/json@
instance Produces ContainerKill MimeJSON
-- | @text/plain@
instance Produces ContainerKill MimePlainText


-- *** containerList

-- | @GET \/containers\/json@
-- 
-- List containers
-- 
containerList 
  :: DockerEngineRequest ContainerList MimeNoContent [A.Value] MimeJSON
containerList =
  _mkRequest "GET" ["/containers/json"]

data ContainerList  

-- | /Optional Param/ "all" - Return all containers. By default, only running containers are shown
instance HasOptionalParam ContainerList All where
  applyOptionalParam req (All xs) =
    req `setQuery` toQuery ("all", Just xs)

-- | /Optional Param/ "limit" - Return this number of most recently created containers, including non-running ones.
instance HasOptionalParam ContainerList Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "size" - Return the size of container as fields `SizeRw` and `SizeRootFs`.
instance HasOptionalParam ContainerList Size where
  applyOptionalParam req (Size xs) =
    req `setQuery` toQuery ("size", Just xs)

-- | /Optional Param/ "filters" - Filters to process on the container list, encoded as JSON (a `map[string][]string`). For example, `{\"status\": [\"paused\"]}` will only return paused containers. Available filters:  - `ancestor`=(`<image-name>[:<tag>]`, `<image id>`, or `<image@digest>`) - `before`=(`<container id>` or `<container name>`) - `expose`=(`<port>[/<proto>]`|`<startport-endport>/[<proto>]`) - `exited=<int>` containers with exit code of `<int>` - `health`=(`starting`|`healthy`|`unhealthy`|`none`) - `id=<ID>` a container's ID - `isolation=`(`default`|`process`|`hyperv`) (Windows daemon only) - `is-task=`(`true`|`false`) - `label=key` or `label=\"key=value\"` of a container label - `name=<name>` a container's name - `network`=(`<network id>` or `<network name>`) - `publish`=(`<port>[/<proto>]`|`<startport-endport>/[<proto>]`) - `since`=(`<container id>` or `<container name>`) - `status=`(`created`|`restarting`|`running`|`removing`|`paused`|`exited`|`dead`) - `volume`=(`<volume name>` or `<mount point destination>`) 
instance HasOptionalParam ContainerList Filters where
  applyOptionalParam req (Filters xs) =
    req `setQuery` toQuery ("filters", Just xs)
-- | @application/json@
instance Produces ContainerList MimeJSON


-- *** containerLogs

-- | @GET \/containers\/{id}\/logs@
-- 
-- Get container logs
-- 
-- Get `stdout` and `stderr` logs from a container.  Note: This endpoint works only for containers with the `json-file` or `journald` logging driver. 
-- 
containerLogs 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerLogs MimeNoContent Text accept
containerLogs  _ (Id id) =
  _mkRequest "GET" ["/containers/",toPath id,"/logs"]

data ContainerLogs  

-- | /Optional Param/ "follow" - Return the logs as a stream.  This will return a `101` HTTP response with a `Connection: upgrade` header, then hijack the HTTP connection to send raw output. For more information about hijacking and the stream format, [see the documentation for the attach endpoint](#operation/ContainerAttach). 
instance HasOptionalParam ContainerLogs Follow where
  applyOptionalParam req (Follow xs) =
    req `setQuery` toQuery ("follow", Just xs)

-- | /Optional Param/ "stdout" - Return logs from `stdout`
instance HasOptionalParam ContainerLogs Stdout where
  applyOptionalParam req (Stdout xs) =
    req `setQuery` toQuery ("stdout", Just xs)

-- | /Optional Param/ "stderr" - Return logs from `stderr`
instance HasOptionalParam ContainerLogs Stderr where
  applyOptionalParam req (Stderr xs) =
    req `setQuery` toQuery ("stderr", Just xs)

-- | /Optional Param/ "since" - Only return logs since this time, as a UNIX timestamp
instance HasOptionalParam ContainerLogs Since where
  applyOptionalParam req (Since xs) =
    req `setQuery` toQuery ("since", Just xs)

-- | /Optional Param/ "timestamps" - Add timestamps to every log line
instance HasOptionalParam ContainerLogs Timestamps where
  applyOptionalParam req (Timestamps xs) =
    req `setQuery` toQuery ("timestamps", Just xs)

-- | /Optional Param/ "tail" - Only return this number of log lines from the end of the logs. Specify as an integer or `all` to output all log lines.
instance HasOptionalParam ContainerLogs Tail where
  applyOptionalParam req (Tail xs) =
    req `setQuery` toQuery ("tail", Just xs)
-- | @application/json@
instance Produces ContainerLogs MimeJSON
-- | @text/plain@
instance Produces ContainerLogs MimePlainText


-- *** containerPause

-- | @POST \/containers\/{id}\/pause@
-- 
-- Pause a container
-- 
-- Use the cgroups freezer to suspend all processes in a container.  Traditionally, when suspending a process the `SIGSTOP` signal is used, which is observable by the process being suspended. With the cgroups freezer the process is unaware, and unable to capture, that it is being suspended, and subsequently resumed. 
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
containerPause 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerPause MimeNoContent res accept
containerPause  _ (Id id) =
  _mkRequest "POST" ["/containers/",toPath id,"/pause"]

data ContainerPause  
-- | @application/json@
instance Produces ContainerPause MimeJSON
-- | @text/plain@
instance Produces ContainerPause MimePlainText


-- *** containerPrune

-- | @POST \/containers\/prune@
-- 
-- Delete stopped containers
-- 
containerPrune 
  :: DockerEngineRequest ContainerPrune MimeNoContent InlineResponse2005 MimeJSON
containerPrune =
  _mkRequest "POST" ["/containers/prune"]

data ContainerPrune  

-- | /Optional Param/ "filters" - Filters to process on the prune list, encoded as JSON (a `map[string][]string`).  Available filters: - `until=<timestamp>` Prune containers created before this timestamp. The `<timestamp>` can be Unix timestamps, date formatted timestamps, or Go duration strings (e.g. `10m`, `1h30m`) computed relative to the daemon machine’s time. - `label` (`label=<key>`, `label=<key>=<value>`, `label!=<key>`, or `label!=<key>=<value>`) Prune containers with (or without, in case `label!=...` is used) the specified labels. 
instance HasOptionalParam ContainerPrune Filters where
  applyOptionalParam req (Filters xs) =
    req `setQuery` toQuery ("filters", Just xs)
-- | @application/json@
instance Produces ContainerPrune MimeJSON


-- *** containerRename

-- | @POST \/containers\/{id}\/rename@
-- 
-- Rename a container
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
containerRename 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> Name -- ^ "name" -  New name for the container
  -> DockerEngineRequest ContainerRename MimeNoContent res accept
containerRename  _ (Id id) (Name name) =
  _mkRequest "POST" ["/containers/",toPath id,"/rename"]
    `setQuery` toQuery ("name", Just name)

data ContainerRename  
-- | @application/json@
instance Produces ContainerRename MimeJSON
-- | @text/plain@
instance Produces ContainerRename MimePlainText


-- *** containerResize

-- | @POST \/containers\/{id}\/resize@
-- 
-- Resize a container TTY
-- 
-- Resize the TTY for a container. You must restart the container for the resize to take effect.
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
containerResize 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerResize MimeNoContent res accept
containerResize  _ (Id id) =
  _mkRequest "POST" ["/containers/",toPath id,"/resize"]

data ContainerResize  

-- | /Optional Param/ "h" - Height of the tty session in characters
instance HasOptionalParam ContainerResize H where
  applyOptionalParam req (H xs) =
    req `setQuery` toQuery ("h", Just xs)

-- | /Optional Param/ "w" - Width of the tty session in characters
instance HasOptionalParam ContainerResize W where
  applyOptionalParam req (W xs) =
    req `setQuery` toQuery ("w", Just xs)
-- | @application/json@
instance Produces ContainerResize MimeJSON
-- | @text/plain@
instance Produces ContainerResize MimePlainText


-- *** containerRestart

-- | @POST \/containers\/{id}\/restart@
-- 
-- Restart a container
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
containerRestart 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerRestart MimeNoContent res accept
containerRestart  _ (Id id) =
  _mkRequest "POST" ["/containers/",toPath id,"/restart"]

data ContainerRestart  

-- | /Optional Param/ "t" - Number of seconds to wait before killing the container
instance HasOptionalParam ContainerRestart T where
  applyOptionalParam req (T xs) =
    req `setQuery` toQuery ("t", Just xs)
-- | @application/json@
instance Produces ContainerRestart MimeJSON
-- | @text/plain@
instance Produces ContainerRestart MimePlainText


-- *** containerStart

-- | @POST \/containers\/{id}\/start@
-- 
-- Start a container
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
containerStart 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerStart MimeNoContent res accept
containerStart  _ (Id id) =
  _mkRequest "POST" ["/containers/",toPath id,"/start"]

data ContainerStart  

-- | /Optional Param/ "detachKeys" - Override the key sequence for detaching a container. Format is a single character `[a-Z]` or `ctrl-<value>` where `<value>` is one of: `a-z`, `@`, `^`, `[`, `,` or `_`.
instance HasOptionalParam ContainerStart DetachKeys where
  applyOptionalParam req (DetachKeys xs) =
    req `setQuery` toQuery ("detachKeys", Just xs)
-- | @application/json@
instance Produces ContainerStart MimeJSON
-- | @text/plain@
instance Produces ContainerStart MimePlainText


-- *** containerStats

-- | @GET \/containers\/{id}\/stats@
-- 
-- Get container stats based on resource usage
-- 
-- This endpoint returns a live stream of a container’s resource usage statistics.  The `precpu_stats` is the CPU statistic of last read, which is used for calculating the CPU usage percentage. It is not the same as the `cpu_stats` field.  If either `precpu_stats.online_cpus` or `cpu_stats.online_cpus` is nil then for compatibility with older daemons the length of the corresponding `cpu_usage.percpu_usage` array should be used. 
-- 
containerStats 
  :: Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerStats MimeNoContent A.Value MimeJSON
containerStats (Id id) =
  _mkRequest "GET" ["/containers/",toPath id,"/stats"]

data ContainerStats  

-- | /Optional Param/ "stream" - Stream the output. If false, the stats will be output once and then it will disconnect.
instance HasOptionalParam ContainerStats Stream where
  applyOptionalParam req (Stream xs) =
    req `setQuery` toQuery ("stream", Just xs)
-- | @application/json@
instance Produces ContainerStats MimeJSON


-- *** containerStop

-- | @POST \/containers\/{id}\/stop@
-- 
-- Stop a container
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
containerStop 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerStop MimeNoContent res accept
containerStop  _ (Id id) =
  _mkRequest "POST" ["/containers/",toPath id,"/stop"]

data ContainerStop  

-- | /Optional Param/ "t" - Number of seconds to wait before killing the container
instance HasOptionalParam ContainerStop T where
  applyOptionalParam req (T xs) =
    req `setQuery` toQuery ("t", Just xs)
-- | @application/json@
instance Produces ContainerStop MimeJSON
-- | @text/plain@
instance Produces ContainerStop MimePlainText


-- *** containerTop

-- | @GET \/containers\/{id}\/top@
-- 
-- List processes running inside a container
-- 
-- On Unix systems, this is done by running the `ps` command. This endpoint is not supported on Windows.
-- 
containerTop 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerTop MimeNoContent InlineResponse2001 accept
containerTop  _ (Id id) =
  _mkRequest "GET" ["/containers/",toPath id,"/top"]

data ContainerTop  

-- | /Optional Param/ "ps_args" - The arguments to pass to `ps`. For example, `aux`
instance HasOptionalParam ContainerTop PsArgs where
  applyOptionalParam req (PsArgs xs) =
    req `setQuery` toQuery ("ps_args", Just xs)
-- | @application/json@
instance Produces ContainerTop MimeJSON
-- | @text/plain@
instance Produces ContainerTop MimePlainText


-- *** containerUnpause

-- | @POST \/containers\/{id}\/unpause@
-- 
-- Unpause a container
-- 
-- Resume a container which has been paused.
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
containerUnpause 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerUnpause MimeNoContent res accept
containerUnpause  _ (Id id) =
  _mkRequest "POST" ["/containers/",toPath id,"/unpause"]

data ContainerUnpause  
-- | @application/json@
instance Produces ContainerUnpause MimeJSON
-- | @text/plain@
instance Produces ContainerUnpause MimePlainText


-- *** containerUpdate

-- | @POST \/containers\/{id}\/update@
-- 
-- Update a container
-- 
-- Change various configuration options of a container without having to recreate it.
-- 
containerUpdate 
  :: (Consumes ContainerUpdate MimeJSON, MimeRender MimeJSON UNKNOWN_BASE_TYPE)
  => UNKNOWN_BASE_TYPE -- ^ "update"
  -> Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerUpdate MimeJSON InlineResponse2003 MimeJSON
containerUpdate update (Id id) =
  _mkRequest "POST" ["/containers/",toPath id,"/update"]
    `setBodyParam` update

data ContainerUpdate 
instance HasBodyParam ContainerUpdate UNKNOWN_BASE_TYPE 

-- | @application/json@
instance Consumes ContainerUpdate MimeJSON

-- | @application/json@
instance Produces ContainerUpdate MimeJSON


-- *** containerWait

-- | @POST \/containers\/{id}\/wait@
-- 
-- Wait for a container
-- 
-- Block until a container stops, then returns the exit code.
-- 
containerWait 
  :: Id -- ^ "id" -  ID or name of the container
  -> DockerEngineRequest ContainerWait MimeNoContent InlineResponse2004 MimeJSON
containerWait (Id id) =
  _mkRequest "POST" ["/containers/",toPath id,"/wait"]

data ContainerWait  

-- | /Optional Param/ "condition" - Wait until a container state reaches the given condition, either 'not-running' (default), 'next-exit', or 'removed'.
instance HasOptionalParam ContainerWait Condition where
  applyOptionalParam req (Condition xs) =
    req `setQuery` toQuery ("condition", Just xs)
-- | @application/json@
instance Produces ContainerWait MimeJSON


-- *** putContainerArchive

-- | @PUT \/containers\/{id}\/archive@
-- 
-- Extract an archive of files or folders to a directory in a container
-- 
-- Upload a tar archive to be extracted to a path in the filesystem of container id.
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
putContainerArchive 
  :: (Consumes PutContainerArchive contentType, MimeRender contentType InputStream)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> InputStream -- ^ "inputStream" -  The input stream must be a tar archive compressed with one of the following algorithms: identity (no compression), gzip, bzip2, xz.
  -> Id -- ^ "id" -  ID or name of the container
  -> Path -- ^ "path" -  Path to a directory in the container to extract the archive’s contents into. 
  -> DockerEngineRequest PutContainerArchive contentType res accept
putContainerArchive _  _ inputStream (Id id) (Path path) =
  _mkRequest "PUT" ["/containers/",toPath id,"/archive"]
    `setBodyParam` inputStream
    `setQuery` toQuery ("path", Just path)

data PutContainerArchive 

-- | /Body Param/ "inputStream" - The input stream must be a tar archive compressed with one of the following algorithms: identity (no compression), gzip, bzip2, xz.
instance HasBodyParam PutContainerArchive InputStream 

-- | /Optional Param/ "noOverwriteDirNonDir" - If “1”, “true”, or “True” then it will be an error if unpacking the given content would cause an existing directory to be replaced with a non-directory and vice versa.
instance HasOptionalParam PutContainerArchive NoOverwriteDirNonDir where
  applyOptionalParam req (NoOverwriteDirNonDir xs) =
    req `setQuery` toQuery ("noOverwriteDirNonDir", Just xs)

-- | @application/x-tar@
instance Consumes PutContainerArchive MimeXTar
-- | @application/octet-stream@
instance Consumes PutContainerArchive MimeOctetStream

-- | @application/json@
instance Produces PutContainerArchive MimeJSON
-- | @text/plain@
instance Produces PutContainerArchive MimePlainText

