{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import DockerEngine.Model
import DockerEngine.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy Address)
      propMimeEq MimeJSON (Proxy :: Proxy AuthConfig)
      propMimeEq MimeJSON (Proxy :: Proxy BuildInfo)
      propMimeEq MimeJSON (Proxy :: Proxy BuildPruneResponse)
      propMimeEq MimeJSON (Proxy :: Proxy CPUStats)
      propMimeEq MimeJSON (Proxy :: Proxy CPUStatsCpuUsage)
      propMimeEq MimeJSON (Proxy :: Proxy CPUStatsThrottlingData)
      propMimeEq MimeJSON (Proxy :: Proxy ClusterInfo)
      propMimeEq MimeJSON (Proxy :: Proxy Commit)
      propMimeEq MimeJSON (Proxy :: Proxy Config)
      propMimeEq MimeJSON (Proxy :: Proxy ConfigSpec)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerChangeResponseItem)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerConfig)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerConfigExtra)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerConfigExtraNetworkingConfig)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerConfigExtraVolumes)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerCreateResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerInspectResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerInspectResponseState)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerPruneResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerStatsResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerStatsResponsePidsStats)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerSummary)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerSummaryHostConfig)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerSummaryNetworkSettings)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerTopResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerUpdateResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerWaitResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerWaitResponseError)
      propMimeEq MimeJSON (Proxy :: Proxy CreateImageInfo)
      propMimeEq MimeJSON (Proxy :: Proxy DeviceMapping)
      propMimeEq MimeJSON (Proxy :: Proxy DistributionInspectResponse)
      propMimeEq MimeJSON (Proxy :: Proxy DistributionInspectResponseDescriptor)
      propMimeEq MimeJSON (Proxy :: Proxy DistributionInspectResponsePlatforms)
      propMimeEq MimeJSON (Proxy :: Proxy Driver)
      propMimeEq MimeJSON (Proxy :: Proxy EndpointIPAMConfig)
      propMimeEq MimeJSON (Proxy :: Proxy EndpointPortConfig)
      propMimeEq MimeJSON (Proxy :: Proxy EndpointSettings)
      propMimeEq MimeJSON (Proxy :: Proxy EndpointSpec)
      propMimeEq MimeJSON (Proxy :: Proxy EngineDescription)
      propMimeEq MimeJSON (Proxy :: Proxy EngineDescriptionPlugins)
      propMimeEq MimeJSON (Proxy :: Proxy ErrorDetail)
      propMimeEq MimeJSON (Proxy :: Proxy ErrorResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ExecConfig)
      propMimeEq MimeJSON (Proxy :: Proxy ExecInspectResponse)
      propMimeEq MimeJSON (Proxy :: Proxy GraphDriverData)
      propMimeEq MimeJSON (Proxy :: Proxy HealthConfig)
      propMimeEq MimeJSON (Proxy :: Proxy HistoryResponseItem)
      propMimeEq MimeJSON (Proxy :: Proxy HostConfig)
      propMimeEq MimeJSON (Proxy :: Proxy HostConfigAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy HostConfigAllOfLogConfig)
      propMimeEq MimeJSON (Proxy :: Proxy IPAM)
      propMimeEq MimeJSON (Proxy :: Proxy IdResponse)
      propMimeEq MimeJSON (Proxy :: Proxy Image)
      propMimeEq MimeJSON (Proxy :: Proxy ImageDeleteResponseItem)
      propMimeEq MimeJSON (Proxy :: Proxy ImageID)
      propMimeEq MimeJSON (Proxy :: Proxy ImageMetadata)
      propMimeEq MimeJSON (Proxy :: Proxy ImagePruneResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ImageRootFS)
      propMimeEq MimeJSON (Proxy :: Proxy ImageSearchResponseItem)
      propMimeEq MimeJSON (Proxy :: Proxy ImageSummary)
      propMimeEq MimeJSON (Proxy :: Proxy IndexInfo)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject1)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject2)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject3)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse400)
      propMimeEq MimeJSON (Proxy :: Proxy JoinTokens)
      propMimeEq MimeJSON (Proxy :: Proxy LocalNodeState)
      propMimeEq MimeJSON (Proxy :: Proxy ManagerStatus)
      propMimeEq MimeJSON (Proxy :: Proxy MemoryStats)
      propMimeEq MimeJSON (Proxy :: Proxy MemoryStatsStats)
      propMimeEq MimeJSON (Proxy :: Proxy Mount)
      propMimeEq MimeJSON (Proxy :: Proxy MountBindOptions)
      propMimeEq MimeJSON (Proxy :: Proxy MountPoint)
      propMimeEq MimeJSON (Proxy :: Proxy MountTmpfsOptions)
      propMimeEq MimeJSON (Proxy :: Proxy MountVolumeOptions)
      propMimeEq MimeJSON (Proxy :: Proxy MountVolumeOptionsDriverConfig)
      propMimeEq MimeJSON (Proxy :: Proxy Network)
      propMimeEq MimeJSON (Proxy :: Proxy NetworkConfig)
      propMimeEq MimeJSON (Proxy :: Proxy NetworkConnectConfig)
      propMimeEq MimeJSON (Proxy :: Proxy NetworkContainer)
      propMimeEq MimeJSON (Proxy :: Proxy NetworkCreateResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NetworkDisconnectConfig)
      propMimeEq MimeJSON (Proxy :: Proxy NetworkPruneResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NetworkSettings)
      propMimeEq MimeJSON (Proxy :: Proxy NetworkStats)
      propMimeEq MimeJSON (Proxy :: Proxy Node)
      propMimeEq MimeJSON (Proxy :: Proxy NodeDescription)
      propMimeEq MimeJSON (Proxy :: Proxy NodeSpec)
      propMimeEq MimeJSON (Proxy :: Proxy NodeState)
      propMimeEq MimeJSON (Proxy :: Proxy NodeStatus)
      propMimeEq MimeJSON (Proxy :: Proxy ObjectVersion)
      propMimeEq MimeJSON (Proxy :: Proxy PeerNode)
      propMimeEq MimeJSON (Proxy :: Proxy Platform)
      propMimeEq MimeJSON (Proxy :: Proxy Plugin)
      propMimeEq MimeJSON (Proxy :: Proxy PluginConfig)
      propMimeEq MimeJSON (Proxy :: Proxy PluginConfigArgs)
      propMimeEq MimeJSON (Proxy :: Proxy PluginConfigInterface)
      propMimeEq MimeJSON (Proxy :: Proxy PluginConfigLinux)
      propMimeEq MimeJSON (Proxy :: Proxy PluginConfigNetwork)
      propMimeEq MimeJSON (Proxy :: Proxy PluginConfigRootfs)
      propMimeEq MimeJSON (Proxy :: Proxy PluginConfigUser)
      propMimeEq MimeJSON (Proxy :: Proxy PluginDevice)
      propMimeEq MimeJSON (Proxy :: Proxy PluginEnv)
      propMimeEq MimeJSON (Proxy :: Proxy PluginInterfaceType)
      propMimeEq MimeJSON (Proxy :: Proxy PluginMount)
      propMimeEq MimeJSON (Proxy :: Proxy PluginPrivilegeItem)
      propMimeEq MimeJSON (Proxy :: Proxy PluginSettings)
      propMimeEq MimeJSON (Proxy :: Proxy PluginsInfo)
      propMimeEq MimeJSON (Proxy :: Proxy Port)
      propMimeEq MimeJSON (Proxy :: Proxy PortBinding)
      propMimeEq MimeJSON (Proxy :: Proxy ProcessConfig)
      propMimeEq MimeJSON (Proxy :: Proxy ProgressDetail)
      propMimeEq MimeJSON (Proxy :: Proxy PushImageInfo)
      propMimeEq MimeJSON (Proxy :: Proxy Reachability)
      propMimeEq MimeJSON (Proxy :: Proxy RegistryServiceConfig)
      propMimeEq MimeJSON (Proxy :: Proxy ResourceObject)
      propMimeEq MimeJSON (Proxy :: Proxy Resources)
      propMimeEq MimeJSON (Proxy :: Proxy ResourcesBlkioWeightDevice)
      propMimeEq MimeJSON (Proxy :: Proxy ResourcesUlimits)
      propMimeEq MimeJSON (Proxy :: Proxy RestartPolicy)
      propMimeEq MimeJSON (Proxy :: Proxy Runtime)
      propMimeEq MimeJSON (Proxy :: Proxy Secret)
      propMimeEq MimeJSON (Proxy :: Proxy SecretSpec)
      propMimeEq MimeJSON (Proxy :: Proxy Service)
      propMimeEq MimeJSON (Proxy :: Proxy ServiceCreateResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ServiceEndpoint)
      propMimeEq MimeJSON (Proxy :: Proxy ServiceEndpointVirtualIPs)
      propMimeEq MimeJSON (Proxy :: Proxy ServiceSpec)
      propMimeEq MimeJSON (Proxy :: Proxy ServiceSpecMode)
      propMimeEq MimeJSON (Proxy :: Proxy ServiceSpecModeReplicated)
      propMimeEq MimeJSON (Proxy :: Proxy ServiceSpecRollbackConfig)
      propMimeEq MimeJSON (Proxy :: Proxy ServiceSpecUpdateConfig)
      propMimeEq MimeJSON (Proxy :: Proxy ServiceUpdateResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ServiceUpdateStatus)
      propMimeEq MimeJSON (Proxy :: Proxy Swarm)
      propMimeEq MimeJSON (Proxy :: Proxy SwarmAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy SwarmInfo)
      propMimeEq MimeJSON (Proxy :: Proxy SwarmSpec)
      propMimeEq MimeJSON (Proxy :: Proxy SwarmSpecCAConfig)
      propMimeEq MimeJSON (Proxy :: Proxy SwarmSpecCAConfigExternalCAs)
      propMimeEq MimeJSON (Proxy :: Proxy SwarmSpecDispatcher)
      propMimeEq MimeJSON (Proxy :: Proxy SwarmSpecEncryptionConfig)
      propMimeEq MimeJSON (Proxy :: Proxy SwarmSpecOrchestration)
      propMimeEq MimeJSON (Proxy :: Proxy SwarmSpecRaft)
      propMimeEq MimeJSON (Proxy :: Proxy SwarmSpecTaskDefaults)
      propMimeEq MimeJSON (Proxy :: Proxy SwarmSpecTaskDefaultsLogDriver)
      propMimeEq MimeJSON (Proxy :: Proxy SystemAuthResponse)
      propMimeEq MimeJSON (Proxy :: Proxy SystemDataUsageResponse)
      propMimeEq MimeJSON (Proxy :: Proxy SystemEventsResponse)
      propMimeEq MimeJSON (Proxy :: Proxy SystemEventsResponseActor)
      propMimeEq MimeJSON (Proxy :: Proxy SystemInfo)
      propMimeEq MimeJSON (Proxy :: Proxy SystemVersionResponse)
      propMimeEq MimeJSON (Proxy :: Proxy SystemVersionResponseComponents)
      propMimeEq MimeJSON (Proxy :: Proxy SystemVersionResponsePlatform)
      propMimeEq MimeJSON (Proxy :: Proxy TLSInfo)
      propMimeEq MimeJSON (Proxy :: Proxy Task)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpec)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecContainerSpec)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecContainerSpecConfigs)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecContainerSpecDNSConfig)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecContainerSpecFile)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecContainerSpecPrivileges)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecContainerSpecPrivilegesCredentialSpec)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecContainerSpecPrivilegesSELinuxContext)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecContainerSpecSecrets)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecLogDriver)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecNetworks)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecPlacement)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecPlacementPreferences)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecPlacementSpread)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecPluginSpec)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecResources)
      propMimeEq MimeJSON (Proxy :: Proxy TaskSpecRestartPolicy)
      propMimeEq MimeJSON (Proxy :: Proxy TaskState)
      propMimeEq MimeJSON (Proxy :: Proxy TaskStatus)
      propMimeEq MimeJSON (Proxy :: Proxy TaskStatusContainerStatus)
      propMimeEq MimeJSON (Proxy :: Proxy ThrottleDevice)
      propMimeEq MimeJSON (Proxy :: Proxy UnlockKeyResponse)
      propMimeEq MimeJSON (Proxy :: Proxy Volume)
      propMimeEq MimeJSON (Proxy :: Proxy VolumeConfig)
      propMimeEq MimeJSON (Proxy :: Proxy VolumeListResponse)
      propMimeEq MimeJSON (Proxy :: Proxy VolumePruneResponse)
      propMimeEq MimeJSON (Proxy :: Proxy VolumeUsageData)
      
