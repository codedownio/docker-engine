{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import DockerEngine.Model
import DockerEngine.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V
import Data.String (fromString)

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

#if MIN_VERSION_aeson(2,0,0)
#else
-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = arbitraryValue
#endif

arbitraryValue :: Gen A.Value
arbitraryValue =
  frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (fromString k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models

instance Arbitrary Address where
  arbitrary = sized genAddress

genAddress :: Int -> Gen Address
genAddress n =
  Address
    <$> arbitraryReducedMaybe n -- addressAddr :: Maybe Text
    <*> arbitraryReducedMaybe n -- addressPrefixLen :: Maybe Int
  
instance Arbitrary AuthConfig where
  arbitrary = sized genAuthConfig

genAuthConfig :: Int -> Gen AuthConfig
genAuthConfig n =
  AuthConfig
    <$> arbitraryReducedMaybe n -- authConfigUsername :: Maybe Text
    <*> arbitraryReducedMaybe n -- authConfigPassword :: Maybe Text
    <*> arbitraryReducedMaybe n -- authConfigEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- authConfigServeraddress :: Maybe Text
  
instance Arbitrary BuildInfo where
  arbitrary = sized genBuildInfo

genBuildInfo :: Int -> Gen BuildInfo
genBuildInfo n =
  BuildInfo
    <$> arbitraryReducedMaybe n -- buildInfoId :: Maybe Text
    <*> arbitraryReducedMaybe n -- buildInfoStream :: Maybe Text
    <*> arbitraryReducedMaybe n -- buildInfoError :: Maybe Text
    <*> arbitraryReducedMaybe n -- buildInfoErrorDetail :: Maybe ErrorDetail
    <*> arbitraryReducedMaybe n -- buildInfoStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- buildInfoProgress :: Maybe Text
    <*> arbitraryReducedMaybe n -- buildInfoProgressDetail :: Maybe ProgressDetail
    <*> arbitraryReducedMaybe n -- buildInfoAux :: Maybe ImageID
  
instance Arbitrary BuildPruneResponse where
  arbitrary = sized genBuildPruneResponse

genBuildPruneResponse :: Int -> Gen BuildPruneResponse
genBuildPruneResponse n =
  BuildPruneResponse
    <$> arbitraryReducedMaybe n -- buildPruneResponseSpaceReclaimed :: Maybe Integer
  
instance Arbitrary ClusterInfo where
  arbitrary = sized genClusterInfo

genClusterInfo :: Int -> Gen ClusterInfo
genClusterInfo n =
  ClusterInfo
    <$> arbitraryReducedMaybe n -- clusterInfoId :: Maybe Text
    <*> arbitraryReducedMaybe n -- clusterInfoVersion :: Maybe ObjectVersion
    <*> arbitraryReducedMaybe n -- clusterInfoCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- clusterInfoUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- clusterInfoSpec :: Maybe SwarmSpec
    <*> arbitraryReducedMaybe n -- clusterInfoTlsInfo :: Maybe TLSInfo
    <*> arbitraryReducedMaybe n -- clusterInfoRootRotationInProgress :: Maybe Bool
  
instance Arbitrary Commit where
  arbitrary = sized genCommit

genCommit :: Int -> Gen Commit
genCommit n =
  Commit
    <$> arbitraryReducedMaybe n -- commitId :: Maybe Text
    <*> arbitraryReducedMaybe n -- commitExpected :: Maybe Text
  
instance Arbitrary Config where
  arbitrary = sized genConfig

genConfig :: Int -> Gen Config
genConfig n =
  Config
    <$> arbitraryReducedMaybe n -- configId :: Maybe Text
    <*> arbitraryReducedMaybe n -- configVersion :: Maybe ObjectVersion
    <*> arbitraryReducedMaybe n -- configCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- configUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- configSpec :: Maybe ConfigSpec
  
instance Arbitrary ConfigCreateRequest where
  arbitrary = sized genConfigCreateRequest

genConfigCreateRequest :: Int -> Gen ConfigCreateRequest
genConfigCreateRequest n =
  ConfigCreateRequest
    <$> arbitraryReducedMaybe n -- configCreateRequestName :: Maybe Text
    <*> arbitraryReducedMaybe n -- configCreateRequestLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- configCreateRequestData :: Maybe Text
  
instance Arbitrary ConfigSpec where
  arbitrary = sized genConfigSpec

genConfigSpec :: Int -> Gen ConfigSpec
genConfigSpec n =
  ConfigSpec
    <$> arbitraryReducedMaybe n -- configSpecName :: Maybe Text
    <*> arbitraryReducedMaybe n -- configSpecLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- configSpecData :: Maybe Text
  
instance Arbitrary ContainerChangeResponseItem where
  arbitrary = sized genContainerChangeResponseItem

genContainerChangeResponseItem :: Int -> Gen ContainerChangeResponseItem
genContainerChangeResponseItem n =
  ContainerChangeResponseItem
    <$> arbitrary -- containerChangeResponseItemPath :: Text
    <*> arbitrary -- containerChangeResponseItemKind :: Int
  
instance Arbitrary ContainerConfig where
  arbitrary = sized genContainerConfig

genContainerConfig :: Int -> Gen ContainerConfig
genContainerConfig n =
  ContainerConfig
    <$> arbitraryReducedMaybe n -- containerConfigHostname :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerConfigDomainname :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerConfigUser :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerConfigAttachStdin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerConfigAttachStdout :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerConfigAttachStderr :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerConfigExposedPorts :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- containerConfigTty :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerConfigOpenStdin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerConfigStdinOnce :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerConfigEnv :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerConfigCmd :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerConfigHealthcheck :: Maybe HealthConfig
    <*> arbitraryReducedMaybe n -- containerConfigArgsEscaped :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerConfigImage :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerConfigVolumes :: Maybe ContainerConfigVolumes
    <*> arbitraryReducedMaybe n -- containerConfigWorkingDir :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerConfigEntrypoint :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerConfigNetworkDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerConfigMacAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerConfigOnBuild :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerConfigLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- containerConfigStopSignal :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerConfigStopTimeout :: Maybe Int
    <*> arbitraryReducedMaybe n -- containerConfigShell :: Maybe [Text]
  
instance Arbitrary ContainerConfigVolumes where
  arbitrary = sized genContainerConfigVolumes

genContainerConfigVolumes :: Int -> Gen ContainerConfigVolumes
genContainerConfigVolumes n =
  ContainerConfigVolumes
    <$> arbitraryReducedMaybeValue n -- containerConfigVolumesAdditionalProperties :: Maybe A.Value
  
instance Arbitrary ContainerCreateRequest where
  arbitrary = sized genContainerCreateRequest

genContainerCreateRequest :: Int -> Gen ContainerCreateRequest
genContainerCreateRequest n =
  ContainerCreateRequest
    <$> arbitraryReducedMaybe n -- containerCreateRequestHostname :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerCreateRequestDomainname :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerCreateRequestUser :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerCreateRequestAttachStdin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerCreateRequestAttachStdout :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerCreateRequestAttachStderr :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerCreateRequestExposedPorts :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- containerCreateRequestTty :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerCreateRequestOpenStdin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerCreateRequestStdinOnce :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerCreateRequestEnv :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerCreateRequestCmd :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerCreateRequestHealthcheck :: Maybe HealthConfig
    <*> arbitraryReducedMaybe n -- containerCreateRequestArgsEscaped :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerCreateRequestImage :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerCreateRequestVolumes :: Maybe ContainerConfigVolumes
    <*> arbitraryReducedMaybe n -- containerCreateRequestWorkingDir :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerCreateRequestEntrypoint :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerCreateRequestNetworkDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerCreateRequestMacAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerCreateRequestOnBuild :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerCreateRequestLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- containerCreateRequestStopSignal :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerCreateRequestStopTimeout :: Maybe Int
    <*> arbitraryReducedMaybe n -- containerCreateRequestShell :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerCreateRequestHostConfig :: Maybe HostConfig
    <*> arbitraryReducedMaybe n -- containerCreateRequestNetworkingConfig :: Maybe ContainerCreateRequestAllOfNetworkingConfig
  
instance Arbitrary ContainerCreateRequestAllOfNetworkingConfig where
  arbitrary = sized genContainerCreateRequestAllOfNetworkingConfig

genContainerCreateRequestAllOfNetworkingConfig :: Int -> Gen ContainerCreateRequestAllOfNetworkingConfig
genContainerCreateRequestAllOfNetworkingConfig n =
  ContainerCreateRequestAllOfNetworkingConfig
    <$> arbitraryReducedMaybe n -- containerCreateRequestAllOfNetworkingConfigEndpointsConfig :: Maybe (Map.Map String EndpointSettings)
  
instance Arbitrary ContainerCreateResponse where
  arbitrary = sized genContainerCreateResponse

genContainerCreateResponse :: Int -> Gen ContainerCreateResponse
genContainerCreateResponse n =
  ContainerCreateResponse
    <$> arbitrary -- containerCreateResponseId :: Text
    <*> arbitrary -- containerCreateResponseWarnings :: [Text]
  
instance Arbitrary ContainerExecRequest where
  arbitrary = sized genContainerExecRequest

genContainerExecRequest :: Int -> Gen ContainerExecRequest
genContainerExecRequest n =
  ContainerExecRequest
    <$> arbitraryReducedMaybe n -- containerExecRequestAttachStdin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerExecRequestAttachStdout :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerExecRequestAttachStderr :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerExecRequestDetachKeys :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerExecRequestTty :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerExecRequestEnv :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerExecRequestCmd :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerExecRequestPrivileged :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerExecRequestUser :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerExecRequestWorkingDir :: Maybe Text
  
instance Arbitrary ContainerInspectResponse where
  arbitrary = sized genContainerInspectResponse

genContainerInspectResponse :: Int -> Gen ContainerInspectResponse
genContainerInspectResponse n =
  ContainerInspectResponse
    <$> arbitraryReducedMaybe n -- containerInspectResponseId :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponseCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponsePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponseArgs :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerInspectResponseState :: Maybe ContainerInspectResponseState
    <*> arbitraryReducedMaybe n -- containerInspectResponseImage :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponseResolvConfPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponseHostnamePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponseHostsPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponseLogPath :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- containerInspectResponseNode :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- containerInspectResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponseRestartCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- containerInspectResponseDriver :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponseMountLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponseProcessLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponseAppArmorProfile :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponseExecIds :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponseHostConfig :: Maybe HostConfig
    <*> arbitraryReducedMaybe n -- containerInspectResponseGraphDriver :: Maybe GraphDriverData
    <*> arbitraryReducedMaybe n -- containerInspectResponseSizeRw :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerInspectResponseSizeRootFs :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerInspectResponseMounts :: Maybe [MountPoint]
    <*> arbitraryReducedMaybe n -- containerInspectResponseConfig :: Maybe ContainerConfig
    <*> arbitraryReducedMaybe n -- containerInspectResponseNetworkSettings :: Maybe NetworkSettings
  
instance Arbitrary ContainerInspectResponseState where
  arbitrary = sized genContainerInspectResponseState

genContainerInspectResponseState :: Int -> Gen ContainerInspectResponseState
genContainerInspectResponseState n =
  ContainerInspectResponseState
    <$> arbitraryReducedMaybe n -- containerInspectResponseStateStatus :: Maybe E'Status
    <*> arbitraryReducedMaybe n -- containerInspectResponseStateRunning :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerInspectResponseStatePaused :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerInspectResponseStateRestarting :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerInspectResponseStateOomKilled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerInspectResponseStateDead :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerInspectResponseStatePid :: Maybe Int
    <*> arbitraryReducedMaybe n -- containerInspectResponseStateExitCode :: Maybe Int
    <*> arbitraryReducedMaybe n -- containerInspectResponseStateError :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponseStateStartedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerInspectResponseStateFinishedAt :: Maybe Text
  
instance Arbitrary ContainerPruneResponse where
  arbitrary = sized genContainerPruneResponse

genContainerPruneResponse :: Int -> Gen ContainerPruneResponse
genContainerPruneResponse n =
  ContainerPruneResponse
    <$> arbitraryReducedMaybe n -- containerPruneResponseContainersDeleted :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerPruneResponseSpaceReclaimed :: Maybe Integer
  
instance Arbitrary ContainerSummary where
  arbitrary = sized genContainerSummary

genContainerSummary :: Int -> Gen ContainerSummary
genContainerSummary n =
  ContainerSummary
    <$> arbitraryReducedMaybe n -- containerSummaryId :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerSummaryNames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerSummaryImage :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerSummaryImageId :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerSummaryCommand :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerSummaryCreated :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerSummaryPorts :: Maybe [Port]
    <*> arbitraryReducedMaybe n -- containerSummarySizeRw :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerSummarySizeRootFs :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerSummaryLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- containerSummaryState :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerSummaryStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerSummaryHostConfig :: Maybe ContainerSummaryHostConfig
    <*> arbitraryReducedMaybe n -- containerSummaryNetworkSettings :: Maybe ContainerSummaryNetworkSettings
    <*> arbitraryReducedMaybe n -- containerSummaryMounts :: Maybe [MountPoint]
  
instance Arbitrary ContainerSummaryHostConfig where
  arbitrary = sized genContainerSummaryHostConfig

genContainerSummaryHostConfig :: Int -> Gen ContainerSummaryHostConfig
genContainerSummaryHostConfig n =
  ContainerSummaryHostConfig
    <$> arbitraryReducedMaybe n -- containerSummaryHostConfigNetworkMode :: Maybe Text
  
instance Arbitrary ContainerSummaryNetworkSettings where
  arbitrary = sized genContainerSummaryNetworkSettings

genContainerSummaryNetworkSettings :: Int -> Gen ContainerSummaryNetworkSettings
genContainerSummaryNetworkSettings n =
  ContainerSummaryNetworkSettings
    <$> arbitraryReducedMaybe n -- containerSummaryNetworkSettingsNetworks :: Maybe (Map.Map String EndpointSettings)
  
instance Arbitrary ContainerTopResponse where
  arbitrary = sized genContainerTopResponse

genContainerTopResponse :: Int -> Gen ContainerTopResponse
genContainerTopResponse n =
  ContainerTopResponse
    <$> arbitraryReducedMaybe n -- containerTopResponseTitles :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerTopResponseProcesses :: Maybe [[Text]]
  
instance Arbitrary ContainerUpdateRequest where
  arbitrary = sized genContainerUpdateRequest

genContainerUpdateRequest :: Int -> Gen ContainerUpdateRequest
genContainerUpdateRequest n =
  ContainerUpdateRequest
    <$> arbitraryReducedMaybe n -- containerUpdateRequestCpuShares :: Maybe Int
    <*> arbitraryReducedMaybe n -- containerUpdateRequestMemory :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestCgroupParent :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerUpdateRequestBlkioWeight :: Maybe Int
    <*> arbitraryReducedMaybe n -- containerUpdateRequestBlkioWeightDevice :: Maybe [ResourcesBlkioWeightDeviceInner]
    <*> arbitraryReducedMaybe n -- containerUpdateRequestBlkioDeviceReadBps :: Maybe [ThrottleDevice]
    <*> arbitraryReducedMaybe n -- containerUpdateRequestBlkioDeviceWriteBps :: Maybe [ThrottleDevice]
    <*> arbitraryReducedMaybe n -- containerUpdateRequestBlkioDeviceReadIOps :: Maybe [ThrottleDevice]
    <*> arbitraryReducedMaybe n -- containerUpdateRequestBlkioDeviceWriteIOps :: Maybe [ThrottleDevice]
    <*> arbitraryReducedMaybe n -- containerUpdateRequestCpuPeriod :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestCpuQuota :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestCpuRealtimePeriod :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestCpuRealtimeRuntime :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestCpusetCpus :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerUpdateRequestCpusetMems :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerUpdateRequestDevices :: Maybe [DeviceMapping]
    <*> arbitraryReducedMaybe n -- containerUpdateRequestDeviceCgroupRules :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- containerUpdateRequestDiskQuota :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestKernelMemory :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestMemoryReservation :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestMemorySwap :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestMemorySwappiness :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestNanoCpus :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestOomKillDisable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerUpdateRequestPidsLimit :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestUlimits :: Maybe [ResourcesUlimitsInner]
    <*> arbitraryReducedMaybe n -- containerUpdateRequestCpuCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestCpuPercent :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestIoMaximumIOps :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestIoMaximumBandwidth :: Maybe Integer
    <*> arbitraryReducedMaybe n -- containerUpdateRequestRestartPolicy :: Maybe RestartPolicy
  
instance Arbitrary ContainerUpdateResponse where
  arbitrary = sized genContainerUpdateResponse

genContainerUpdateResponse :: Int -> Gen ContainerUpdateResponse
genContainerUpdateResponse n =
  ContainerUpdateResponse
    <$> arbitraryReducedMaybe n -- containerUpdateResponseWarnings :: Maybe [Text]
  
instance Arbitrary ContainerWaitResponse where
  arbitrary = sized genContainerWaitResponse

genContainerWaitResponse :: Int -> Gen ContainerWaitResponse
genContainerWaitResponse n =
  ContainerWaitResponse
    <$> arbitrary -- containerWaitResponseStatusCode :: Int
    <*> arbitraryReducedMaybe n -- containerWaitResponseError :: Maybe ContainerWaitResponseError
  
instance Arbitrary ContainerWaitResponseError where
  arbitrary = sized genContainerWaitResponseError

genContainerWaitResponseError :: Int -> Gen ContainerWaitResponseError
genContainerWaitResponseError n =
  ContainerWaitResponseError
    <$> arbitraryReducedMaybe n -- containerWaitResponseErrorMessage :: Maybe Text
  
instance Arbitrary CreateImageInfo where
  arbitrary = sized genCreateImageInfo

genCreateImageInfo :: Int -> Gen CreateImageInfo
genCreateImageInfo n =
  CreateImageInfo
    <$> arbitraryReducedMaybe n -- createImageInfoId :: Maybe Text
    <*> arbitraryReducedMaybe n -- createImageInfoError :: Maybe Text
    <*> arbitraryReducedMaybe n -- createImageInfoStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- createImageInfoProgress :: Maybe Text
    <*> arbitraryReducedMaybe n -- createImageInfoProgressDetail :: Maybe ProgressDetail
  
instance Arbitrary DeviceMapping where
  arbitrary = sized genDeviceMapping

genDeviceMapping :: Int -> Gen DeviceMapping
genDeviceMapping n =
  DeviceMapping
    <$> arbitraryReducedMaybe n -- deviceMappingPathOnHost :: Maybe Text
    <*> arbitraryReducedMaybe n -- deviceMappingPathInContainer :: Maybe Text
    <*> arbitraryReducedMaybe n -- deviceMappingCgroupPermissions :: Maybe Text
  
instance Arbitrary DistributionInspectResponse where
  arbitrary = sized genDistributionInspectResponse

genDistributionInspectResponse :: Int -> Gen DistributionInspectResponse
genDistributionInspectResponse n =
  DistributionInspectResponse
    <$> arbitraryReduced n -- distributionInspectResponseDescriptor :: DistributionInspectResponseDescriptor
    <*> arbitraryReduced n -- distributionInspectResponsePlatforms :: [DistributionInspectResponsePlatformsInner]
  
instance Arbitrary DistributionInspectResponseDescriptor where
  arbitrary = sized genDistributionInspectResponseDescriptor

genDistributionInspectResponseDescriptor :: Int -> Gen DistributionInspectResponseDescriptor
genDistributionInspectResponseDescriptor n =
  DistributionInspectResponseDescriptor
    <$> arbitraryReducedMaybe n -- distributionInspectResponseDescriptorMediaType :: Maybe Text
    <*> arbitraryReducedMaybe n -- distributionInspectResponseDescriptorSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- distributionInspectResponseDescriptorDigest :: Maybe Text
    <*> arbitraryReducedMaybe n -- distributionInspectResponseDescriptorUrls :: Maybe [Text]
  
instance Arbitrary DistributionInspectResponsePlatformsInner where
  arbitrary = sized genDistributionInspectResponsePlatformsInner

genDistributionInspectResponsePlatformsInner :: Int -> Gen DistributionInspectResponsePlatformsInner
genDistributionInspectResponsePlatformsInner n =
  DistributionInspectResponsePlatformsInner
    <$> arbitraryReducedMaybe n -- distributionInspectResponsePlatformsInnerArchitecture :: Maybe Text
    <*> arbitraryReducedMaybe n -- distributionInspectResponsePlatformsInnerOs :: Maybe Text
    <*> arbitraryReducedMaybe n -- distributionInspectResponsePlatformsInnerOsVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- distributionInspectResponsePlatformsInnerOsFeatures :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- distributionInspectResponsePlatformsInnerVariant :: Maybe Text
    <*> arbitraryReducedMaybe n -- distributionInspectResponsePlatformsInnerFeatures :: Maybe [Text]
  
instance Arbitrary Driver where
  arbitrary = sized genDriver

genDriver :: Int -> Gen Driver
genDriver n =
  Driver
    <$> arbitrary -- driverName :: Text
    <*> arbitraryReducedMaybe n -- driverOptions :: Maybe (Map.Map String Text)
  
instance Arbitrary EndpointIPAMConfig where
  arbitrary = sized genEndpointIPAMConfig

genEndpointIPAMConfig :: Int -> Gen EndpointIPAMConfig
genEndpointIPAMConfig n =
  EndpointIPAMConfig
    <$> arbitraryReducedMaybe n -- endpointIPAMConfigIpv4Address :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointIPAMConfigIpv6Address :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointIPAMConfigLinkLocalIps :: Maybe [Text]
  
instance Arbitrary EndpointPortConfig where
  arbitrary = sized genEndpointPortConfig

genEndpointPortConfig :: Int -> Gen EndpointPortConfig
genEndpointPortConfig n =
  EndpointPortConfig
    <$> arbitraryReducedMaybe n -- endpointPortConfigName :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointPortConfigProtocol :: Maybe E'Type
    <*> arbitraryReducedMaybe n -- endpointPortConfigTargetPort :: Maybe Int
    <*> arbitraryReducedMaybe n -- endpointPortConfigPublishedPort :: Maybe Int
    <*> arbitraryReducedMaybe n -- endpointPortConfigPublishMode :: Maybe E'PublishMode
  
instance Arbitrary EndpointSettings where
  arbitrary = sized genEndpointSettings

genEndpointSettings :: Int -> Gen EndpointSettings
genEndpointSettings n =
  EndpointSettings
    <$> arbitraryReducedMaybe n -- endpointSettingsIpamConfig :: Maybe EndpointIPAMConfig
    <*> arbitraryReducedMaybe n -- endpointSettingsLinks :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- endpointSettingsAliases :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- endpointSettingsNetworkId :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsEndpointId :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsGateway :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsIpAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsIpPrefixLen :: Maybe Int
    <*> arbitraryReducedMaybe n -- endpointSettingsIpv6Gateway :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsGlobalIpv6Address :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsGlobalIpv6PrefixLen :: Maybe Integer
    <*> arbitraryReducedMaybe n -- endpointSettingsMacAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsDriverOpts :: Maybe (Map.Map String Text)
  
instance Arbitrary EndpointSpec where
  arbitrary = sized genEndpointSpec

genEndpointSpec :: Int -> Gen EndpointSpec
genEndpointSpec n =
  EndpointSpec
    <$> arbitraryReducedMaybe n -- endpointSpecMode :: Maybe E'Mode
    <*> arbitraryReducedMaybe n -- endpointSpecPorts :: Maybe [EndpointPortConfig]
  
instance Arbitrary EngineDescription where
  arbitrary = sized genEngineDescription

genEngineDescription :: Int -> Gen EngineDescription
genEngineDescription n =
  EngineDescription
    <$> arbitraryReducedMaybe n -- engineDescriptionEngineVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- engineDescriptionLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- engineDescriptionPlugins :: Maybe [EngineDescriptionPluginsInner]
  
instance Arbitrary EngineDescriptionPluginsInner where
  arbitrary = sized genEngineDescriptionPluginsInner

genEngineDescriptionPluginsInner :: Int -> Gen EngineDescriptionPluginsInner
genEngineDescriptionPluginsInner n =
  EngineDescriptionPluginsInner
    <$> arbitraryReducedMaybe n -- engineDescriptionPluginsInnerType :: Maybe Text
    <*> arbitraryReducedMaybe n -- engineDescriptionPluginsInnerName :: Maybe Text
  
instance Arbitrary ErrorDetail where
  arbitrary = sized genErrorDetail

genErrorDetail :: Int -> Gen ErrorDetail
genErrorDetail n =
  ErrorDetail
    <$> arbitraryReducedMaybe n -- errorDetailCode :: Maybe Int
    <*> arbitraryReducedMaybe n -- errorDetailMessage :: Maybe Text
  
instance Arbitrary ErrorResponse where
  arbitrary = sized genErrorResponse

genErrorResponse :: Int -> Gen ErrorResponse
genErrorResponse n =
  ErrorResponse
    <$> arbitrary -- errorResponseMessage :: Text
  
instance Arbitrary ExecInspectResponse where
  arbitrary = sized genExecInspectResponse

genExecInspectResponse :: Int -> Gen ExecInspectResponse
genExecInspectResponse n =
  ExecInspectResponse
    <$> arbitraryReducedMaybe n -- execInspectResponseCanRemove :: Maybe Bool
    <*> arbitraryReducedMaybe n -- execInspectResponseDetachKeys :: Maybe Text
    <*> arbitraryReducedMaybe n -- execInspectResponseId :: Maybe Text
    <*> arbitraryReducedMaybe n -- execInspectResponseRunning :: Maybe Bool
    <*> arbitraryReducedMaybe n -- execInspectResponseExitCode :: Maybe Int
    <*> arbitraryReducedMaybe n -- execInspectResponseProcessConfig :: Maybe ProcessConfig
    <*> arbitraryReducedMaybe n -- execInspectResponseOpenStdin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- execInspectResponseOpenStderr :: Maybe Bool
    <*> arbitraryReducedMaybe n -- execInspectResponseOpenStdout :: Maybe Bool
    <*> arbitraryReducedMaybe n -- execInspectResponseContainerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- execInspectResponsePid :: Maybe Int
  
instance Arbitrary ExecStartRequest where
  arbitrary = sized genExecStartRequest

genExecStartRequest :: Int -> Gen ExecStartRequest
genExecStartRequest n =
  ExecStartRequest
    <$> arbitraryReducedMaybe n -- execStartRequestDetach :: Maybe Bool
    <*> arbitraryReducedMaybe n -- execStartRequestTty :: Maybe Bool
  
instance Arbitrary GenericResourcesInner where
  arbitrary = sized genGenericResourcesInner

genGenericResourcesInner :: Int -> Gen GenericResourcesInner
genGenericResourcesInner n =
  GenericResourcesInner
    <$> arbitraryReducedMaybe n -- genericResourcesInnerNamedResourceSpec :: Maybe GenericResourcesInnerNamedResourceSpec
    <*> arbitraryReducedMaybe n -- genericResourcesInnerDiscreteResourceSpec :: Maybe GenericResourcesInnerDiscreteResourceSpec
  
instance Arbitrary GenericResourcesInnerDiscreteResourceSpec where
  arbitrary = sized genGenericResourcesInnerDiscreteResourceSpec

genGenericResourcesInnerDiscreteResourceSpec :: Int -> Gen GenericResourcesInnerDiscreteResourceSpec
genGenericResourcesInnerDiscreteResourceSpec n =
  GenericResourcesInnerDiscreteResourceSpec
    <$> arbitraryReducedMaybe n -- genericResourcesInnerDiscreteResourceSpecKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- genericResourcesInnerDiscreteResourceSpecValue :: Maybe Integer
  
instance Arbitrary GenericResourcesInnerNamedResourceSpec where
  arbitrary = sized genGenericResourcesInnerNamedResourceSpec

genGenericResourcesInnerNamedResourceSpec :: Int -> Gen GenericResourcesInnerNamedResourceSpec
genGenericResourcesInnerNamedResourceSpec n =
  GenericResourcesInnerNamedResourceSpec
    <$> arbitraryReducedMaybe n -- genericResourcesInnerNamedResourceSpecKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- genericResourcesInnerNamedResourceSpecValue :: Maybe Text
  
instance Arbitrary GraphDriverData where
  arbitrary = sized genGraphDriverData

genGraphDriverData :: Int -> Gen GraphDriverData
genGraphDriverData n =
  GraphDriverData
    <$> arbitrary -- graphDriverDataName :: Text
    <*> arbitrary -- graphDriverDataData :: (Map.Map String Text)
  
instance Arbitrary HealthConfig where
  arbitrary = sized genHealthConfig

genHealthConfig :: Int -> Gen HealthConfig
genHealthConfig n =
  HealthConfig
    <$> arbitraryReducedMaybe n -- healthConfigTest :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- healthConfigInterval :: Maybe Int
    <*> arbitraryReducedMaybe n -- healthConfigTimeout :: Maybe Int
    <*> arbitraryReducedMaybe n -- healthConfigRetries :: Maybe Int
    <*> arbitraryReducedMaybe n -- healthConfigStartPeriod :: Maybe Int
  
instance Arbitrary HistoryResponseItem where
  arbitrary = sized genHistoryResponseItem

genHistoryResponseItem :: Int -> Gen HistoryResponseItem
genHistoryResponseItem n =
  HistoryResponseItem
    <$> arbitrary -- historyResponseItemId :: Text
    <*> arbitrary -- historyResponseItemCreated :: Integer
    <*> arbitrary -- historyResponseItemCreatedBy :: Text
    <*> arbitrary -- historyResponseItemTags :: [Text]
    <*> arbitrary -- historyResponseItemSize :: Integer
    <*> arbitrary -- historyResponseItemComment :: Text
  
instance Arbitrary HostConfig where
  arbitrary = sized genHostConfig

genHostConfig :: Int -> Gen HostConfig
genHostConfig n =
  HostConfig
    <$> arbitraryReducedMaybe n -- hostConfigCpuShares :: Maybe Int
    <*> arbitraryReducedMaybe n -- hostConfigMemory :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigCgroupParent :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigBlkioWeight :: Maybe Int
    <*> arbitraryReducedMaybe n -- hostConfigBlkioWeightDevice :: Maybe [ResourcesBlkioWeightDeviceInner]
    <*> arbitraryReducedMaybe n -- hostConfigBlkioDeviceReadBps :: Maybe [ThrottleDevice]
    <*> arbitraryReducedMaybe n -- hostConfigBlkioDeviceWriteBps :: Maybe [ThrottleDevice]
    <*> arbitraryReducedMaybe n -- hostConfigBlkioDeviceReadIOps :: Maybe [ThrottleDevice]
    <*> arbitraryReducedMaybe n -- hostConfigBlkioDeviceWriteIOps :: Maybe [ThrottleDevice]
    <*> arbitraryReducedMaybe n -- hostConfigCpuPeriod :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigCpuQuota :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigCpuRealtimePeriod :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigCpuRealtimeRuntime :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigCpusetCpus :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigCpusetMems :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigDevices :: Maybe [DeviceMapping]
    <*> arbitraryReducedMaybe n -- hostConfigDeviceCgroupRules :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigDiskQuota :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigKernelMemory :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigMemoryReservation :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigMemorySwap :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigMemorySwappiness :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigNanoCpus :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigOomKillDisable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- hostConfigPidsLimit :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigUlimits :: Maybe [ResourcesUlimitsInner]
    <*> arbitraryReducedMaybe n -- hostConfigCpuCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigCpuPercent :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigIoMaximumIOps :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigIoMaximumBandwidth :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigBinds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigContainerIdFile :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigLogConfig :: Maybe HostConfigAllOfLogConfig
    <*> arbitraryReducedMaybe n -- hostConfigNetworkMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigPortBindings :: Maybe (Map.Map String [PortBinding])
    <*> arbitraryReducedMaybe n -- hostConfigRestartPolicy :: Maybe RestartPolicy
    <*> arbitraryReducedMaybe n -- hostConfigAutoRemove :: Maybe Bool
    <*> arbitraryReducedMaybe n -- hostConfigVolumeDriver :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigVolumesFrom :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigMounts :: Maybe [Mount]
    <*> arbitraryReducedMaybe n -- hostConfigCapAdd :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigCapDrop :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigDns :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigDnsOptions :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigDnsSearch :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigExtraHosts :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigGroupAdd :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigIpcMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigCgroup :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigLinks :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigOomScoreAdj :: Maybe Int
    <*> arbitraryReducedMaybe n -- hostConfigPidMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigPrivileged :: Maybe Bool
    <*> arbitraryReducedMaybe n -- hostConfigPublishAllPorts :: Maybe Bool
    <*> arbitraryReducedMaybe n -- hostConfigReadonlyRootfs :: Maybe Bool
    <*> arbitraryReducedMaybe n -- hostConfigSecurityOpt :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigStorageOpt :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- hostConfigTmpfs :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- hostConfigUtsMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigUsernsMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigShmSize :: Maybe Int
    <*> arbitraryReducedMaybe n -- hostConfigSysctls :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- hostConfigRuntime :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigConsoleSize :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- hostConfigIsolation :: Maybe E'Isolation
  
instance Arbitrary HostConfigAllOfLogConfig where
  arbitrary = sized genHostConfigAllOfLogConfig

genHostConfigAllOfLogConfig :: Int -> Gen HostConfigAllOfLogConfig
genHostConfigAllOfLogConfig n =
  HostConfigAllOfLogConfig
    <$> arbitraryReducedMaybe n -- hostConfigAllOfLogConfigType :: Maybe E'Type4
    <*> arbitraryReducedMaybe n -- hostConfigAllOfLogConfigConfig :: Maybe (Map.Map String Text)
  
instance Arbitrary IPAM where
  arbitrary = sized genIPAM

genIPAM :: Int -> Gen IPAM
genIPAM n =
  IPAM
    <$> arbitraryReducedMaybe n -- iPAMDriver :: Maybe Text
    <*> arbitraryReducedMaybe n -- iPAMConfig :: Maybe [(Map.Map String Text)]
    <*> arbitraryReducedMaybe n -- iPAMOptions :: Maybe [(Map.Map String Text)]
  
instance Arbitrary IdResponse where
  arbitrary = sized genIdResponse

genIdResponse :: Int -> Gen IdResponse
genIdResponse n =
  IdResponse
    <$> arbitrary -- idResponseId :: Text
  
instance Arbitrary Image where
  arbitrary = sized genImage

genImage :: Int -> Gen Image
genImage n =
  Image
    <$> arbitrary -- imageId :: Text
    <*> arbitraryReducedMaybe n -- imageRepoTags :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- imageRepoDigests :: Maybe [Text]
    <*> arbitrary -- imageParent :: Text
    <*> arbitrary -- imageComment :: Text
    <*> arbitrary -- imageCreated :: Text
    <*> arbitrary -- imageContainer :: Text
    <*> arbitraryReducedMaybe n -- imageContainerConfig :: Maybe ContainerConfig
    <*> arbitrary -- imageDockerVersion :: Text
    <*> arbitrary -- imageAuthor :: Text
    <*> arbitraryReducedMaybe n -- imageConfig :: Maybe ContainerConfig
    <*> arbitrary -- imageArchitecture :: Text
    <*> arbitrary -- imageOs :: Text
    <*> arbitraryReducedMaybe n -- imageOsVersion :: Maybe Text
    <*> arbitrary -- imageSize :: Integer
    <*> arbitrary -- imageVirtualSize :: Integer
    <*> arbitraryReduced n -- imageGraphDriver :: GraphDriverData
    <*> arbitraryReduced n -- imageRootFs :: ImageRootFS
    <*> arbitraryReducedMaybe n -- imageMetadata :: Maybe ImageMetadata
  
instance Arbitrary ImageDeleteResponseItem where
  arbitrary = sized genImageDeleteResponseItem

genImageDeleteResponseItem :: Int -> Gen ImageDeleteResponseItem
genImageDeleteResponseItem n =
  ImageDeleteResponseItem
    <$> arbitraryReducedMaybe n -- imageDeleteResponseItemUntagged :: Maybe Text
    <*> arbitraryReducedMaybe n -- imageDeleteResponseItemDeleted :: Maybe Text
  
instance Arbitrary ImageID where
  arbitrary = sized genImageID

genImageID :: Int -> Gen ImageID
genImageID n =
  ImageID
    <$> arbitraryReducedMaybe n -- imageIDId :: Maybe Text
  
instance Arbitrary ImageMetadata where
  arbitrary = sized genImageMetadata

genImageMetadata :: Int -> Gen ImageMetadata
genImageMetadata n =
  ImageMetadata
    <$> arbitraryReducedMaybe n -- imageMetadataLastTagTime :: Maybe Text
  
instance Arbitrary ImagePruneResponse where
  arbitrary = sized genImagePruneResponse

genImagePruneResponse :: Int -> Gen ImagePruneResponse
genImagePruneResponse n =
  ImagePruneResponse
    <$> arbitraryReducedMaybe n -- imagePruneResponseImagesDeleted :: Maybe [ImageDeleteResponseItem]
    <*> arbitraryReducedMaybe n -- imagePruneResponseSpaceReclaimed :: Maybe Integer
  
instance Arbitrary ImageRootFS where
  arbitrary = sized genImageRootFS

genImageRootFS :: Int -> Gen ImageRootFS
genImageRootFS n =
  ImageRootFS
    <$> arbitrary -- imageRootFSType :: Text
    <*> arbitraryReducedMaybe n -- imageRootFSLayers :: Maybe [Text]
  
instance Arbitrary ImageSearchResponseItem where
  arbitrary = sized genImageSearchResponseItem

genImageSearchResponseItem :: Int -> Gen ImageSearchResponseItem
genImageSearchResponseItem n =
  ImageSearchResponseItem
    <$> arbitraryReducedMaybe n -- imageSearchResponseItemDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- imageSearchResponseItemIsOfficial :: Maybe Bool
    <*> arbitraryReducedMaybe n -- imageSearchResponseItemIsAutomated :: Maybe Bool
    <*> arbitraryReducedMaybe n -- imageSearchResponseItemName :: Maybe Text
    <*> arbitraryReducedMaybe n -- imageSearchResponseItemStarCount :: Maybe Int
  
instance Arbitrary ImageSummary where
  arbitrary = sized genImageSummary

genImageSummary :: Int -> Gen ImageSummary
genImageSummary n =
  ImageSummary
    <$> arbitrary -- imageSummaryId :: Text
    <*> arbitrary -- imageSummaryParentId :: Text
    <*> arbitrary -- imageSummaryRepoTags :: [Text]
    <*> arbitrary -- imageSummaryRepoDigests :: [Text]
    <*> arbitrary -- imageSummaryCreated :: Int
    <*> arbitrary -- imageSummarySize :: Int
    <*> arbitrary -- imageSummarySharedSize :: Int
    <*> arbitrary -- imageSummaryVirtualSize :: Int
    <*> arbitrary -- imageSummaryLabels :: (Map.Map String Text)
    <*> arbitrary -- imageSummaryContainers :: Int
  
instance Arbitrary IndexInfo where
  arbitrary = sized genIndexInfo

genIndexInfo :: Int -> Gen IndexInfo
genIndexInfo n =
  IndexInfo
    <$> arbitraryReducedMaybe n -- indexInfoName :: Maybe Text
    <*> arbitraryReducedMaybe n -- indexInfoMirrors :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- indexInfoSecure :: Maybe Bool
    <*> arbitraryReducedMaybe n -- indexInfoOfficial :: Maybe Bool
  
instance Arbitrary JoinTokens where
  arbitrary = sized genJoinTokens

genJoinTokens :: Int -> Gen JoinTokens
genJoinTokens n =
  JoinTokens
    <$> arbitraryReducedMaybe n -- joinTokensWorker :: Maybe Text
    <*> arbitraryReducedMaybe n -- joinTokensManager :: Maybe Text
  
instance Arbitrary ManagerStatus where
  arbitrary = sized genManagerStatus

genManagerStatus :: Int -> Gen ManagerStatus
genManagerStatus n =
  ManagerStatus
    <$> arbitraryReducedMaybe n -- managerStatusLeader :: Maybe Bool
    <*> arbitraryReducedMaybe n -- managerStatusReachability :: Maybe Reachability
    <*> arbitraryReducedMaybe n -- managerStatusAddr :: Maybe Text
  
instance Arbitrary Mount where
  arbitrary = sized genMount

genMount :: Int -> Gen Mount
genMount n =
  Mount
    <$> arbitraryReducedMaybe n -- mountTarget :: Maybe Text
    <*> arbitraryReducedMaybe n -- mountSource :: Maybe Text
    <*> arbitraryReducedMaybe n -- mountType :: Maybe E'Type3
    <*> arbitraryReducedMaybe n -- mountReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- mountConsistency :: Maybe Text
    <*> arbitraryReducedMaybe n -- mountBindOptions :: Maybe MountBindOptions
    <*> arbitraryReducedMaybe n -- mountVolumeOptions :: Maybe MountVolumeOptions
    <*> arbitraryReducedMaybe n -- mountTmpfsOptions :: Maybe MountTmpfsOptions
  
instance Arbitrary MountBindOptions where
  arbitrary = sized genMountBindOptions

genMountBindOptions :: Int -> Gen MountBindOptions
genMountBindOptions n =
  MountBindOptions
    <$> arbitraryReducedMaybe n -- mountBindOptionsPropagation :: Maybe E'Propagation
  
instance Arbitrary MountPoint where
  arbitrary = sized genMountPoint

genMountPoint :: Int -> Gen MountPoint
genMountPoint n =
  MountPoint
    <$> arbitraryReducedMaybe n -- mountPointType :: Maybe E'Type2
    <*> arbitraryReducedMaybe n -- mountPointName :: Maybe Text
    <*> arbitraryReducedMaybe n -- mountPointSource :: Maybe Text
    <*> arbitraryReducedMaybe n -- mountPointDestination :: Maybe Text
    <*> arbitraryReducedMaybe n -- mountPointDriver :: Maybe Text
    <*> arbitraryReducedMaybe n -- mountPointMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- mountPointRw :: Maybe Bool
    <*> arbitraryReducedMaybe n -- mountPointPropagation :: Maybe Text
  
instance Arbitrary MountTmpfsOptions where
  arbitrary = sized genMountTmpfsOptions

genMountTmpfsOptions :: Int -> Gen MountTmpfsOptions
genMountTmpfsOptions n =
  MountTmpfsOptions
    <$> arbitraryReducedMaybe n -- mountTmpfsOptionsSizeBytes :: Maybe Integer
    <*> arbitraryReducedMaybe n -- mountTmpfsOptionsMode :: Maybe Int
  
instance Arbitrary MountVolumeOptions where
  arbitrary = sized genMountVolumeOptions

genMountVolumeOptions :: Int -> Gen MountVolumeOptions
genMountVolumeOptions n =
  MountVolumeOptions
    <$> arbitraryReducedMaybe n -- mountVolumeOptionsNoCopy :: Maybe Bool
    <*> arbitraryReducedMaybe n -- mountVolumeOptionsLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- mountVolumeOptionsDriverConfig :: Maybe MountVolumeOptionsDriverConfig
  
instance Arbitrary MountVolumeOptionsDriverConfig where
  arbitrary = sized genMountVolumeOptionsDriverConfig

genMountVolumeOptionsDriverConfig :: Int -> Gen MountVolumeOptionsDriverConfig
genMountVolumeOptionsDriverConfig n =
  MountVolumeOptionsDriverConfig
    <$> arbitraryReducedMaybe n -- mountVolumeOptionsDriverConfigName :: Maybe Text
    <*> arbitraryReducedMaybe n -- mountVolumeOptionsDriverConfigOptions :: Maybe (Map.Map String Text)
  
instance Arbitrary Network where
  arbitrary = sized genNetwork

genNetwork :: Int -> Gen Network
genNetwork n =
  Network
    <$> arbitraryReducedMaybe n -- networkName :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkId :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkScope :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkDriver :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkEnableIpv6 :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkIpam :: Maybe IPAM
    <*> arbitraryReducedMaybe n -- networkInternal :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkAttachable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkIngress :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkContainers :: Maybe (Map.Map String NetworkContainer)
    <*> arbitraryReducedMaybe n -- networkOptions :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- networkLabels :: Maybe (Map.Map String Text)
  
instance Arbitrary NetworkConnectRequest where
  arbitrary = sized genNetworkConnectRequest

genNetworkConnectRequest :: Int -> Gen NetworkConnectRequest
genNetworkConnectRequest n =
  NetworkConnectRequest
    <$> arbitraryReducedMaybe n -- networkConnectRequestContainer :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkConnectRequestEndpointConfig :: Maybe EndpointSettings
  
instance Arbitrary NetworkContainer where
  arbitrary = sized genNetworkContainer

genNetworkContainer :: Int -> Gen NetworkContainer
genNetworkContainer n =
  NetworkContainer
    <$> arbitraryReducedMaybe n -- networkContainerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkContainerEndpointId :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkContainerMacAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkContainerIpv4Address :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkContainerIpv6Address :: Maybe Text
  
instance Arbitrary NetworkCreateRequest where
  arbitrary = sized genNetworkCreateRequest

genNetworkCreateRequest :: Int -> Gen NetworkCreateRequest
genNetworkCreateRequest n =
  NetworkCreateRequest
    <$> arbitrary -- networkCreateRequestName :: Text
    <*> arbitraryReducedMaybe n -- networkCreateRequestCheckDuplicate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkCreateRequestDriver :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkCreateRequestInternal :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkCreateRequestAttachable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkCreateRequestIngress :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkCreateRequestIpam :: Maybe IPAM
    <*> arbitraryReducedMaybe n -- networkCreateRequestEnableIpv6 :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkCreateRequestOptions :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- networkCreateRequestLabels :: Maybe (Map.Map String Text)
  
instance Arbitrary NetworkCreateResponse where
  arbitrary = sized genNetworkCreateResponse

genNetworkCreateResponse :: Int -> Gen NetworkCreateResponse
genNetworkCreateResponse n =
  NetworkCreateResponse
    <$> arbitraryReducedMaybe n -- networkCreateResponseId :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkCreateResponseWarning :: Maybe Text
  
instance Arbitrary NetworkDisconnectRequest where
  arbitrary = sized genNetworkDisconnectRequest

genNetworkDisconnectRequest :: Int -> Gen NetworkDisconnectRequest
genNetworkDisconnectRequest n =
  NetworkDisconnectRequest
    <$> arbitraryReducedMaybe n -- networkDisconnectRequestContainer :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkDisconnectRequestForce :: Maybe Bool
  
instance Arbitrary NetworkPruneResponse where
  arbitrary = sized genNetworkPruneResponse

genNetworkPruneResponse :: Int -> Gen NetworkPruneResponse
genNetworkPruneResponse n =
  NetworkPruneResponse
    <$> arbitraryReducedMaybe n -- networkPruneResponseNetworksDeleted :: Maybe [Text]
  
instance Arbitrary NetworkSettings where
  arbitrary = sized genNetworkSettings

genNetworkSettings :: Int -> Gen NetworkSettings
genNetworkSettings n =
  NetworkSettings
    <$> arbitraryReducedMaybe n -- networkSettingsBridge :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkSettingsSandboxId :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkSettingsHairpinMode :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkSettingsLinkLocalIpv6Address :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkSettingsLinkLocalIpv6PrefixLen :: Maybe Int
    <*> arbitraryReducedMaybe n -- networkSettingsPorts :: Maybe (Map.Map String [PortBinding])
    <*> arbitraryReducedMaybe n -- networkSettingsSandboxKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkSettingsSecondaryIpAddresses :: Maybe [Address]
    <*> arbitraryReducedMaybe n -- networkSettingsSecondaryIpv6Addresses :: Maybe [Address]
    <*> arbitraryReducedMaybe n -- networkSettingsEndpointId :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkSettingsGateway :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkSettingsGlobalIpv6Address :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkSettingsGlobalIpv6PrefixLen :: Maybe Int
    <*> arbitraryReducedMaybe n -- networkSettingsIpAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkSettingsIpPrefixLen :: Maybe Int
    <*> arbitraryReducedMaybe n -- networkSettingsIpv6Gateway :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkSettingsMacAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkSettingsNetworks :: Maybe (Map.Map String EndpointSettings)
  
instance Arbitrary Node where
  arbitrary = sized genNode

genNode :: Int -> Gen Node
genNode n =
  Node
    <$> arbitraryReducedMaybe n -- nodeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nodeVersion :: Maybe ObjectVersion
    <*> arbitraryReducedMaybe n -- nodeCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nodeUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nodeSpec :: Maybe NodeSpec
    <*> arbitraryReducedMaybe n -- nodeDescription :: Maybe NodeDescription
    <*> arbitraryReducedMaybe n -- nodeStatus :: Maybe NodeStatus
    <*> arbitraryReducedMaybe n -- nodeManagerStatus :: Maybe ManagerStatus
  
instance Arbitrary NodeDescription where
  arbitrary = sized genNodeDescription

genNodeDescription :: Int -> Gen NodeDescription
genNodeDescription n =
  NodeDescription
    <$> arbitraryReducedMaybe n -- nodeDescriptionHostname :: Maybe Text
    <*> arbitraryReducedMaybe n -- nodeDescriptionPlatform :: Maybe Platform
    <*> arbitraryReducedMaybe n -- nodeDescriptionResources :: Maybe ResourceObject
    <*> arbitraryReducedMaybe n -- nodeDescriptionEngine :: Maybe EngineDescription
    <*> arbitraryReducedMaybe n -- nodeDescriptionTlsInfo :: Maybe TLSInfo
  
instance Arbitrary NodeSpec where
  arbitrary = sized genNodeSpec

genNodeSpec :: Int -> Gen NodeSpec
genNodeSpec n =
  NodeSpec
    <$> arbitraryReducedMaybe n -- nodeSpecName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nodeSpecLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- nodeSpecRole :: Maybe E'Role
    <*> arbitraryReducedMaybe n -- nodeSpecAvailability :: Maybe E'Availability
  
instance Arbitrary NodeStatus where
  arbitrary = sized genNodeStatus

genNodeStatus :: Int -> Gen NodeStatus
genNodeStatus n =
  NodeStatus
    <$> arbitraryReducedMaybe n -- nodeStatusState :: Maybe NodeState
    <*> arbitraryReducedMaybe n -- nodeStatusMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- nodeStatusAddr :: Maybe Text
  
instance Arbitrary ObjectVersion where
  arbitrary = sized genObjectVersion

genObjectVersion :: Int -> Gen ObjectVersion
genObjectVersion n =
  ObjectVersion
    <$> arbitraryReducedMaybe n -- objectVersionIndex :: Maybe Int
  
instance Arbitrary PeerNode where
  arbitrary = sized genPeerNode

genPeerNode :: Int -> Gen PeerNode
genPeerNode n =
  PeerNode
    <$> arbitraryReducedMaybe n -- peerNodeNodeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- peerNodeAddr :: Maybe Text
  
instance Arbitrary Platform where
  arbitrary = sized genPlatform

genPlatform :: Int -> Gen Platform
genPlatform n =
  Platform
    <$> arbitraryReducedMaybe n -- platformArchitecture :: Maybe Text
    <*> arbitraryReducedMaybe n -- platformOs :: Maybe Text
  
instance Arbitrary Plugin where
  arbitrary = sized genPlugin

genPlugin :: Int -> Gen Plugin
genPlugin n =
  Plugin
    <$> arbitraryReducedMaybe n -- pluginId :: Maybe Text
    <*> arbitrary -- pluginName :: Text
    <*> arbitrary -- pluginEnabled :: Bool
    <*> arbitraryReduced n -- pluginSettings :: PluginSettings
    <*> arbitraryReducedMaybe n -- pluginPluginReference :: Maybe Text
    <*> arbitraryReduced n -- pluginConfig :: PluginConfig
  
instance Arbitrary PluginConfig where
  arbitrary = sized genPluginConfig

genPluginConfig :: Int -> Gen PluginConfig
genPluginConfig n =
  PluginConfig
    <$> arbitraryReducedMaybe n -- pluginConfigDockerVersion :: Maybe Text
    <*> arbitrary -- pluginConfigDescription :: Text
    <*> arbitrary -- pluginConfigDocumentation :: Text
    <*> arbitraryReduced n -- pluginConfigInterface :: PluginConfigInterface
    <*> arbitrary -- pluginConfigEntrypoint :: [Text]
    <*> arbitrary -- pluginConfigWorkDir :: Text
    <*> arbitraryReducedMaybe n -- pluginConfigUser :: Maybe PluginConfigUser
    <*> arbitraryReduced n -- pluginConfigNetwork :: PluginConfigNetwork
    <*> arbitraryReduced n -- pluginConfigLinux :: PluginConfigLinux
    <*> arbitrary -- pluginConfigPropagatedMount :: Text
    <*> arbitrary -- pluginConfigIpcHost :: Bool
    <*> arbitrary -- pluginConfigPidHost :: Bool
    <*> arbitraryReduced n -- pluginConfigMounts :: [PluginMount]
    <*> arbitraryReduced n -- pluginConfigEnv :: [PluginEnv]
    <*> arbitraryReduced n -- pluginConfigArgs :: PluginConfigArgs
    <*> arbitraryReducedMaybe n -- pluginConfigRootfs :: Maybe PluginConfigRootfs
  
instance Arbitrary PluginConfigArgs where
  arbitrary = sized genPluginConfigArgs

genPluginConfigArgs :: Int -> Gen PluginConfigArgs
genPluginConfigArgs n =
  PluginConfigArgs
    <$> arbitrary -- pluginConfigArgsName :: Text
    <*> arbitrary -- pluginConfigArgsDescription :: Text
    <*> arbitrary -- pluginConfigArgsSettable :: [Text]
    <*> arbitrary -- pluginConfigArgsValue :: [Text]
  
instance Arbitrary PluginConfigInterface where
  arbitrary = sized genPluginConfigInterface

genPluginConfigInterface :: Int -> Gen PluginConfigInterface
genPluginConfigInterface n =
  PluginConfigInterface
    <$> arbitraryReduced n -- pluginConfigInterfaceTypes :: [PluginInterfaceType]
    <*> arbitrary -- pluginConfigInterfaceSocket :: Text
  
instance Arbitrary PluginConfigLinux where
  arbitrary = sized genPluginConfigLinux

genPluginConfigLinux :: Int -> Gen PluginConfigLinux
genPluginConfigLinux n =
  PluginConfigLinux
    <$> arbitrary -- pluginConfigLinuxCapabilities :: [Text]
    <*> arbitrary -- pluginConfigLinuxAllowAllDevices :: Bool
    <*> arbitraryReduced n -- pluginConfigLinuxDevices :: [PluginDevice]
  
instance Arbitrary PluginConfigNetwork where
  arbitrary = sized genPluginConfigNetwork

genPluginConfigNetwork :: Int -> Gen PluginConfigNetwork
genPluginConfigNetwork n =
  PluginConfigNetwork
    <$> arbitrary -- pluginConfigNetworkType :: Text
  
instance Arbitrary PluginConfigRootfs where
  arbitrary = sized genPluginConfigRootfs

genPluginConfigRootfs :: Int -> Gen PluginConfigRootfs
genPluginConfigRootfs n =
  PluginConfigRootfs
    <$> arbitraryReducedMaybe n -- pluginConfigRootfsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- pluginConfigRootfsDiffIds :: Maybe [Text]
  
instance Arbitrary PluginConfigUser where
  arbitrary = sized genPluginConfigUser

genPluginConfigUser :: Int -> Gen PluginConfigUser
genPluginConfigUser n =
  PluginConfigUser
    <$> arbitraryReducedMaybe n -- pluginConfigUserUid :: Maybe Int
    <*> arbitraryReducedMaybe n -- pluginConfigUserGid :: Maybe Int
  
instance Arbitrary PluginDevice where
  arbitrary = sized genPluginDevice

genPluginDevice :: Int -> Gen PluginDevice
genPluginDevice n =
  PluginDevice
    <$> arbitrary -- pluginDeviceName :: Text
    <*> arbitrary -- pluginDeviceDescription :: Text
    <*> arbitrary -- pluginDeviceSettable :: [Text]
    <*> arbitrary -- pluginDevicePath :: Text
  
instance Arbitrary PluginEnv where
  arbitrary = sized genPluginEnv

genPluginEnv :: Int -> Gen PluginEnv
genPluginEnv n =
  PluginEnv
    <$> arbitrary -- pluginEnvName :: Text
    <*> arbitrary -- pluginEnvDescription :: Text
    <*> arbitrary -- pluginEnvSettable :: [Text]
    <*> arbitrary -- pluginEnvValue :: Text
  
instance Arbitrary PluginInterfaceType where
  arbitrary = sized genPluginInterfaceType

genPluginInterfaceType :: Int -> Gen PluginInterfaceType
genPluginInterfaceType n =
  PluginInterfaceType
    <$> arbitrary -- pluginInterfaceTypePrefix :: Text
    <*> arbitrary -- pluginInterfaceTypeCapability :: Text
    <*> arbitrary -- pluginInterfaceTypeVersion :: Text
  
instance Arbitrary PluginMount where
  arbitrary = sized genPluginMount

genPluginMount :: Int -> Gen PluginMount
genPluginMount n =
  PluginMount
    <$> arbitrary -- pluginMountName :: Text
    <*> arbitrary -- pluginMountDescription :: Text
    <*> arbitrary -- pluginMountSettable :: [Text]
    <*> arbitrary -- pluginMountSource :: Text
    <*> arbitrary -- pluginMountDestination :: Text
    <*> arbitrary -- pluginMountType :: Text
    <*> arbitrary -- pluginMountOptions :: [Text]
  
instance Arbitrary PluginPrivilegeItem where
  arbitrary = sized genPluginPrivilegeItem

genPluginPrivilegeItem :: Int -> Gen PluginPrivilegeItem
genPluginPrivilegeItem n =
  PluginPrivilegeItem
    <$> arbitraryReducedMaybe n -- pluginPrivilegeItemName :: Maybe Text
    <*> arbitraryReducedMaybe n -- pluginPrivilegeItemDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- pluginPrivilegeItemValue :: Maybe [Text]
  
instance Arbitrary PluginPullRequestInner where
  arbitrary = sized genPluginPullRequestInner

genPluginPullRequestInner :: Int -> Gen PluginPullRequestInner
genPluginPullRequestInner n =
  PluginPullRequestInner
    <$> arbitraryReducedMaybe n -- pluginPullRequestInnerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- pluginPullRequestInnerDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- pluginPullRequestInnerValue :: Maybe [Text]
  
instance Arbitrary PluginSettings where
  arbitrary = sized genPluginSettings

genPluginSettings :: Int -> Gen PluginSettings
genPluginSettings n =
  PluginSettings
    <$> arbitraryReduced n -- pluginSettingsMounts :: [PluginMount]
    <*> arbitrary -- pluginSettingsEnv :: [Text]
    <*> arbitrary -- pluginSettingsArgs :: [Text]
    <*> arbitraryReduced n -- pluginSettingsDevices :: [PluginDevice]
  
instance Arbitrary PluginsInfo where
  arbitrary = sized genPluginsInfo

genPluginsInfo :: Int -> Gen PluginsInfo
genPluginsInfo n =
  PluginsInfo
    <$> arbitraryReducedMaybe n -- pluginsInfoVolume :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- pluginsInfoNetwork :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- pluginsInfoAuthorization :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- pluginsInfoLog :: Maybe [Text]
  
instance Arbitrary Port where
  arbitrary = sized genPort

genPort :: Int -> Gen Port
genPort n =
  Port
    <$> arbitraryReducedMaybe n -- portIp :: Maybe Text
    <*> arbitrary -- portPrivatePort :: Int
    <*> arbitraryReducedMaybe n -- portPublicPort :: Maybe Int
    <*> arbitrary -- portType :: E'Type
  
instance Arbitrary PortBinding where
  arbitrary = sized genPortBinding

genPortBinding :: Int -> Gen PortBinding
genPortBinding n =
  PortBinding
    <$> arbitraryReducedMaybe n -- portBindingHostIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- portBindingHostPort :: Maybe Text
  
instance Arbitrary ProcessConfig where
  arbitrary = sized genProcessConfig

genProcessConfig :: Int -> Gen ProcessConfig
genProcessConfig n =
  ProcessConfig
    <$> arbitraryReducedMaybe n -- processConfigPrivileged :: Maybe Bool
    <*> arbitraryReducedMaybe n -- processConfigUser :: Maybe Text
    <*> arbitraryReducedMaybe n -- processConfigTty :: Maybe Bool
    <*> arbitraryReducedMaybe n -- processConfigEntrypoint :: Maybe Text
    <*> arbitraryReducedMaybe n -- processConfigArguments :: Maybe [Text]
  
instance Arbitrary ProgressDetail where
  arbitrary = sized genProgressDetail

genProgressDetail :: Int -> Gen ProgressDetail
genProgressDetail n =
  ProgressDetail
    <$> arbitraryReducedMaybe n -- progressDetailCurrent :: Maybe Int
    <*> arbitraryReducedMaybe n -- progressDetailTotal :: Maybe Int
  
instance Arbitrary PushImageInfo where
  arbitrary = sized genPushImageInfo

genPushImageInfo :: Int -> Gen PushImageInfo
genPushImageInfo n =
  PushImageInfo
    <$> arbitraryReducedMaybe n -- pushImageInfoError :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushImageInfoStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushImageInfoProgress :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushImageInfoProgressDetail :: Maybe ProgressDetail
  
instance Arbitrary RegistryServiceConfig where
  arbitrary = sized genRegistryServiceConfig

genRegistryServiceConfig :: Int -> Gen RegistryServiceConfig
genRegistryServiceConfig n =
  RegistryServiceConfig
    <$> arbitraryReducedMaybe n -- registryServiceConfigAllowNondistributableArtifactsCidrs :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- registryServiceConfigAllowNondistributableArtifactsHostnames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- registryServiceConfigInsecureRegistryCidrs :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- registryServiceConfigIndexConfigs :: Maybe (Map.Map String IndexInfo)
    <*> arbitraryReducedMaybe n -- registryServiceConfigMirrors :: Maybe [Text]
  
instance Arbitrary ResourceObject where
  arbitrary = sized genResourceObject

genResourceObject :: Int -> Gen ResourceObject
genResourceObject n =
  ResourceObject
    <$> arbitraryReducedMaybe n -- resourceObjectNanoCpus :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourceObjectMemoryBytes :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourceObjectGenericResources :: Maybe [GenericResourcesInner]
  
instance Arbitrary Resources where
  arbitrary = sized genResources

genResources :: Int -> Gen Resources
genResources n =
  Resources
    <$> arbitraryReducedMaybe n -- resourcesCpuShares :: Maybe Int
    <*> arbitraryReducedMaybe n -- resourcesMemory :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesCgroupParent :: Maybe Text
    <*> arbitraryReducedMaybe n -- resourcesBlkioWeight :: Maybe Int
    <*> arbitraryReducedMaybe n -- resourcesBlkioWeightDevice :: Maybe [ResourcesBlkioWeightDeviceInner]
    <*> arbitraryReducedMaybe n -- resourcesBlkioDeviceReadBps :: Maybe [ThrottleDevice]
    <*> arbitraryReducedMaybe n -- resourcesBlkioDeviceWriteBps :: Maybe [ThrottleDevice]
    <*> arbitraryReducedMaybe n -- resourcesBlkioDeviceReadIOps :: Maybe [ThrottleDevice]
    <*> arbitraryReducedMaybe n -- resourcesBlkioDeviceWriteIOps :: Maybe [ThrottleDevice]
    <*> arbitraryReducedMaybe n -- resourcesCpuPeriod :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesCpuQuota :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesCpuRealtimePeriod :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesCpuRealtimeRuntime :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesCpusetCpus :: Maybe Text
    <*> arbitraryReducedMaybe n -- resourcesCpusetMems :: Maybe Text
    <*> arbitraryReducedMaybe n -- resourcesDevices :: Maybe [DeviceMapping]
    <*> arbitraryReducedMaybe n -- resourcesDeviceCgroupRules :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- resourcesDiskQuota :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesKernelMemory :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesMemoryReservation :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesMemorySwap :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesMemorySwappiness :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesNanoCpus :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesOomKillDisable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- resourcesPidsLimit :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesUlimits :: Maybe [ResourcesUlimitsInner]
    <*> arbitraryReducedMaybe n -- resourcesCpuCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesCpuPercent :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesIoMaximumIOps :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesIoMaximumBandwidth :: Maybe Integer
  
instance Arbitrary ResourcesBlkioWeightDeviceInner where
  arbitrary = sized genResourcesBlkioWeightDeviceInner

genResourcesBlkioWeightDeviceInner :: Int -> Gen ResourcesBlkioWeightDeviceInner
genResourcesBlkioWeightDeviceInner n =
  ResourcesBlkioWeightDeviceInner
    <$> arbitraryReducedMaybe n -- resourcesBlkioWeightDeviceInnerPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- resourcesBlkioWeightDeviceInnerWeight :: Maybe Int
  
instance Arbitrary ResourcesUlimitsInner where
  arbitrary = sized genResourcesUlimitsInner

genResourcesUlimitsInner :: Int -> Gen ResourcesUlimitsInner
genResourcesUlimitsInner n =
  ResourcesUlimitsInner
    <$> arbitraryReducedMaybe n -- resourcesUlimitsInnerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- resourcesUlimitsInnerSoft :: Maybe Int
    <*> arbitraryReducedMaybe n -- resourcesUlimitsInnerHard :: Maybe Int
  
instance Arbitrary RestartPolicy where
  arbitrary = sized genRestartPolicy

genRestartPolicy :: Int -> Gen RestartPolicy
genRestartPolicy n =
  RestartPolicy
    <$> arbitraryReducedMaybe n -- restartPolicyName :: Maybe E'Name
    <*> arbitraryReducedMaybe n -- restartPolicyMaximumRetryCount :: Maybe Int
  
instance Arbitrary Runtime where
  arbitrary = sized genRuntime

genRuntime :: Int -> Gen Runtime
genRuntime n =
  Runtime
    <$> arbitraryReducedMaybe n -- runtimePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- runtimeRuntimeArgs :: Maybe [Text]
  
instance Arbitrary Secret where
  arbitrary = sized genSecret

genSecret :: Int -> Gen Secret
genSecret n =
  Secret
    <$> arbitraryReducedMaybe n -- secretId :: Maybe Text
    <*> arbitraryReducedMaybe n -- secretVersion :: Maybe ObjectVersion
    <*> arbitraryReducedMaybe n -- secretCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- secretUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- secretSpec :: Maybe SecretSpec
  
instance Arbitrary SecretCreateRequest where
  arbitrary = sized genSecretCreateRequest

genSecretCreateRequest :: Int -> Gen SecretCreateRequest
genSecretCreateRequest n =
  SecretCreateRequest
    <$> arbitraryReducedMaybe n -- secretCreateRequestName :: Maybe Text
    <*> arbitraryReducedMaybe n -- secretCreateRequestLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- secretCreateRequestData :: Maybe Text
    <*> arbitraryReducedMaybe n -- secretCreateRequestDriver :: Maybe Driver
  
instance Arbitrary SecretSpec where
  arbitrary = sized genSecretSpec

genSecretSpec :: Int -> Gen SecretSpec
genSecretSpec n =
  SecretSpec
    <$> arbitraryReducedMaybe n -- secretSpecName :: Maybe Text
    <*> arbitraryReducedMaybe n -- secretSpecLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- secretSpecData :: Maybe Text
    <*> arbitraryReducedMaybe n -- secretSpecDriver :: Maybe Driver
  
instance Arbitrary Service where
  arbitrary = sized genService

genService :: Int -> Gen Service
genService n =
  Service
    <$> arbitraryReducedMaybe n -- serviceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceVersion :: Maybe ObjectVersion
    <*> arbitraryReducedMaybe n -- serviceCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceSpec :: Maybe ServiceSpec
    <*> arbitraryReducedMaybe n -- serviceEndpoint :: Maybe ServiceEndpoint
    <*> arbitraryReducedMaybe n -- serviceUpdateStatus :: Maybe ServiceUpdateStatus
  
instance Arbitrary ServiceCreateRequest where
  arbitrary = sized genServiceCreateRequest

genServiceCreateRequest :: Int -> Gen ServiceCreateRequest
genServiceCreateRequest n =
  ServiceCreateRequest
    <$> arbitraryReducedMaybe n -- serviceCreateRequestName :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceCreateRequestLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- serviceCreateRequestTaskTemplate :: Maybe TaskSpec
    <*> arbitraryReducedMaybe n -- serviceCreateRequestMode :: Maybe ServiceSpecMode
    <*> arbitraryReducedMaybe n -- serviceCreateRequestUpdateConfig :: Maybe ServiceSpecUpdateConfig
    <*> arbitraryReducedMaybe n -- serviceCreateRequestRollbackConfig :: Maybe ServiceSpecRollbackConfig
    <*> arbitraryReducedMaybe n -- serviceCreateRequestNetworks :: Maybe [TaskSpecNetworksInner]
    <*> arbitraryReducedMaybe n -- serviceCreateRequestEndpointSpec :: Maybe EndpointSpec
  
instance Arbitrary ServiceCreateResponse where
  arbitrary = sized genServiceCreateResponse

genServiceCreateResponse :: Int -> Gen ServiceCreateResponse
genServiceCreateResponse n =
  ServiceCreateResponse
    <$> arbitraryReducedMaybe n -- serviceCreateResponseId :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceCreateResponseWarning :: Maybe Text
  
instance Arbitrary ServiceEndpoint where
  arbitrary = sized genServiceEndpoint

genServiceEndpoint :: Int -> Gen ServiceEndpoint
genServiceEndpoint n =
  ServiceEndpoint
    <$> arbitraryReducedMaybe n -- serviceEndpointSpec :: Maybe EndpointSpec
    <*> arbitraryReducedMaybe n -- serviceEndpointPorts :: Maybe [EndpointPortConfig]
    <*> arbitraryReducedMaybe n -- serviceEndpointVirtualIps :: Maybe [ServiceEndpointVirtualIPsInner]
  
instance Arbitrary ServiceEndpointVirtualIPsInner where
  arbitrary = sized genServiceEndpointVirtualIPsInner

genServiceEndpointVirtualIPsInner :: Int -> Gen ServiceEndpointVirtualIPsInner
genServiceEndpointVirtualIPsInner n =
  ServiceEndpointVirtualIPsInner
    <$> arbitraryReducedMaybe n -- serviceEndpointVirtualIPsInnerNetworkId :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceEndpointVirtualIPsInnerAddr :: Maybe Text
  
instance Arbitrary ServiceSpec where
  arbitrary = sized genServiceSpec

genServiceSpec :: Int -> Gen ServiceSpec
genServiceSpec n =
  ServiceSpec
    <$> arbitraryReducedMaybe n -- serviceSpecName :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceSpecLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- serviceSpecTaskTemplate :: Maybe TaskSpec
    <*> arbitraryReducedMaybe n -- serviceSpecMode :: Maybe ServiceSpecMode
    <*> arbitraryReducedMaybe n -- serviceSpecUpdateConfig :: Maybe ServiceSpecUpdateConfig
    <*> arbitraryReducedMaybe n -- serviceSpecRollbackConfig :: Maybe ServiceSpecRollbackConfig
    <*> arbitraryReducedMaybe n -- serviceSpecNetworks :: Maybe [TaskSpecNetworksInner]
    <*> arbitraryReducedMaybe n -- serviceSpecEndpointSpec :: Maybe EndpointSpec
  
instance Arbitrary ServiceSpecMode where
  arbitrary = sized genServiceSpecMode

genServiceSpecMode :: Int -> Gen ServiceSpecMode
genServiceSpecMode n =
  ServiceSpecMode
    <$> arbitraryReducedMaybe n -- serviceSpecModeReplicated :: Maybe ServiceSpecModeReplicated
    <*> arbitraryReducedMaybeValue n -- serviceSpecModeGlobal :: Maybe A.Value
  
instance Arbitrary ServiceSpecModeReplicated where
  arbitrary = sized genServiceSpecModeReplicated

genServiceSpecModeReplicated :: Int -> Gen ServiceSpecModeReplicated
genServiceSpecModeReplicated n =
  ServiceSpecModeReplicated
    <$> arbitraryReducedMaybe n -- serviceSpecModeReplicatedReplicas :: Maybe Integer
  
instance Arbitrary ServiceSpecRollbackConfig where
  arbitrary = sized genServiceSpecRollbackConfig

genServiceSpecRollbackConfig :: Int -> Gen ServiceSpecRollbackConfig
genServiceSpecRollbackConfig n =
  ServiceSpecRollbackConfig
    <$> arbitraryReducedMaybe n -- serviceSpecRollbackConfigParallelism :: Maybe Integer
    <*> arbitraryReducedMaybe n -- serviceSpecRollbackConfigDelay :: Maybe Integer
    <*> arbitraryReducedMaybe n -- serviceSpecRollbackConfigFailureAction :: Maybe E'FailureAction2
    <*> arbitraryReducedMaybe n -- serviceSpecRollbackConfigMonitor :: Maybe Integer
    <*> arbitraryReducedMaybe n -- serviceSpecRollbackConfigMaxFailureRatio :: Maybe Double
    <*> arbitraryReducedMaybe n -- serviceSpecRollbackConfigOrder :: Maybe E'Order
  
instance Arbitrary ServiceSpecUpdateConfig where
  arbitrary = sized genServiceSpecUpdateConfig

genServiceSpecUpdateConfig :: Int -> Gen ServiceSpecUpdateConfig
genServiceSpecUpdateConfig n =
  ServiceSpecUpdateConfig
    <$> arbitraryReducedMaybe n -- serviceSpecUpdateConfigParallelism :: Maybe Integer
    <*> arbitraryReducedMaybe n -- serviceSpecUpdateConfigDelay :: Maybe Integer
    <*> arbitraryReducedMaybe n -- serviceSpecUpdateConfigFailureAction :: Maybe E'FailureAction
    <*> arbitraryReducedMaybe n -- serviceSpecUpdateConfigMonitor :: Maybe Integer
    <*> arbitraryReducedMaybe n -- serviceSpecUpdateConfigMaxFailureRatio :: Maybe Double
    <*> arbitraryReducedMaybe n -- serviceSpecUpdateConfigOrder :: Maybe E'Order
  
instance Arbitrary ServiceUpdateRequest where
  arbitrary = sized genServiceUpdateRequest

genServiceUpdateRequest :: Int -> Gen ServiceUpdateRequest
genServiceUpdateRequest n =
  ServiceUpdateRequest
    <$> arbitraryReducedMaybe n -- serviceUpdateRequestName :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceUpdateRequestLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- serviceUpdateRequestTaskTemplate :: Maybe TaskSpec
    <*> arbitraryReducedMaybe n -- serviceUpdateRequestMode :: Maybe ServiceSpecMode
    <*> arbitraryReducedMaybe n -- serviceUpdateRequestUpdateConfig :: Maybe ServiceSpecUpdateConfig
    <*> arbitraryReducedMaybe n -- serviceUpdateRequestRollbackConfig :: Maybe ServiceSpecRollbackConfig
    <*> arbitraryReducedMaybe n -- serviceUpdateRequestNetworks :: Maybe [TaskSpecNetworksInner]
    <*> arbitraryReducedMaybe n -- serviceUpdateRequestEndpointSpec :: Maybe EndpointSpec
  
instance Arbitrary ServiceUpdateResponse where
  arbitrary = sized genServiceUpdateResponse

genServiceUpdateResponse :: Int -> Gen ServiceUpdateResponse
genServiceUpdateResponse n =
  ServiceUpdateResponse
    <$> arbitraryReducedMaybe n -- serviceUpdateResponseWarnings :: Maybe [Text]
  
instance Arbitrary ServiceUpdateStatus where
  arbitrary = sized genServiceUpdateStatus

genServiceUpdateStatus :: Int -> Gen ServiceUpdateStatus
genServiceUpdateStatus n =
  ServiceUpdateStatus
    <$> arbitraryReducedMaybe n -- serviceUpdateStatusState :: Maybe E'State
    <*> arbitraryReducedMaybe n -- serviceUpdateStatusStartedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceUpdateStatusCompletedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceUpdateStatusMessage :: Maybe Text
  
instance Arbitrary Swarm where
  arbitrary = sized genSwarm

genSwarm :: Int -> Gen Swarm
genSwarm n =
  Swarm
    <$> arbitraryReducedMaybe n -- swarmId :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmVersion :: Maybe ObjectVersion
    <*> arbitraryReducedMaybe n -- swarmCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmSpec :: Maybe SwarmSpec
    <*> arbitraryReducedMaybe n -- swarmTlsInfo :: Maybe TLSInfo
    <*> arbitraryReducedMaybe n -- swarmRootRotationInProgress :: Maybe Bool
    <*> arbitraryReducedMaybe n -- swarmJoinTokens :: Maybe JoinTokens
  
instance Arbitrary SwarmInfo where
  arbitrary = sized genSwarmInfo

genSwarmInfo :: Int -> Gen SwarmInfo
genSwarmInfo n =
  SwarmInfo
    <$> arbitraryReducedMaybe n -- swarmInfoNodeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmInfoNodeAddr :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmInfoLocalNodeState :: Maybe LocalNodeState
    <*> arbitraryReducedMaybe n -- swarmInfoControlAvailable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- swarmInfoError :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmInfoRemoteManagers :: Maybe [PeerNode]
    <*> arbitraryReducedMaybe n -- swarmInfoNodes :: Maybe Int
    <*> arbitraryReducedMaybe n -- swarmInfoManagers :: Maybe Int
    <*> arbitraryReducedMaybe n -- swarmInfoCluster :: Maybe ClusterInfo
  
instance Arbitrary SwarmInitRequest where
  arbitrary = sized genSwarmInitRequest

genSwarmInitRequest :: Int -> Gen SwarmInitRequest
genSwarmInitRequest n =
  SwarmInitRequest
    <$> arbitraryReducedMaybe n -- swarmInitRequestListenAddr :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmInitRequestAdvertiseAddr :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmInitRequestDataPathAddr :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmInitRequestForceNewCluster :: Maybe Bool
    <*> arbitraryReducedMaybe n -- swarmInitRequestSpec :: Maybe SwarmSpec
  
instance Arbitrary SwarmJoinRequest where
  arbitrary = sized genSwarmJoinRequest

genSwarmJoinRequest :: Int -> Gen SwarmJoinRequest
genSwarmJoinRequest n =
  SwarmJoinRequest
    <$> arbitraryReducedMaybe n -- swarmJoinRequestListenAddr :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmJoinRequestAdvertiseAddr :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmJoinRequestDataPathAddr :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmJoinRequestRemoteAddrs :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmJoinRequestJoinToken :: Maybe Text
  
instance Arbitrary SwarmSpec where
  arbitrary = sized genSwarmSpec

genSwarmSpec :: Int -> Gen SwarmSpec
genSwarmSpec n =
  SwarmSpec
    <$> arbitraryReducedMaybe n -- swarmSpecName :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmSpecLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- swarmSpecOrchestration :: Maybe SwarmSpecOrchestration
    <*> arbitraryReducedMaybe n -- swarmSpecRaft :: Maybe SwarmSpecRaft
    <*> arbitraryReducedMaybe n -- swarmSpecDispatcher :: Maybe SwarmSpecDispatcher
    <*> arbitraryReducedMaybe n -- swarmSpecCaConfig :: Maybe SwarmSpecCAConfig
    <*> arbitraryReducedMaybe n -- swarmSpecEncryptionConfig :: Maybe SwarmSpecEncryptionConfig
    <*> arbitraryReducedMaybe n -- swarmSpecTaskDefaults :: Maybe SwarmSpecTaskDefaults
  
instance Arbitrary SwarmSpecCAConfig where
  arbitrary = sized genSwarmSpecCAConfig

genSwarmSpecCAConfig :: Int -> Gen SwarmSpecCAConfig
genSwarmSpecCAConfig n =
  SwarmSpecCAConfig
    <$> arbitraryReducedMaybe n -- swarmSpecCAConfigNodeCertExpiry :: Maybe Integer
    <*> arbitraryReducedMaybe n -- swarmSpecCAConfigExternalCas :: Maybe [SwarmSpecCAConfigExternalCAsInner]
    <*> arbitraryReducedMaybe n -- swarmSpecCAConfigSigningCaCert :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmSpecCAConfigSigningCaKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmSpecCAConfigForceRotate :: Maybe Int
  
instance Arbitrary SwarmSpecCAConfigExternalCAsInner where
  arbitrary = sized genSwarmSpecCAConfigExternalCAsInner

genSwarmSpecCAConfigExternalCAsInner :: Int -> Gen SwarmSpecCAConfigExternalCAsInner
genSwarmSpecCAConfigExternalCAsInner n =
  SwarmSpecCAConfigExternalCAsInner
    <$> arbitraryReducedMaybe n -- swarmSpecCAConfigExternalCAsInnerProtocol :: Maybe E'Protocol
    <*> arbitraryReducedMaybe n -- swarmSpecCAConfigExternalCAsInnerUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmSpecCAConfigExternalCAsInnerOptions :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- swarmSpecCAConfigExternalCAsInnerCaCert :: Maybe Text
  
instance Arbitrary SwarmSpecDispatcher where
  arbitrary = sized genSwarmSpecDispatcher

genSwarmSpecDispatcher :: Int -> Gen SwarmSpecDispatcher
genSwarmSpecDispatcher n =
  SwarmSpecDispatcher
    <$> arbitraryReducedMaybe n -- swarmSpecDispatcherHeartbeatPeriod :: Maybe Integer
  
instance Arbitrary SwarmSpecEncryptionConfig where
  arbitrary = sized genSwarmSpecEncryptionConfig

genSwarmSpecEncryptionConfig :: Int -> Gen SwarmSpecEncryptionConfig
genSwarmSpecEncryptionConfig n =
  SwarmSpecEncryptionConfig
    <$> arbitraryReducedMaybe n -- swarmSpecEncryptionConfigAutoLockManagers :: Maybe Bool
  
instance Arbitrary SwarmSpecOrchestration where
  arbitrary = sized genSwarmSpecOrchestration

genSwarmSpecOrchestration :: Int -> Gen SwarmSpecOrchestration
genSwarmSpecOrchestration n =
  SwarmSpecOrchestration
    <$> arbitraryReducedMaybe n -- swarmSpecOrchestrationTaskHistoryRetentionLimit :: Maybe Integer
  
instance Arbitrary SwarmSpecRaft where
  arbitrary = sized genSwarmSpecRaft

genSwarmSpecRaft :: Int -> Gen SwarmSpecRaft
genSwarmSpecRaft n =
  SwarmSpecRaft
    <$> arbitraryReducedMaybe n -- swarmSpecRaftSnapshotInterval :: Maybe Int
    <*> arbitraryReducedMaybe n -- swarmSpecRaftKeepOldSnapshots :: Maybe Int
    <*> arbitraryReducedMaybe n -- swarmSpecRaftLogEntriesForSlowFollowers :: Maybe Int
    <*> arbitraryReducedMaybe n -- swarmSpecRaftElectionTick :: Maybe Int
    <*> arbitraryReducedMaybe n -- swarmSpecRaftHeartbeatTick :: Maybe Int
  
instance Arbitrary SwarmSpecTaskDefaults where
  arbitrary = sized genSwarmSpecTaskDefaults

genSwarmSpecTaskDefaults :: Int -> Gen SwarmSpecTaskDefaults
genSwarmSpecTaskDefaults n =
  SwarmSpecTaskDefaults
    <$> arbitraryReducedMaybe n -- swarmSpecTaskDefaultsLogDriver :: Maybe SwarmSpecTaskDefaultsLogDriver
  
instance Arbitrary SwarmSpecTaskDefaultsLogDriver where
  arbitrary = sized genSwarmSpecTaskDefaultsLogDriver

genSwarmSpecTaskDefaultsLogDriver :: Int -> Gen SwarmSpecTaskDefaultsLogDriver
genSwarmSpecTaskDefaultsLogDriver n =
  SwarmSpecTaskDefaultsLogDriver
    <$> arbitraryReducedMaybe n -- swarmSpecTaskDefaultsLogDriverName :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmSpecTaskDefaultsLogDriverOptions :: Maybe (Map.Map String Text)
  
instance Arbitrary SwarmUnlockRequest where
  arbitrary = sized genSwarmUnlockRequest

genSwarmUnlockRequest :: Int -> Gen SwarmUnlockRequest
genSwarmUnlockRequest n =
  SwarmUnlockRequest
    <$> arbitraryReducedMaybe n -- swarmUnlockRequestUnlockKey :: Maybe Text
  
instance Arbitrary SystemAuthResponse where
  arbitrary = sized genSystemAuthResponse

genSystemAuthResponse :: Int -> Gen SystemAuthResponse
genSystemAuthResponse n =
  SystemAuthResponse
    <$> arbitrary -- systemAuthResponseStatus :: Text
    <*> arbitraryReducedMaybe n -- systemAuthResponseIdentityToken :: Maybe Text
  
instance Arbitrary SystemDataUsageResponse where
  arbitrary = sized genSystemDataUsageResponse

genSystemDataUsageResponse :: Int -> Gen SystemDataUsageResponse
genSystemDataUsageResponse n =
  SystemDataUsageResponse
    <$> arbitraryReducedMaybe n -- systemDataUsageResponseLayersSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- systemDataUsageResponseImages :: Maybe [ImageSummary]
    <*> arbitraryReducedMaybe n -- systemDataUsageResponseContainers :: Maybe [ContainerSummary]
    <*> arbitraryReducedMaybe n -- systemDataUsageResponseVolumes :: Maybe [Volume]
  
instance Arbitrary SystemEventsResponse where
  arbitrary = sized genSystemEventsResponse

genSystemEventsResponse :: Int -> Gen SystemEventsResponse
genSystemEventsResponse n =
  SystemEventsResponse
    <$> arbitraryReducedMaybe n -- systemEventsResponseType :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemEventsResponseAction :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemEventsResponseActor :: Maybe SystemEventsResponseActor
    <*> arbitraryReducedMaybe n -- systemEventsResponseTime :: Maybe Int
    <*> arbitraryReducedMaybe n -- systemEventsResponseTimeNano :: Maybe Integer
  
instance Arbitrary SystemEventsResponseActor where
  arbitrary = sized genSystemEventsResponseActor

genSystemEventsResponseActor :: Int -> Gen SystemEventsResponseActor
genSystemEventsResponseActor n =
  SystemEventsResponseActor
    <$> arbitraryReducedMaybe n -- systemEventsResponseActorId :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemEventsResponseActorAttributes :: Maybe (Map.Map String Text)
  
instance Arbitrary SystemInfo where
  arbitrary = sized genSystemInfo

genSystemInfo :: Int -> Gen SystemInfo
genSystemInfo n =
  SystemInfo
    <$> arbitraryReducedMaybe n -- systemInfoId :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoContainers :: Maybe Int
    <*> arbitraryReducedMaybe n -- systemInfoContainersRunning :: Maybe Int
    <*> arbitraryReducedMaybe n -- systemInfoContainersPaused :: Maybe Int
    <*> arbitraryReducedMaybe n -- systemInfoContainersStopped :: Maybe Int
    <*> arbitraryReducedMaybe n -- systemInfoImages :: Maybe Int
    <*> arbitraryReducedMaybe n -- systemInfoDriver :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoDriverStatus :: Maybe [[Text]]
    <*> arbitraryReducedMaybe n -- systemInfoDockerRootDir :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoSystemStatus :: Maybe [[Text]]
    <*> arbitraryReducedMaybe n -- systemInfoPlugins :: Maybe PluginsInfo
    <*> arbitraryReducedMaybe n -- systemInfoMemoryLimit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemInfoSwapLimit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemInfoKernelMemory :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemInfoCpuCfsPeriod :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemInfoCpuCfsQuota :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemInfoCpuShares :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemInfoCpuSet :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemInfoOomKillDisable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemInfoIpv4Forwarding :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemInfoBridgeNfIptables :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemInfoBridgeNfIp6tables :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemInfoDebug :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemInfoNfd :: Maybe Int
    <*> arbitraryReducedMaybe n -- systemInfoNGoroutines :: Maybe Int
    <*> arbitraryReducedMaybe n -- systemInfoSystemTime :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoLoggingDriver :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoCgroupDriver :: Maybe E'CgroupDriver
    <*> arbitraryReducedMaybe n -- systemInfoNEventsListener :: Maybe Int
    <*> arbitraryReducedMaybe n -- systemInfoKernelVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoOperatingSystem :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoOsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoArchitecture :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoNcpu :: Maybe Int
    <*> arbitraryReducedMaybe n -- systemInfoMemTotal :: Maybe Integer
    <*> arbitraryReducedMaybe n -- systemInfoIndexServerAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoRegistryConfig :: Maybe RegistryServiceConfig
    <*> arbitraryReducedMaybe n -- systemInfoGenericResources :: Maybe [GenericResourcesInner]
    <*> arbitraryReducedMaybe n -- systemInfoHttpProxy :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoHttpsProxy :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoNoProxy :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoName :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoLabels :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- systemInfoExperimentalBuild :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemInfoServerVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoClusterStore :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoClusterAdvertise :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoRuntimes :: Maybe (Map.Map String Runtime)
    <*> arbitraryReducedMaybe n -- systemInfoDefaultRuntime :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoSwarm :: Maybe SwarmInfo
    <*> arbitraryReducedMaybe n -- systemInfoLiveRestoreEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemInfoIsolation :: Maybe E'Isolation2
    <*> arbitraryReducedMaybe n -- systemInfoInitBinary :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemInfoContainerdCommit :: Maybe Commit
    <*> arbitraryReducedMaybe n -- systemInfoRuncCommit :: Maybe Commit
    <*> arbitraryReducedMaybe n -- systemInfoInitCommit :: Maybe Commit
    <*> arbitraryReducedMaybe n -- systemInfoSecurityOptions :: Maybe [Text]
  
instance Arbitrary SystemVersionResponse where
  arbitrary = sized genSystemVersionResponse

genSystemVersionResponse :: Int -> Gen SystemVersionResponse
genSystemVersionResponse n =
  SystemVersionResponse
    <$> arbitraryReducedMaybe n -- systemVersionResponsePlatform :: Maybe SystemVersionResponsePlatform
    <*> arbitraryReducedMaybe n -- systemVersionResponseComponents :: Maybe [SystemVersionResponseComponentsInner]
    <*> arbitraryReducedMaybe n -- systemVersionResponseVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemVersionResponseApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemVersionResponseMinApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemVersionResponseGitCommit :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemVersionResponseGoVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemVersionResponseOs :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemVersionResponseArch :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemVersionResponseKernelVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- systemVersionResponseExperimental :: Maybe Bool
    <*> arbitraryReducedMaybe n -- systemVersionResponseBuildTime :: Maybe Text
  
instance Arbitrary SystemVersionResponseComponentsInner where
  arbitrary = sized genSystemVersionResponseComponentsInner

genSystemVersionResponseComponentsInner :: Int -> Gen SystemVersionResponseComponentsInner
genSystemVersionResponseComponentsInner n =
  SystemVersionResponseComponentsInner
    <$> arbitrary -- systemVersionResponseComponentsInnerName :: Text
    <*> arbitrary -- systemVersionResponseComponentsInnerVersion :: Text
    <*> arbitraryReducedMaybeValue n -- systemVersionResponseComponentsInnerDetails :: Maybe A.Value
  
instance Arbitrary SystemVersionResponsePlatform where
  arbitrary = sized genSystemVersionResponsePlatform

genSystemVersionResponsePlatform :: Int -> Gen SystemVersionResponsePlatform
genSystemVersionResponsePlatform n =
  SystemVersionResponsePlatform
    <$> arbitrary -- systemVersionResponsePlatformName :: Text
  
instance Arbitrary TLSInfo where
  arbitrary = sized genTLSInfo

genTLSInfo :: Int -> Gen TLSInfo
genTLSInfo n =
  TLSInfo
    <$> arbitraryReducedMaybe n -- tLSInfoTrustRoot :: Maybe Text
    <*> arbitraryReducedMaybe n -- tLSInfoCertIssuerSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- tLSInfoCertIssuerPublicKey :: Maybe Text
  
instance Arbitrary Task where
  arbitrary = sized genTask

genTask :: Int -> Gen Task
genTask n =
  Task
    <$> arbitraryReducedMaybe n -- taskId :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskVersion :: Maybe ObjectVersion
    <*> arbitraryReducedMaybe n -- taskCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskUpdatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskName :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- taskSpec :: Maybe TaskSpec
    <*> arbitraryReducedMaybe n -- taskServiceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSlot :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskNodeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskAssignedGenericResources :: Maybe [GenericResourcesInner]
    <*> arbitraryReducedMaybe n -- taskStatus :: Maybe TaskStatus
    <*> arbitraryReducedMaybe n -- taskDesiredState :: Maybe TaskState
  
instance Arbitrary TaskSpec where
  arbitrary = sized genTaskSpec

genTaskSpec :: Int -> Gen TaskSpec
genTaskSpec n =
  TaskSpec
    <$> arbitraryReducedMaybe n -- taskSpecPluginSpec :: Maybe TaskSpecPluginSpec
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpec :: Maybe TaskSpecContainerSpec
    <*> arbitraryReducedMaybe n -- taskSpecResources :: Maybe TaskSpecResources
    <*> arbitraryReducedMaybe n -- taskSpecRestartPolicy :: Maybe TaskSpecRestartPolicy
    <*> arbitraryReducedMaybe n -- taskSpecPlacement :: Maybe TaskSpecPlacement
    <*> arbitraryReducedMaybe n -- taskSpecForceUpdate :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskSpecRuntime :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecNetworks :: Maybe [TaskSpecNetworksInner]
    <*> arbitraryReducedMaybe n -- taskSpecLogDriver :: Maybe TaskSpecLogDriver
  
instance Arbitrary TaskSpecContainerSpec where
  arbitrary = sized genTaskSpecContainerSpec

genTaskSpecContainerSpec :: Int -> Gen TaskSpecContainerSpec
genTaskSpecContainerSpec n =
  TaskSpecContainerSpec
    <$> arbitraryReducedMaybe n -- taskSpecContainerSpecImage :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecCommand :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecArgs :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecHostname :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecEnv :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecDir :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecUser :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecPrivileges :: Maybe TaskSpecContainerSpecPrivileges
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecTty :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecOpenStdin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecMounts :: Maybe [Mount]
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecStopSignal :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecStopGracePeriod :: Maybe Integer
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecHealthCheck :: Maybe HealthConfig
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecHosts :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecDnsConfig :: Maybe TaskSpecContainerSpecDNSConfig
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecSecrets :: Maybe [TaskSpecContainerSpecSecretsInner]
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecConfigs :: Maybe [TaskSpecContainerSpecConfigsInner]
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecIsolation :: Maybe E'Isolation
  
instance Arbitrary TaskSpecContainerSpecConfigsInner where
  arbitrary = sized genTaskSpecContainerSpecConfigsInner

genTaskSpecContainerSpecConfigsInner :: Int -> Gen TaskSpecContainerSpecConfigsInner
genTaskSpecContainerSpecConfigsInner n =
  TaskSpecContainerSpecConfigsInner
    <$> arbitraryReducedMaybe n -- taskSpecContainerSpecConfigsInnerFile :: Maybe TaskSpecContainerSpecSecretsInnerFile
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecConfigsInnerConfigId :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecConfigsInnerConfigName :: Maybe Text
  
instance Arbitrary TaskSpecContainerSpecDNSConfig where
  arbitrary = sized genTaskSpecContainerSpecDNSConfig

genTaskSpecContainerSpecDNSConfig :: Int -> Gen TaskSpecContainerSpecDNSConfig
genTaskSpecContainerSpecDNSConfig n =
  TaskSpecContainerSpecDNSConfig
    <$> arbitraryReducedMaybe n -- taskSpecContainerSpecDNSConfigNameservers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecDNSConfigSearch :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecDNSConfigOptions :: Maybe [Text]
  
instance Arbitrary TaskSpecContainerSpecPrivileges where
  arbitrary = sized genTaskSpecContainerSpecPrivileges

genTaskSpecContainerSpecPrivileges :: Int -> Gen TaskSpecContainerSpecPrivileges
genTaskSpecContainerSpecPrivileges n =
  TaskSpecContainerSpecPrivileges
    <$> arbitraryReducedMaybe n -- taskSpecContainerSpecPrivilegesCredentialSpec :: Maybe TaskSpecContainerSpecPrivilegesCredentialSpec
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecPrivilegesSeLinuxContext :: Maybe TaskSpecContainerSpecPrivilegesSELinuxContext
  
instance Arbitrary TaskSpecContainerSpecPrivilegesCredentialSpec where
  arbitrary = sized genTaskSpecContainerSpecPrivilegesCredentialSpec

genTaskSpecContainerSpecPrivilegesCredentialSpec :: Int -> Gen TaskSpecContainerSpecPrivilegesCredentialSpec
genTaskSpecContainerSpecPrivilegesCredentialSpec n =
  TaskSpecContainerSpecPrivilegesCredentialSpec
    <$> arbitraryReducedMaybe n -- taskSpecContainerSpecPrivilegesCredentialSpecFile :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecPrivilegesCredentialSpecRegistry :: Maybe Text
  
instance Arbitrary TaskSpecContainerSpecPrivilegesSELinuxContext where
  arbitrary = sized genTaskSpecContainerSpecPrivilegesSELinuxContext

genTaskSpecContainerSpecPrivilegesSELinuxContext :: Int -> Gen TaskSpecContainerSpecPrivilegesSELinuxContext
genTaskSpecContainerSpecPrivilegesSELinuxContext n =
  TaskSpecContainerSpecPrivilegesSELinuxContext
    <$> arbitraryReducedMaybe n -- taskSpecContainerSpecPrivilegesSELinuxContextDisable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecPrivilegesSELinuxContextUser :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecPrivilegesSELinuxContextRole :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecPrivilegesSELinuxContextType :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecPrivilegesSELinuxContextLevel :: Maybe Text
  
instance Arbitrary TaskSpecContainerSpecSecretsInner where
  arbitrary = sized genTaskSpecContainerSpecSecretsInner

genTaskSpecContainerSpecSecretsInner :: Int -> Gen TaskSpecContainerSpecSecretsInner
genTaskSpecContainerSpecSecretsInner n =
  TaskSpecContainerSpecSecretsInner
    <$> arbitraryReducedMaybe n -- taskSpecContainerSpecSecretsInnerFile :: Maybe TaskSpecContainerSpecSecretsInnerFile
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecSecretsInnerSecretId :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecSecretsInnerSecretName :: Maybe Text
  
instance Arbitrary TaskSpecContainerSpecSecretsInnerFile where
  arbitrary = sized genTaskSpecContainerSpecSecretsInnerFile

genTaskSpecContainerSpecSecretsInnerFile :: Int -> Gen TaskSpecContainerSpecSecretsInnerFile
genTaskSpecContainerSpecSecretsInnerFile n =
  TaskSpecContainerSpecSecretsInnerFile
    <$> arbitraryReducedMaybe n -- taskSpecContainerSpecSecretsInnerFileName :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecSecretsInnerFileUid :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecSecretsInnerFileGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecSecretsInnerFileMode :: Maybe Int
  
instance Arbitrary TaskSpecLogDriver where
  arbitrary = sized genTaskSpecLogDriver

genTaskSpecLogDriver :: Int -> Gen TaskSpecLogDriver
genTaskSpecLogDriver n =
  TaskSpecLogDriver
    <$> arbitraryReducedMaybe n -- taskSpecLogDriverName :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecLogDriverOptions :: Maybe (Map.Map String Text)
  
instance Arbitrary TaskSpecNetworksInner where
  arbitrary = sized genTaskSpecNetworksInner

genTaskSpecNetworksInner :: Int -> Gen TaskSpecNetworksInner
genTaskSpecNetworksInner n =
  TaskSpecNetworksInner
    <$> arbitraryReducedMaybe n -- taskSpecNetworksInnerTarget :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecNetworksInnerAliases :: Maybe [Text]
  
instance Arbitrary TaskSpecPlacement where
  arbitrary = sized genTaskSpecPlacement

genTaskSpecPlacement :: Int -> Gen TaskSpecPlacement
genTaskSpecPlacement n =
  TaskSpecPlacement
    <$> arbitraryReducedMaybe n -- taskSpecPlacementConstraints :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskSpecPlacementPreferences :: Maybe [TaskSpecPlacementPreferencesInner]
    <*> arbitraryReducedMaybe n -- taskSpecPlacementPlatforms :: Maybe [Platform]
  
instance Arbitrary TaskSpecPlacementPreferencesInner where
  arbitrary = sized genTaskSpecPlacementPreferencesInner

genTaskSpecPlacementPreferencesInner :: Int -> Gen TaskSpecPlacementPreferencesInner
genTaskSpecPlacementPreferencesInner n =
  TaskSpecPlacementPreferencesInner
    <$> arbitraryReducedMaybe n -- taskSpecPlacementPreferencesInnerSpread :: Maybe TaskSpecPlacementPreferencesInnerSpread
  
instance Arbitrary TaskSpecPlacementPreferencesInnerSpread where
  arbitrary = sized genTaskSpecPlacementPreferencesInnerSpread

genTaskSpecPlacementPreferencesInnerSpread :: Int -> Gen TaskSpecPlacementPreferencesInnerSpread
genTaskSpecPlacementPreferencesInnerSpread n =
  TaskSpecPlacementPreferencesInnerSpread
    <$> arbitraryReducedMaybe n -- taskSpecPlacementPreferencesInnerSpreadSpreadDescriptor :: Maybe Text
  
instance Arbitrary TaskSpecPluginSpec where
  arbitrary = sized genTaskSpecPluginSpec

genTaskSpecPluginSpec :: Int -> Gen TaskSpecPluginSpec
genTaskSpecPluginSpec n =
  TaskSpecPluginSpec
    <$> arbitraryReducedMaybe n -- taskSpecPluginSpecName :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecPluginSpecRemote :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecPluginSpecDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- taskSpecPluginSpecPluginPrivilege :: Maybe [PluginPullRequestInner]
  
instance Arbitrary TaskSpecResources where
  arbitrary = sized genTaskSpecResources

genTaskSpecResources :: Int -> Gen TaskSpecResources
genTaskSpecResources n =
  TaskSpecResources
    <$> arbitraryReducedMaybe n -- taskSpecResourcesLimits :: Maybe ResourceObject
    <*> arbitraryReducedMaybe n -- taskSpecResourcesReservations :: Maybe ResourceObject
  
instance Arbitrary TaskSpecRestartPolicy where
  arbitrary = sized genTaskSpecRestartPolicy

genTaskSpecRestartPolicy :: Int -> Gen TaskSpecRestartPolicy
genTaskSpecRestartPolicy n =
  TaskSpecRestartPolicy
    <$> arbitraryReducedMaybe n -- taskSpecRestartPolicyCondition :: Maybe E'Condition
    <*> arbitraryReducedMaybe n -- taskSpecRestartPolicyDelay :: Maybe Integer
    <*> arbitraryReducedMaybe n -- taskSpecRestartPolicyMaxAttempts :: Maybe Integer
    <*> arbitraryReducedMaybe n -- taskSpecRestartPolicyWindow :: Maybe Integer
  
instance Arbitrary TaskStatus where
  arbitrary = sized genTaskStatus

genTaskStatus :: Int -> Gen TaskStatus
genTaskStatus n =
  TaskStatus
    <$> arbitraryReducedMaybe n -- taskStatusTimestamp :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskStatusState :: Maybe TaskState
    <*> arbitraryReducedMaybe n -- taskStatusMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskStatusErr :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskStatusContainerStatus :: Maybe TaskStatusContainerStatus
  
instance Arbitrary TaskStatusContainerStatus where
  arbitrary = sized genTaskStatusContainerStatus

genTaskStatusContainerStatus :: Int -> Gen TaskStatusContainerStatus
genTaskStatusContainerStatus n =
  TaskStatusContainerStatus
    <$> arbitraryReducedMaybe n -- taskStatusContainerStatusContainerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskStatusContainerStatusPid :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskStatusContainerStatusExitCode :: Maybe Int
  
instance Arbitrary ThrottleDevice where
  arbitrary = sized genThrottleDevice

genThrottleDevice :: Int -> Gen ThrottleDevice
genThrottleDevice n =
  ThrottleDevice
    <$> arbitraryReducedMaybe n -- throttleDevicePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- throttleDeviceRate :: Maybe Integer
  
instance Arbitrary UnlockKeyResponse where
  arbitrary = sized genUnlockKeyResponse

genUnlockKeyResponse :: Int -> Gen UnlockKeyResponse
genUnlockKeyResponse n =
  UnlockKeyResponse
    <$> arbitraryReducedMaybe n -- unlockKeyResponseUnlockKey :: Maybe Text
  
instance Arbitrary Volume where
  arbitrary = sized genVolume

genVolume :: Int -> Gen Volume
genVolume n =
  Volume
    <$> arbitrary -- volumeName :: Text
    <*> arbitrary -- volumeDriver :: Text
    <*> arbitrary -- volumeMountpoint :: Text
    <*> arbitraryReducedMaybe n -- volumeCreatedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- volumeStatus :: Maybe (Map.Map String A.Value)
    <*> arbitrary -- volumeLabels :: (Map.Map String Text)
    <*> arbitrary -- volumeScope :: E'Scope
    <*> arbitrary -- volumeOptions :: (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- volumeUsageData :: Maybe VolumeUsageData
  
instance Arbitrary VolumeCreateRequest where
  arbitrary = sized genVolumeCreateRequest

genVolumeCreateRequest :: Int -> Gen VolumeCreateRequest
genVolumeCreateRequest n =
  VolumeCreateRequest
    <$> arbitraryReducedMaybe n -- volumeCreateRequestName :: Maybe Text
    <*> arbitraryReducedMaybe n -- volumeCreateRequestDriver :: Maybe Text
    <*> arbitraryReducedMaybe n -- volumeCreateRequestDriverOpts :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- volumeCreateRequestLabels :: Maybe (Map.Map String Text)
  
instance Arbitrary VolumeListResponse where
  arbitrary = sized genVolumeListResponse

genVolumeListResponse :: Int -> Gen VolumeListResponse
genVolumeListResponse n =
  VolumeListResponse
    <$> arbitraryReduced n -- volumeListResponseVolumes :: [Volume]
    <*> arbitrary -- volumeListResponseWarnings :: [Text]
  
instance Arbitrary VolumePruneResponse where
  arbitrary = sized genVolumePruneResponse

genVolumePruneResponse :: Int -> Gen VolumePruneResponse
genVolumePruneResponse n =
  VolumePruneResponse
    <$> arbitraryReducedMaybe n -- volumePruneResponseVolumesDeleted :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- volumePruneResponseSpaceReclaimed :: Maybe Integer
  
instance Arbitrary VolumeUsageData where
  arbitrary = sized genVolumeUsageData

genVolumeUsageData :: Int -> Gen VolumeUsageData
genVolumeUsageData n =
  VolumeUsageData
    <$> arbitrary -- volumeUsageDataSize :: Int
    <*> arbitrary -- volumeUsageDataRefCount :: Int
  



instance Arbitrary E'Availability where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'CgroupDriver where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Condition where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Condition2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ContentType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'FailureAction where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'FailureAction2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Isolation where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Isolation2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Mode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Name where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Order where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Propagation where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Protocol where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'PublishMode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Role where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Scope where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'State where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type4 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LocalNodeState where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary NodeState where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Reachability where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary TaskState where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary (HM.HashMap String String) where arbitrary = HM.fromList <$> arbitrary
