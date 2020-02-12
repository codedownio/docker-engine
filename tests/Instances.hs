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

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
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
  
instance Arbitrary ConfigSpec where
  arbitrary = sized genConfigSpec

genConfigSpec :: Int -> Gen ConfigSpec
genConfigSpec n =
  ConfigSpec
    <$> arbitraryReducedMaybe n -- configSpecName :: Maybe Text
    <*> arbitraryReducedMaybe n -- configSpecLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- configSpecData :: Maybe [Text]
  
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
    <*> arbitraryReducedMaybe n -- containerConfigHealthcheck :: Maybe HealthConfig
    <*> arbitraryReducedMaybe n -- containerConfigArgsEscaped :: Maybe Bool
    <*> arbitraryReducedMaybe n -- containerConfigImage :: Maybe Text
    <*> arbitraryReducedMaybe n -- containerConfigVolumes :: Maybe ContainerConfigVolumes
    <*> arbitraryReducedMaybe n -- containerConfigWorkingDir :: Maybe Text
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
    <*> arbitraryReducedMaybe n -- containerSummaryMounts :: Maybe [Mount]
  
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
  
instance Arbitrary CreateImageInfo where
  arbitrary = sized genCreateImageInfo

genCreateImageInfo :: Int -> Gen CreateImageInfo
genCreateImageInfo n =
  CreateImageInfo
    <$> arbitraryReducedMaybe n -- createImageInfoError :: Maybe Text
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
  
instance Arbitrary EndpointPortConfig where
  arbitrary = sized genEndpointPortConfig

genEndpointPortConfig :: Int -> Gen EndpointPortConfig
genEndpointPortConfig n =
  EndpointPortConfig
    <$> arbitraryReducedMaybe n -- endpointPortConfigName :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointPortConfigProtocol :: Maybe E'Type
    <*> arbitraryReducedMaybe n -- endpointPortConfigTargetPort :: Maybe Int
    <*> arbitraryReducedMaybe n -- endpointPortConfigPublishedPort :: Maybe Int
  
instance Arbitrary EndpointSettings where
  arbitrary = sized genEndpointSettings

genEndpointSettings :: Int -> Gen EndpointSettings
genEndpointSettings n =
  EndpointSettings
    <$> arbitraryReducedMaybe n -- endpointSettingsIpamConfig :: Maybe EndpointSettingsIPAMConfig
    <*> arbitraryReducedMaybe n -- endpointSettingsLinks :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- endpointSettingsAliases :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- endpointSettingsNetworkId :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsEndpointId :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsGateway :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsIpAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsIpPrefixLen :: Maybe Int
    <*> arbitraryReducedMaybe n -- endpointSettingsIPv6Gateway :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsGlobalIPv6Address :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsGlobalIPv6PrefixLen :: Maybe Integer
    <*> arbitraryReducedMaybe n -- endpointSettingsMacAddress :: Maybe Text
  
instance Arbitrary EndpointSettingsIPAMConfig where
  arbitrary = sized genEndpointSettingsIPAMConfig

genEndpointSettingsIPAMConfig :: Int -> Gen EndpointSettingsIPAMConfig
genEndpointSettingsIPAMConfig n =
  EndpointSettingsIPAMConfig
    <$> arbitraryReducedMaybe n -- endpointSettingsIPAMConfigIPv4Address :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsIPAMConfigIPv6Address :: Maybe Text
    <*> arbitraryReducedMaybe n -- endpointSettingsIPAMConfigLinkLocalIPs :: Maybe [Text]
  
instance Arbitrary EndpointSpec where
  arbitrary = sized genEndpointSpec

genEndpointSpec :: Int -> Gen EndpointSpec
genEndpointSpec n =
  EndpointSpec
    <$> arbitraryReducedMaybe n -- endpointSpecMode :: Maybe E'Mode
    <*> arbitraryReducedMaybe n -- endpointSpecPorts :: Maybe [EndpointPortConfig]
  
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
  
instance Arbitrary HostConfig where
  arbitrary = sized genHostConfig

genHostConfig :: Int -> Gen HostConfig
genHostConfig n =
  HostConfig
    <$> arbitraryReducedMaybe n -- hostConfigCpuShares :: Maybe Int
    <*> arbitraryReducedMaybe n -- hostConfigMemory :: Maybe Int
    <*> arbitraryReducedMaybe n -- hostConfigCgroupParent :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigBlkioWeight :: Maybe Int
    <*> arbitraryReducedMaybe n -- hostConfigBlkioWeightDevice :: Maybe [ResourcesBlkioWeightDevice]
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
    <*> arbitraryReducedMaybe n -- hostConfigNanoCpUs :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigOomKillDisable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- hostConfigPidsLimit :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigUlimits :: Maybe [ResourcesUlimits]
    <*> arbitraryReducedMaybe n -- hostConfigCpuCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigCpuPercent :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigIoMaximumIOps :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigIoMaximumBandwidth :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hostConfigBinds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigContainerIdFile :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigLogConfig :: Maybe HostConfigAllOfLogConfig
    <*> arbitraryReducedMaybe n -- hostConfigNetworkMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigPortBindings :: Maybe (Map.Map String HostConfigAllOfPortBindings)
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
  
instance Arbitrary HostConfigAllOf where
  arbitrary = sized genHostConfigAllOf

genHostConfigAllOf :: Int -> Gen HostConfigAllOf
genHostConfigAllOf n =
  HostConfigAllOf
    <$> arbitraryReducedMaybe n -- hostConfigAllOfBinds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigAllOfContainerIdFile :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigAllOfLogConfig :: Maybe HostConfigAllOfLogConfig
    <*> arbitraryReducedMaybe n -- hostConfigAllOfNetworkMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigAllOfPortBindings :: Maybe (Map.Map String HostConfigAllOfPortBindings)
    <*> arbitraryReducedMaybe n -- hostConfigAllOfRestartPolicy :: Maybe RestartPolicy
    <*> arbitraryReducedMaybe n -- hostConfigAllOfAutoRemove :: Maybe Bool
    <*> arbitraryReducedMaybe n -- hostConfigAllOfVolumeDriver :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigAllOfVolumesFrom :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigAllOfMounts :: Maybe [Mount]
    <*> arbitraryReducedMaybe n -- hostConfigAllOfCapAdd :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigAllOfCapDrop :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigAllOfDns :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigAllOfDnsOptions :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigAllOfDnsSearch :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigAllOfExtraHosts :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigAllOfGroupAdd :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigAllOfIpcMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigAllOfCgroup :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigAllOfLinks :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigAllOfOomScoreAdj :: Maybe Int
    <*> arbitraryReducedMaybe n -- hostConfigAllOfPidMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigAllOfPrivileged :: Maybe Bool
    <*> arbitraryReducedMaybe n -- hostConfigAllOfPublishAllPorts :: Maybe Bool
    <*> arbitraryReducedMaybe n -- hostConfigAllOfReadonlyRootfs :: Maybe Bool
    <*> arbitraryReducedMaybe n -- hostConfigAllOfSecurityOpt :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hostConfigAllOfStorageOpt :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- hostConfigAllOfTmpfs :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- hostConfigAllOfUtsMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigAllOfUsernsMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigAllOfShmSize :: Maybe Int
    <*> arbitraryReducedMaybe n -- hostConfigAllOfSysctls :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- hostConfigAllOfRuntime :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigAllOfConsoleSize :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- hostConfigAllOfIsolation :: Maybe E'Isolation
  
instance Arbitrary HostConfigAllOfLogConfig where
  arbitrary = sized genHostConfigAllOfLogConfig

genHostConfigAllOfLogConfig :: Int -> Gen HostConfigAllOfLogConfig
genHostConfigAllOfLogConfig n =
  HostConfigAllOfLogConfig
    <$> arbitraryReducedMaybe n -- hostConfigAllOfLogConfigType :: Maybe E'Type3
    <*> arbitraryReducedMaybe n -- hostConfigAllOfLogConfigConfig :: Maybe (Map.Map String Text)
  
instance Arbitrary HostConfigAllOfPortBindings where
  arbitrary = sized genHostConfigAllOfPortBindings

genHostConfigAllOfPortBindings :: Int -> Gen HostConfigAllOfPortBindings
genHostConfigAllOfPortBindings n =
  HostConfigAllOfPortBindings
    <$> arbitraryReducedMaybe n -- hostConfigAllOfPortBindingsHostIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- hostConfigAllOfPortBindingsHostPort :: Maybe Text
  
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
  
instance Arbitrary ImageDeleteResponseItem where
  arbitrary = sized genImageDeleteResponseItem

genImageDeleteResponseItem :: Int -> Gen ImageDeleteResponseItem
genImageDeleteResponseItem n =
  ImageDeleteResponseItem
    <$> arbitraryReducedMaybe n -- imageDeleteResponseItemUntagged :: Maybe Text
    <*> arbitraryReducedMaybe n -- imageDeleteResponseItemDeleted :: Maybe Text
  
instance Arbitrary ImageRootFS where
  arbitrary = sized genImageRootFS

genImageRootFS :: Int -> Gen ImageRootFS
genImageRootFS n =
  ImageRootFS
    <$> arbitrary -- imageRootFSType :: Text
    <*> arbitraryReducedMaybe n -- imageRootFSLayers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- imageRootFSBaseLayer :: Maybe Text
  
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
  
instance Arbitrary InlineObject where
  arbitrary = sized genInlineObject

genInlineObject :: Int -> Gen InlineObject
genInlineObject n =
  InlineObject
    <$> arbitraryReducedMaybe n -- inlineObjectName :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineObjectDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineObjectValue :: Maybe [Text]
  
instance Arbitrary InlineObject1 where
  arbitrary = sized genInlineObject1

genInlineObject1 :: Int -> Gen InlineObject1
genInlineObject1 n =
  InlineObject1
    <$> arbitraryReducedMaybe n -- inlineObject1Detach :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineObject1Tty :: Maybe Bool
  
instance Arbitrary InlineObject2 where
  arbitrary = sized genInlineObject2

genInlineObject2 :: Int -> Gen InlineObject2
genInlineObject2 n =
  InlineObject2
    <$> arbitraryReducedMaybe n -- inlineObject2Name :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineObject2Driver :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineObject2DriverOpts :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- inlineObject2Labels :: Maybe (Map.Map String Text)
  
instance Arbitrary InlineObject3 where
  arbitrary = sized genInlineObject3

genInlineObject3 :: Int -> Gen InlineObject3
genInlineObject3 n =
  InlineObject3
    <$> arbitrary -- inlineObject3Name :: Text
    <*> arbitraryReducedMaybe n -- inlineObject3CheckDuplicate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineObject3Driver :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineObject3Internal :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineObject3Attachable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineObject3Ingress :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineObject3Ipam :: Maybe IPAM
    <*> arbitraryReducedMaybe n -- inlineObject3EnableIPv6 :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineObject3Options :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- inlineObject3Labels :: Maybe (Map.Map String Text)
  
instance Arbitrary InlineObject4 where
  arbitrary = sized genInlineObject4

genInlineObject4 :: Int -> Gen InlineObject4
genInlineObject4 n =
  InlineObject4
    <$> arbitraryReducedMaybe n -- inlineObject4Container :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineObject4EndpointConfig :: Maybe EndpointSettings
  
instance Arbitrary InlineObject5 where
  arbitrary = sized genInlineObject5

genInlineObject5 :: Int -> Gen InlineObject5
genInlineObject5 n =
  InlineObject5
    <$> arbitraryReducedMaybe n -- inlineObject5Container :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineObject5Force :: Maybe Bool
  
instance Arbitrary InlineObject6 where
  arbitrary = sized genInlineObject6

genInlineObject6 :: Int -> Gen InlineObject6
genInlineObject6 n =
  InlineObject6
    <$> arbitraryReducedMaybe n -- inlineObject6ListenAddr :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineObject6AdvertiseAddr :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineObject6DataPathAddr :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineObject6ForceNewCluster :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineObject6Spec :: Maybe SwarmSpec
  
instance Arbitrary InlineObject7 where
  arbitrary = sized genInlineObject7

genInlineObject7 :: Int -> Gen InlineObject7
genInlineObject7 n =
  InlineObject7
    <$> arbitraryReducedMaybe n -- inlineObject7ListenAddr :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineObject7AdvertiseAddr :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineObject7DataPathAddr :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineObject7RemoteAddrs :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineObject7JoinToken :: Maybe Text
  
instance Arbitrary InlineObject8 where
  arbitrary = sized genInlineObject8

genInlineObject8 :: Int -> Gen InlineObject8
genInlineObject8 n =
  InlineObject8
    <$> arbitraryReducedMaybe n -- inlineObject8UnlockKey :: Maybe Text
  
instance Arbitrary InlineResponse200 where
  arbitrary = sized genInlineResponse200

genInlineResponse200 :: Int -> Gen InlineResponse200
genInlineResponse200 n =
  InlineResponse200
    <$> arbitraryReducedMaybe n -- inlineResponse200Titles :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- inlineResponse200Processes :: Maybe [[Text]]
  
instance Arbitrary InlineResponse2001 where
  arbitrary = sized genInlineResponse2001

genInlineResponse2001 :: Int -> Gen InlineResponse2001
genInlineResponse2001 n =
  InlineResponse2001
    <$> arbitrary -- inlineResponse2001Path :: Text
    <*> arbitrary -- inlineResponse2001Kind :: Int
  
instance Arbitrary InlineResponse20010 where
  arbitrary = sized genInlineResponse20010

genInlineResponse20010 :: Int -> Gen InlineResponse20010
genInlineResponse20010 n =
  InlineResponse20010
    <$> arbitraryReducedMaybe n -- inlineResponse20010Version :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20010ApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20010MinApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20010GitCommit :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20010GoVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20010Os :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20010Arch :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20010KernelVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20010Experimental :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse20010BuildTime :: Maybe Text
  
instance Arbitrary InlineResponse20011 where
  arbitrary = sized genInlineResponse20011

genInlineResponse20011 :: Int -> Gen InlineResponse20011
genInlineResponse20011 n =
  InlineResponse20011
    <$> arbitraryReducedMaybe n -- inlineResponse20011Type :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20011Action :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20011Actor :: Maybe InlineResponse20011Actor
    <*> arbitraryReducedMaybe n -- inlineResponse20011Time :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse20011TimeNano :: Maybe Integer
  
instance Arbitrary InlineResponse20011Actor where
  arbitrary = sized genInlineResponse20011Actor

genInlineResponse20011Actor :: Int -> Gen InlineResponse20011Actor
genInlineResponse20011Actor n =
  InlineResponse20011Actor
    <$> arbitraryReducedMaybe n -- inlineResponse20011ActorId :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20011ActorAttributes :: Maybe (Map.Map String Text)
  
instance Arbitrary InlineResponse20012 where
  arbitrary = sized genInlineResponse20012

genInlineResponse20012 :: Int -> Gen InlineResponse20012
genInlineResponse20012 n =
  InlineResponse20012
    <$> arbitraryReducedMaybe n -- inlineResponse20012LayersSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- inlineResponse20012Images :: Maybe [ImageSummary]
    <*> arbitraryReducedMaybe n -- inlineResponse20012Containers :: Maybe [ContainerSummary]
    <*> arbitraryReducedMaybe n -- inlineResponse20012Volumes :: Maybe [Volume]
  
instance Arbitrary InlineResponse20013 where
  arbitrary = sized genInlineResponse20013

genInlineResponse20013 :: Int -> Gen InlineResponse20013
genInlineResponse20013 n =
  InlineResponse20013
    <$> arbitraryReducedMaybe n -- inlineResponse20013Id :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20013Running :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse20013ExitCode :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse20013ProcessConfig :: Maybe ProcessConfig
    <*> arbitraryReducedMaybe n -- inlineResponse20013OpenStdin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse20013OpenStderr :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse20013OpenStdout :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse20013ContainerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20013Pid :: Maybe Int
  
instance Arbitrary InlineResponse20014 where
  arbitrary = sized genInlineResponse20014

genInlineResponse20014 :: Int -> Gen InlineResponse20014
genInlineResponse20014 n =
  InlineResponse20014
    <$> arbitraryReduced n -- inlineResponse20014Volumes :: [Volume]
    <*> arbitrary -- inlineResponse20014Warnings :: [Text]
  
instance Arbitrary InlineResponse20015 where
  arbitrary = sized genInlineResponse20015

genInlineResponse20015 :: Int -> Gen InlineResponse20015
genInlineResponse20015 n =
  InlineResponse20015
    <$> arbitraryReducedMaybe n -- inlineResponse20015VolumesDeleted :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- inlineResponse20015SpaceReclaimed :: Maybe Integer
  
instance Arbitrary InlineResponse20016 where
  arbitrary = sized genInlineResponse20016

genInlineResponse20016 :: Int -> Gen InlineResponse20016
genInlineResponse20016 n =
  InlineResponse20016
    <$> arbitraryReducedMaybe n -- inlineResponse20016NetworksDeleted :: Maybe [Text]
  
instance Arbitrary InlineResponse20017 where
  arbitrary = sized genInlineResponse20017

genInlineResponse20017 :: Int -> Gen InlineResponse20017
genInlineResponse20017 n =
  InlineResponse20017
    <$> arbitraryReducedMaybe n -- inlineResponse20017Name :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20017Description :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20017Value :: Maybe [Text]
  
instance Arbitrary InlineResponse20018 where
  arbitrary = sized genInlineResponse20018

genInlineResponse20018 :: Int -> Gen InlineResponse20018
genInlineResponse20018 n =
  InlineResponse20018
    <$> arbitraryReducedMaybe n -- inlineResponse20018ClusterInfo :: Maybe ClusterInfo
    <*> arbitraryReducedMaybe n -- inlineResponse20018JoinTokens :: Maybe InlineResponse20018JoinTokens
  
instance Arbitrary InlineResponse20018JoinTokens where
  arbitrary = sized genInlineResponse20018JoinTokens

genInlineResponse20018JoinTokens :: Int -> Gen InlineResponse20018JoinTokens
genInlineResponse20018JoinTokens n =
  InlineResponse20018JoinTokens
    <$> arbitraryReducedMaybe n -- inlineResponse20018JoinTokensWorker :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20018JoinTokensManager :: Maybe Text
  
instance Arbitrary InlineResponse20019 where
  arbitrary = sized genInlineResponse20019

genInlineResponse20019 :: Int -> Gen InlineResponse20019
genInlineResponse20019 n =
  InlineResponse20019
    <$> arbitraryReducedMaybe n -- inlineResponse20019UnlockKey :: Maybe Text
  
instance Arbitrary InlineResponse2002 where
  arbitrary = sized genInlineResponse2002

genInlineResponse2002 :: Int -> Gen InlineResponse2002
genInlineResponse2002 n =
  InlineResponse2002
    <$> arbitraryReducedMaybe n -- inlineResponse2002Warnings :: Maybe [Text]
  
instance Arbitrary InlineResponse20020 where
  arbitrary = sized genInlineResponse20020

genInlineResponse20020 :: Int -> Gen InlineResponse20020
genInlineResponse20020 n =
  InlineResponse20020
    <$> arbitraryReduced n -- inlineResponse20020Descriptor :: InlineResponse20020Descriptor
    <*> arbitraryReduced n -- inlineResponse20020Platforms :: [InlineResponse20020Platforms]
  
instance Arbitrary InlineResponse20020Descriptor where
  arbitrary = sized genInlineResponse20020Descriptor

genInlineResponse20020Descriptor :: Int -> Gen InlineResponse20020Descriptor
genInlineResponse20020Descriptor n =
  InlineResponse20020Descriptor
    <$> arbitraryReducedMaybe n -- inlineResponse20020DescriptorMediaType :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20020DescriptorSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- inlineResponse20020DescriptorDigest :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20020DescriptorUrLs :: Maybe [Text]
  
instance Arbitrary InlineResponse20020Platforms where
  arbitrary = sized genInlineResponse20020Platforms

genInlineResponse20020Platforms :: Int -> Gen InlineResponse20020Platforms
genInlineResponse20020Platforms n =
  InlineResponse20020Platforms
    <$> arbitraryReducedMaybe n -- inlineResponse20020PlatformsArchitecture :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20020PlatformsOs :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20020PlatformsOsVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20020PlatformsOsFeatures :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- inlineResponse20020PlatformsVariant :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse20020PlatformsFeatures :: Maybe [Text]
  
instance Arbitrary InlineResponse2003 where
  arbitrary = sized genInlineResponse2003

genInlineResponse2003 :: Int -> Gen InlineResponse2003
genInlineResponse2003 n =
  InlineResponse2003
    <$> arbitrary -- inlineResponse2003StatusCode :: Int
  
instance Arbitrary InlineResponse2004 where
  arbitrary = sized genInlineResponse2004

genInlineResponse2004 :: Int -> Gen InlineResponse2004
genInlineResponse2004 n =
  InlineResponse2004
    <$> arbitraryReducedMaybe n -- inlineResponse2004ContainersDeleted :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- inlineResponse2004SpaceReclaimed :: Maybe Integer
  
instance Arbitrary InlineResponse2005 where
  arbitrary = sized genInlineResponse2005

genInlineResponse2005 :: Int -> Gen InlineResponse2005
genInlineResponse2005 n =
  InlineResponse2005
    <$> arbitrary -- inlineResponse2005Id :: Text
    <*> arbitrary -- inlineResponse2005Created :: Integer
    <*> arbitrary -- inlineResponse2005CreatedBy :: Text
    <*> arbitrary -- inlineResponse2005Tags :: [Text]
    <*> arbitrary -- inlineResponse2005Size :: Integer
    <*> arbitrary -- inlineResponse2005Comment :: Text
  
instance Arbitrary InlineResponse2006 where
  arbitrary = sized genInlineResponse2006

genInlineResponse2006 :: Int -> Gen InlineResponse2006
genInlineResponse2006 n =
  InlineResponse2006
    <$> arbitraryReducedMaybe n -- inlineResponse2006Description :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2006IsOfficial :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse2006IsAutomated :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse2006Name :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2006StarCount :: Maybe Int
  
instance Arbitrary InlineResponse2007 where
  arbitrary = sized genInlineResponse2007

genInlineResponse2007 :: Int -> Gen InlineResponse2007
genInlineResponse2007 n =
  InlineResponse2007
    <$> arbitraryReducedMaybe n -- inlineResponse2007ImagesDeleted :: Maybe [ImageDeleteResponseItem]
    <*> arbitraryReducedMaybe n -- inlineResponse2007SpaceReclaimed :: Maybe Integer
  
instance Arbitrary InlineResponse2008 where
  arbitrary = sized genInlineResponse2008

genInlineResponse2008 :: Int -> Gen InlineResponse2008
genInlineResponse2008 n =
  InlineResponse2008
    <$> arbitrary -- inlineResponse2008Status :: Text
    <*> arbitraryReducedMaybe n -- inlineResponse2008IdentityToken :: Maybe Text
  
instance Arbitrary InlineResponse2009 where
  arbitrary = sized genInlineResponse2009

genInlineResponse2009 :: Int -> Gen InlineResponse2009
genInlineResponse2009 n =
  InlineResponse2009
    <$> arbitraryReducedMaybe n -- inlineResponse2009Architecture :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009Containers :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2009ContainersRunning :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2009ContainersStopped :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2009ContainersPaused :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2009CpuCfsPeriod :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse2009CpuCfsQuota :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse2009Debug :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse2009DiscoveryBackend :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009DockerRootDir :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009Driver :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009DriverStatus :: Maybe [[Text]]
    <*> arbitraryReducedMaybe n -- inlineResponse2009SystemStatus :: Maybe [[Text]]
    <*> arbitraryReducedMaybe n -- inlineResponse2009Plugins :: Maybe InlineResponse2009Plugins
    <*> arbitraryReducedMaybe n -- inlineResponse2009ExperimentalBuild :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse2009HttpProxy :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009HttpsProxy :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009Id :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009IPv4Forwarding :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse2009Images :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2009IndexServerAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009InitPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009InitSha1 :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009KernelVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009Labels :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- inlineResponse2009MemTotal :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2009MemoryLimit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse2009Ncpu :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2009NEventsListener :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2009NFd :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2009NGoroutines :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2009Name :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009NoProxy :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009OomKillDisable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse2009OsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009OomScoreAdj :: Maybe Int
    <*> arbitraryReducedMaybe n -- inlineResponse2009OperatingSystem :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009RegistryConfig :: Maybe InlineResponse2009RegistryConfig
    <*> arbitraryReducedMaybe n -- inlineResponse2009SwapLimit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse2009SystemTime :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009ServerVersion :: Maybe Text
  
instance Arbitrary InlineResponse2009Plugins where
  arbitrary = sized genInlineResponse2009Plugins

genInlineResponse2009Plugins :: Int -> Gen InlineResponse2009Plugins
genInlineResponse2009Plugins n =
  InlineResponse2009Plugins
    <$> arbitraryReducedMaybe n -- inlineResponse2009PluginsVolume :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- inlineResponse2009PluginsNetwork :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- inlineResponse2009PluginsLog :: Maybe [Text]
  
instance Arbitrary InlineResponse2009RegistryConfig where
  arbitrary = sized genInlineResponse2009RegistryConfig

genInlineResponse2009RegistryConfig :: Int -> Gen InlineResponse2009RegistryConfig
genInlineResponse2009RegistryConfig n =
  InlineResponse2009RegistryConfig
    <$> arbitraryReducedMaybe n -- inlineResponse2009RegistryConfigIndexConfigs :: Maybe (Map.Map String InlineResponse2009RegistryConfigIndexConfigs)
    <*> arbitraryReducedMaybe n -- inlineResponse2009RegistryConfigInsecureRegistryCidRs :: Maybe [Text]
  
instance Arbitrary InlineResponse2009RegistryConfigIndexConfigs where
  arbitrary = sized genInlineResponse2009RegistryConfigIndexConfigs

genInlineResponse2009RegistryConfigIndexConfigs :: Int -> Gen InlineResponse2009RegistryConfigIndexConfigs
genInlineResponse2009RegistryConfigIndexConfigs n =
  InlineResponse2009RegistryConfigIndexConfigs
    <$> arbitraryReducedMaybe n -- inlineResponse2009RegistryConfigIndexConfigsMirrors :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- inlineResponse2009RegistryConfigIndexConfigsName :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2009RegistryConfigIndexConfigsOfficial :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inlineResponse2009RegistryConfigIndexConfigsSecure :: Maybe Bool
  
instance Arbitrary InlineResponse201 where
  arbitrary = sized genInlineResponse201

genInlineResponse201 :: Int -> Gen InlineResponse201
genInlineResponse201 n =
  InlineResponse201
    <$> arbitrary -- inlineResponse201Id :: Text
    <*> arbitrary -- inlineResponse201Warnings :: [Text]
  
instance Arbitrary InlineResponse2011 where
  arbitrary = sized genInlineResponse2011

genInlineResponse2011 :: Int -> Gen InlineResponse2011
genInlineResponse2011 n =
  InlineResponse2011
    <$> arbitraryReducedMaybe n -- inlineResponse2011Id :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2011Warning :: Maybe Text
  
instance Arbitrary InlineResponse2012 where
  arbitrary = sized genInlineResponse2012

genInlineResponse2012 :: Int -> Gen InlineResponse2012
genInlineResponse2012 n =
  InlineResponse2012
    <$> arbitraryReducedMaybe n -- inlineResponse2012Id :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineResponse2012Warning :: Maybe Text
  
instance Arbitrary InlineResponse2013 where
  arbitrary = sized genInlineResponse2013

genInlineResponse2013 :: Int -> Gen InlineResponse2013
genInlineResponse2013 n =
  InlineResponse2013
    <$> arbitraryReducedMaybe n -- inlineResponse2013Id :: Maybe Text
  
instance Arbitrary InlineResponse2014 where
  arbitrary = sized genInlineResponse2014

genInlineResponse2014 :: Int -> Gen InlineResponse2014
genInlineResponse2014 n =
  InlineResponse2014
    <$> arbitraryReducedMaybe n -- inlineResponse2014Id :: Maybe Text
  
instance Arbitrary InlineResponse400 where
  arbitrary = sized genInlineResponse400

genInlineResponse400 :: Int -> Gen InlineResponse400
genInlineResponse400 n =
  InlineResponse400
    <$> arbitraryReducedMaybe n -- inlineResponse400ErrorResponse :: Maybe ErrorResponse
    <*> arbitraryReducedMaybe n -- inlineResponse400Message :: Maybe Text
  
instance Arbitrary InspectResponse where
  arbitrary = sized genInspectResponse

genInspectResponse :: Int -> Gen InspectResponse
genInspectResponse n =
  InspectResponse
    <$> arbitraryReducedMaybe n -- inspectResponseId :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponseCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponsePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponseArgs :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- inspectResponseState :: Maybe InspectResponseState
    <*> arbitraryReducedMaybe n -- inspectResponseImage :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponseResolvConfPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponseHostnamePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponseHostsPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponseLogPath :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- inspectResponseNode :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- inspectResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponseRestartCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- inspectResponseDriver :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponseMountLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponseProcessLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponseAppArmorProfile :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponseExecIDs :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponseHostConfig :: Maybe HostConfig
    <*> arbitraryReducedMaybe n -- inspectResponseGraphDriver :: Maybe GraphDriverData
    <*> arbitraryReducedMaybe n -- inspectResponseSizeRw :: Maybe Integer
    <*> arbitraryReducedMaybe n -- inspectResponseSizeRootFs :: Maybe Integer
    <*> arbitraryReducedMaybe n -- inspectResponseMounts :: Maybe [MountPoint]
    <*> arbitraryReducedMaybe n -- inspectResponseConfig :: Maybe ContainerConfig
    <*> arbitraryReducedMaybe n -- inspectResponseNetworkSettings :: Maybe NetworkConfig
  
instance Arbitrary InspectResponseState where
  arbitrary = sized genInspectResponseState

genInspectResponseState :: Int -> Gen InspectResponseState
genInspectResponseState n =
  InspectResponseState
    <$> arbitraryReducedMaybe n -- inspectResponseStateStatus :: Maybe E'Status
    <*> arbitraryReducedMaybe n -- inspectResponseStateRunning :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inspectResponseStatePaused :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inspectResponseStateRestarting :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inspectResponseStateOomKilled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inspectResponseStateDead :: Maybe Bool
    <*> arbitraryReducedMaybe n -- inspectResponseStatePid :: Maybe Int
    <*> arbitraryReducedMaybe n -- inspectResponseStateExitCode :: Maybe Int
    <*> arbitraryReducedMaybe n -- inspectResponseStateError :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponseStateStartedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- inspectResponseStateFinishedAt :: Maybe Text
  
instance Arbitrary Mount where
  arbitrary = sized genMount

genMount :: Int -> Gen Mount
genMount n =
  Mount
    <$> arbitraryReducedMaybe n -- mountTarget :: Maybe Text
    <*> arbitraryReducedMaybe n -- mountSource :: Maybe Text
    <*> arbitraryReducedMaybe n -- mountType :: Maybe E'Type2
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
    <$> arbitraryReducedMaybeValue n -- mountBindOptionsPropagation :: Maybe A.Value
  
instance Arbitrary MountPoint where
  arbitrary = sized genMountPoint

genMountPoint :: Int -> Gen MountPoint
genMountPoint n =
  MountPoint
    <$> arbitraryReducedMaybe n -- mountPointType :: Maybe Text
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
    <*> arbitraryReducedMaybe n -- networkEnableIPv6 :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkIpam :: Maybe IPAM
    <*> arbitraryReducedMaybe n -- networkInternal :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkAttachable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkIngress :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkContainers :: Maybe (Map.Map String NetworkContainer)
    <*> arbitraryReducedMaybe n -- networkOptions :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- networkLabels :: Maybe (Map.Map String Text)
  
instance Arbitrary NetworkConfig where
  arbitrary = sized genNetworkConfig

genNetworkConfig :: Int -> Gen NetworkConfig
genNetworkConfig n =
  NetworkConfig
    <$> arbitraryReducedMaybe n -- networkConfigBridge :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkConfigGateway :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkConfigAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkConfigIpPrefixLen :: Maybe Int
    <*> arbitraryReducedMaybe n -- networkConfigMacAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkConfigPortMapping :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkConfigPorts :: Maybe [Port]
  
instance Arbitrary NetworkContainer where
  arbitrary = sized genNetworkContainer

genNetworkContainer :: Int -> Gen NetworkContainer
genNetworkContainer n =
  NetworkContainer
    <$> arbitraryReducedMaybe n -- networkContainerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkContainerEndpointId :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkContainerMacAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkContainerIPv4Address :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkContainerIPv6Address :: Maybe Text
  
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
  
instance Arbitrary NodeDescription where
  arbitrary = sized genNodeDescription

genNodeDescription :: Int -> Gen NodeDescription
genNodeDescription n =
  NodeDescription
    <$> arbitraryReducedMaybe n -- nodeDescriptionHostname :: Maybe Text
    <*> arbitraryReducedMaybe n -- nodeDescriptionPlatform :: Maybe NodeDescriptionPlatform
    <*> arbitraryReducedMaybe n -- nodeDescriptionResources :: Maybe NodeDescriptionResources
    <*> arbitraryReducedMaybe n -- nodeDescriptionEngine :: Maybe NodeDescriptionEngine
    <*> arbitraryReducedMaybe n -- nodeDescriptionTlsInfo :: Maybe SwarmSpec
  
instance Arbitrary NodeDescriptionEngine where
  arbitrary = sized genNodeDescriptionEngine

genNodeDescriptionEngine :: Int -> Gen NodeDescriptionEngine
genNodeDescriptionEngine n =
  NodeDescriptionEngine
    <$> arbitraryReducedMaybe n -- nodeDescriptionEngineEngineVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- nodeDescriptionEngineLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- nodeDescriptionEnginePlugins :: Maybe [NodeDescriptionEnginePlugins]
  
instance Arbitrary NodeDescriptionEnginePlugins where
  arbitrary = sized genNodeDescriptionEnginePlugins

genNodeDescriptionEnginePlugins :: Int -> Gen NodeDescriptionEnginePlugins
genNodeDescriptionEnginePlugins n =
  NodeDescriptionEnginePlugins
    <$> arbitraryReducedMaybe n -- nodeDescriptionEnginePluginsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- nodeDescriptionEnginePluginsName :: Maybe Text
  
instance Arbitrary NodeDescriptionPlatform where
  arbitrary = sized genNodeDescriptionPlatform

genNodeDescriptionPlatform :: Int -> Gen NodeDescriptionPlatform
genNodeDescriptionPlatform n =
  NodeDescriptionPlatform
    <$> arbitraryReducedMaybe n -- nodeDescriptionPlatformArchitecture :: Maybe Text
    <*> arbitraryReducedMaybe n -- nodeDescriptionPlatformOs :: Maybe Text
  
instance Arbitrary NodeDescriptionResources where
  arbitrary = sized genNodeDescriptionResources

genNodeDescriptionResources :: Int -> Gen NodeDescriptionResources
genNodeDescriptionResources n =
  NodeDescriptionResources
    <$> arbitraryReducedMaybe n -- nodeDescriptionResourcesNanoCpUs :: Maybe Integer
    <*> arbitraryReducedMaybe n -- nodeDescriptionResourcesMemoryBytes :: Maybe Integer
  
instance Arbitrary NodeSpec where
  arbitrary = sized genNodeSpec

genNodeSpec :: Int -> Gen NodeSpec
genNodeSpec n =
  NodeSpec
    <$> arbitraryReducedMaybe n -- nodeSpecName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nodeSpecLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- nodeSpecRole :: Maybe E'Role
    <*> arbitraryReducedMaybe n -- nodeSpecAvailability :: Maybe E'Availability
  
instance Arbitrary ObjectVersion where
  arbitrary = sized genObjectVersion

genObjectVersion :: Int -> Gen ObjectVersion
genObjectVersion n =
  ObjectVersion
    <$> arbitraryReducedMaybe n -- objectVersionIndex :: Maybe Integer
  
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
  
instance Arbitrary PluginSettings where
  arbitrary = sized genPluginSettings

genPluginSettings :: Int -> Gen PluginSettings
genPluginSettings n =
  PluginSettings
    <$> arbitraryReduced n -- pluginSettingsMounts :: [PluginMount]
    <*> arbitrary -- pluginSettingsEnv :: [Text]
    <*> arbitrary -- pluginSettingsArgs :: [Text]
    <*> arbitraryReduced n -- pluginSettingsDevices :: [PluginDevice]
  
instance Arbitrary Port where
  arbitrary = sized genPort

genPort :: Int -> Gen Port
genPort n =
  Port
    <$> arbitraryReducedMaybe n -- portIp :: Maybe Text
    <*> arbitrary -- portPrivatePort :: Int
    <*> arbitraryReducedMaybe n -- portPublicPort :: Maybe Int
    <*> arbitrary -- portType :: E'Type
  
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
    <$> arbitraryReducedMaybe n -- progressDetailCode :: Maybe Int
    <*> arbitraryReducedMaybe n -- progressDetailMessage :: Maybe Int
  
instance Arbitrary PushImageInfo where
  arbitrary = sized genPushImageInfo

genPushImageInfo :: Int -> Gen PushImageInfo
genPushImageInfo n =
  PushImageInfo
    <$> arbitraryReducedMaybe n -- pushImageInfoError :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushImageInfoStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushImageInfoProgress :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushImageInfoProgressDetail :: Maybe ProgressDetail
  
instance Arbitrary Resources where
  arbitrary = sized genResources

genResources :: Int -> Gen Resources
genResources n =
  Resources
    <$> arbitraryReducedMaybe n -- resourcesCpuShares :: Maybe Int
    <*> arbitraryReducedMaybe n -- resourcesMemory :: Maybe Int
    <*> arbitraryReducedMaybe n -- resourcesCgroupParent :: Maybe Text
    <*> arbitraryReducedMaybe n -- resourcesBlkioWeight :: Maybe Int
    <*> arbitraryReducedMaybe n -- resourcesBlkioWeightDevice :: Maybe [ResourcesBlkioWeightDevice]
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
    <*> arbitraryReducedMaybe n -- resourcesNanoCpUs :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesOomKillDisable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- resourcesPidsLimit :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesUlimits :: Maybe [ResourcesUlimits]
    <*> arbitraryReducedMaybe n -- resourcesCpuCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesCpuPercent :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesIoMaximumIOps :: Maybe Integer
    <*> arbitraryReducedMaybe n -- resourcesIoMaximumBandwidth :: Maybe Integer
  
instance Arbitrary ResourcesBlkioWeightDevice where
  arbitrary = sized genResourcesBlkioWeightDevice

genResourcesBlkioWeightDevice :: Int -> Gen ResourcesBlkioWeightDevice
genResourcesBlkioWeightDevice n =
  ResourcesBlkioWeightDevice
    <$> arbitraryReducedMaybe n -- resourcesBlkioWeightDevicePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- resourcesBlkioWeightDeviceWeight :: Maybe Int
  
instance Arbitrary ResourcesUlimits where
  arbitrary = sized genResourcesUlimits

genResourcesUlimits :: Int -> Gen ResourcesUlimits
genResourcesUlimits n =
  ResourcesUlimits
    <$> arbitraryReducedMaybe n -- resourcesUlimitsName :: Maybe Text
    <*> arbitraryReducedMaybe n -- resourcesUlimitsSoft :: Maybe Int
    <*> arbitraryReducedMaybe n -- resourcesUlimitsHard :: Maybe Int
  
instance Arbitrary RestartPolicy where
  arbitrary = sized genRestartPolicy

genRestartPolicy :: Int -> Gen RestartPolicy
genRestartPolicy n =
  RestartPolicy
    <$> arbitraryReducedMaybe n -- restartPolicyName :: Maybe E'Name
    <*> arbitraryReducedMaybe n -- restartPolicyMaximumRetryCount :: Maybe Int
  
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
  
instance Arbitrary SecretSpec where
  arbitrary = sized genSecretSpec

genSecretSpec :: Int -> Gen SecretSpec
genSecretSpec n =
  SecretSpec
    <$> arbitraryReducedMaybe n -- secretSpecName :: Maybe Text
    <*> arbitraryReducedMaybe n -- secretSpecLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- secretSpecData :: Maybe [Text]
  
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
  
instance Arbitrary ServiceEndpoint where
  arbitrary = sized genServiceEndpoint

genServiceEndpoint :: Int -> Gen ServiceEndpoint
genServiceEndpoint n =
  ServiceEndpoint
    <$> arbitraryReducedMaybe n -- serviceEndpointSpec :: Maybe EndpointSpec
    <*> arbitraryReducedMaybe n -- serviceEndpointPorts :: Maybe [EndpointPortConfig]
    <*> arbitraryReducedMaybe n -- serviceEndpointVirtualIPs :: Maybe [ServiceEndpointVirtualIPs]
  
instance Arbitrary ServiceEndpointVirtualIPs where
  arbitrary = sized genServiceEndpointVirtualIPs

genServiceEndpointVirtualIPs :: Int -> Gen ServiceEndpointVirtualIPs
genServiceEndpointVirtualIPs n =
  ServiceEndpointVirtualIPs
    <$> arbitraryReducedMaybe n -- serviceEndpointVirtualIPsNetworkId :: Maybe Text
    <*> arbitraryReducedMaybe n -- serviceEndpointVirtualIPsAddr :: Maybe Text
  
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
    <*> arbitraryReducedMaybe n -- serviceSpecNetworks :: Maybe [TaskSpecNetworks]
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
    <*> arbitraryReducedMaybe n -- swarmSpecCAConfigExternalCAs :: Maybe [SwarmSpecCAConfigExternalCAs]
    <*> arbitraryReducedMaybe n -- swarmSpecCAConfigSigningCaCert :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmSpecCAConfigSigningCaKey :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- swarmSpecCAConfigForceRotate :: Maybe A.Value
  
instance Arbitrary SwarmSpecCAConfigExternalCAs where
  arbitrary = sized genSwarmSpecCAConfigExternalCAs

genSwarmSpecCAConfigExternalCAs :: Int -> Gen SwarmSpecCAConfigExternalCAs
genSwarmSpecCAConfigExternalCAs n =
  SwarmSpecCAConfigExternalCAs
    <$> arbitraryReducedMaybe n -- swarmSpecCAConfigExternalCAsProtocol :: Maybe E'Protocol
    <*> arbitraryReducedMaybe n -- swarmSpecCAConfigExternalCAsUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- swarmSpecCAConfigExternalCAsOptions :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- swarmSpecCAConfigExternalCAsCaCert :: Maybe Text
  
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
    <$> arbitraryReducedMaybe n -- swarmSpecRaftSnapshotInterval :: Maybe Integer
    <*> arbitraryReducedMaybe n -- swarmSpecRaftKeepOldSnapshots :: Maybe Integer
    <*> arbitraryReducedMaybe n -- swarmSpecRaftLogEntriesForSlowFollowers :: Maybe Integer
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
    <*> arbitraryReducedMaybe n -- taskStatus :: Maybe TaskStatus
    <*> arbitraryReducedMaybe n -- taskDesiredState :: Maybe TaskState
  
instance Arbitrary TaskSpec where
  arbitrary = sized genTaskSpec

genTaskSpec :: Int -> Gen TaskSpec
genTaskSpec n =
  TaskSpec
    <$> arbitraryReducedMaybe n -- taskSpecContainerSpec :: Maybe TaskSpecContainerSpec
    <*> arbitraryReducedMaybe n -- taskSpecResources :: Maybe TaskSpecResources
    <*> arbitraryReducedMaybe n -- taskSpecRestartPolicy :: Maybe TaskSpecRestartPolicy
    <*> arbitraryReducedMaybe n -- taskSpecPlacement :: Maybe TaskSpecPlacement
    <*> arbitraryReducedMaybe n -- taskSpecForceUpdate :: Maybe Int
    <*> arbitraryReducedMaybe n -- taskSpecRuntime :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecNetworks :: Maybe [TaskSpecNetworks]
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
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecSecrets :: Maybe [TaskSpecContainerSpecSecrets]
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecConfigs :: Maybe [TaskSpecContainerSpecConfigs]
  
instance Arbitrary TaskSpecContainerSpecConfigs where
  arbitrary = sized genTaskSpecContainerSpecConfigs

genTaskSpecContainerSpecConfigs :: Int -> Gen TaskSpecContainerSpecConfigs
genTaskSpecContainerSpecConfigs n =
  TaskSpecContainerSpecConfigs
    <$> arbitraryReducedMaybe n -- taskSpecContainerSpecConfigsFile :: Maybe TaskSpecContainerSpecFile
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecConfigsConfigId :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecConfigsConfigName :: Maybe Text
  
instance Arbitrary TaskSpecContainerSpecDNSConfig where
  arbitrary = sized genTaskSpecContainerSpecDNSConfig

genTaskSpecContainerSpecDNSConfig :: Int -> Gen TaskSpecContainerSpecDNSConfig
genTaskSpecContainerSpecDNSConfig n =
  TaskSpecContainerSpecDNSConfig
    <$> arbitraryReducedMaybe n -- taskSpecContainerSpecDNSConfigNameservers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecDNSConfigSearch :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecDNSConfigOptions :: Maybe [Text]
  
instance Arbitrary TaskSpecContainerSpecFile where
  arbitrary = sized genTaskSpecContainerSpecFile

genTaskSpecContainerSpecFile :: Int -> Gen TaskSpecContainerSpecFile
genTaskSpecContainerSpecFile n =
  TaskSpecContainerSpecFile
    <$> arbitraryReducedMaybe n -- taskSpecContainerSpecFileName :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecFileUid :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecFileGid :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecFileMode :: Maybe Int
  
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
  
instance Arbitrary TaskSpecContainerSpecSecrets where
  arbitrary = sized genTaskSpecContainerSpecSecrets

genTaskSpecContainerSpecSecrets :: Int -> Gen TaskSpecContainerSpecSecrets
genTaskSpecContainerSpecSecrets n =
  TaskSpecContainerSpecSecrets
    <$> arbitraryReducedMaybe n -- taskSpecContainerSpecSecretsFile :: Maybe TaskSpecContainerSpecFile
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecSecretsSecretId :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecContainerSpecSecretsSecretName :: Maybe Text
  
instance Arbitrary TaskSpecLogDriver where
  arbitrary = sized genTaskSpecLogDriver

genTaskSpecLogDriver :: Int -> Gen TaskSpecLogDriver
genTaskSpecLogDriver n =
  TaskSpecLogDriver
    <$> arbitraryReducedMaybe n -- taskSpecLogDriverName :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecLogDriverOptions :: Maybe (Map.Map String Text)
  
instance Arbitrary TaskSpecNetworks where
  arbitrary = sized genTaskSpecNetworks

genTaskSpecNetworks :: Int -> Gen TaskSpecNetworks
genTaskSpecNetworks n =
  TaskSpecNetworks
    <$> arbitraryReducedMaybe n -- taskSpecNetworksTarget :: Maybe Text
    <*> arbitraryReducedMaybe n -- taskSpecNetworksAliases :: Maybe [Text]
  
instance Arbitrary TaskSpecPlacement where
  arbitrary = sized genTaskSpecPlacement

genTaskSpecPlacement :: Int -> Gen TaskSpecPlacement
genTaskSpecPlacement n =
  TaskSpecPlacement
    <$> arbitraryReducedMaybe n -- taskSpecPlacementConstraints :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- taskSpecPlacementPreferences :: Maybe [TaskSpecPlacementPreferences]
    <*> arbitraryReducedMaybe n -- taskSpecPlacementPlatforms :: Maybe [NodeDescriptionPlatform]
  
instance Arbitrary TaskSpecPlacementPreferences where
  arbitrary = sized genTaskSpecPlacementPreferences

genTaskSpecPlacementPreferences :: Int -> Gen TaskSpecPlacementPreferences
genTaskSpecPlacementPreferences n =
  TaskSpecPlacementPreferences
    <$> arbitraryReducedMaybe n -- taskSpecPlacementPreferencesSpread :: Maybe TaskSpecPlacementSpread
  
instance Arbitrary TaskSpecPlacementSpread where
  arbitrary = sized genTaskSpecPlacementSpread

genTaskSpecPlacementSpread :: Int -> Gen TaskSpecPlacementSpread
genTaskSpecPlacementSpread n =
  TaskSpecPlacementSpread
    <$> arbitraryReducedMaybe n -- taskSpecPlacementSpreadSpreadDescriptor :: Maybe Text
  
instance Arbitrary TaskSpecResources where
  arbitrary = sized genTaskSpecResources

genTaskSpecResources :: Int -> Gen TaskSpecResources
genTaskSpecResources n =
  TaskSpecResources
    <$> arbitraryReducedMaybe n -- taskSpecResourcesLimits :: Maybe TaskSpecResourcesLimits
    <*> arbitraryReducedMaybe n -- taskSpecResourcesReservation :: Maybe TaskSpecResourcesReservation
  
instance Arbitrary TaskSpecResourcesLimits where
  arbitrary = sized genTaskSpecResourcesLimits

genTaskSpecResourcesLimits :: Int -> Gen TaskSpecResourcesLimits
genTaskSpecResourcesLimits n =
  TaskSpecResourcesLimits
    <$> arbitraryReducedMaybe n -- taskSpecResourcesLimitsNanoCpUs :: Maybe Integer
    <*> arbitraryReducedMaybe n -- taskSpecResourcesLimitsMemoryBytes :: Maybe Integer
  
instance Arbitrary TaskSpecResourcesReservation where
  arbitrary = sized genTaskSpecResourcesReservation

genTaskSpecResourcesReservation :: Int -> Gen TaskSpecResourcesReservation
genTaskSpecResourcesReservation n =
  TaskSpecResourcesReservation
    <$> arbitraryReducedMaybe n -- taskSpecResourcesReservationNanoCpUs :: Maybe Integer
    <*> arbitraryReducedMaybe n -- taskSpecResourcesReservationMemoryBytes :: Maybe Integer
  
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
  
instance Arbitrary Volume where
  arbitrary = sized genVolume

genVolume :: Int -> Gen Volume
genVolume n =
  Volume
    <$> arbitrary -- volumeName :: Text
    <*> arbitrary -- volumeDriver :: Text
    <*> arbitrary -- volumeMountpoint :: Text
    <*> arbitraryReducedMaybe n -- volumeStatus :: Maybe (Map.Map String A.Value)
    <*> arbitrary -- volumeLabels :: (Map.Map String Text)
    <*> arbitrary -- volumeScope :: E'Scope
    <*> arbitrary -- volumeOptions :: (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- volumeUsageData :: Maybe VolumeUsageData
  
instance Arbitrary VolumeUsageData where
  arbitrary = sized genVolumeUsageData

genVolumeUsageData :: Int -> Gen VolumeUsageData
genVolumeUsageData n =
  VolumeUsageData
    <$> arbitrary -- volumeUsageDataSize :: Int
    <*> arbitrary -- volumeUsageDataRefCount :: Int
  



instance Arbitrary E'Availability where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Condition where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ContentType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'FailureAction where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'FailureAction2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Isolation where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Mode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Name where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Order where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Protocol where
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

instance Arbitrary TaskState where
  arbitrary = arbitraryBoundedEnum

