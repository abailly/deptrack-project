{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Devops.DnsResolver (
    DnsResolver, DnsService, dnsResolver , listeningDnsResolverDaemon
  , googlePublicDns
  , RunDir
  ) where

import           Data.Monoid             ((<>))
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           System.FilePath.Posix   ((</>))
import           Text.Printf             (printf)

import           Devops.Debian.Commands
import           Devops.Debian.User
import           Devops.Networking
import           Devops.Service
import           Devops.Storage
import           Devops.Base

type RunDir = FilePath

showt :: Show a => a -> Text
showt = Text.pack . show

data DnsService = DnsService

data DnsResolver
type instance DaemonConfig DnsResolver = (User, FilePresent, [FilePresent])

type DnsForwarder = Remoted (Listening DnsService)

data Bind9Configuration = Bind9Configuration { bind9Forwarder :: !DnsForwarder
                                             , bind9Zones     :: ![Zone]
                                             }

data ZoneType = Hint | Master
data Zone = Zone HostString !ZoneType ![ZoneRecord]
zoneRecords :: Zone -> [ZoneRecord]
zoneRecords (Zone _ _ rs) = rs

type RecordData = Text
data RecordType = A | AAAA | NS | SOA | PTR | CNAME | MX | SRV
  deriving Show
data RecordClass = IN
  deriving Show
data ZoneRecord = ZoneRecord !HostString !RecordType !TTL !(Maybe RecordClass) !RecordData
record :: HostString -> RecordType -> TTL -> Maybe RecordClass -> RecordData -> ZoneRecord
record = ZoneRecord

zoneHostString :: Zone -> FilePath
zoneHostString (Zone h _ _) = convertString h

zoneFilePath :: RunDir -> Zone -> FilePath
zoneFilePath rundir (Zone "." _ _) = rundir </> "bind9" </> "db.root"
zoneFilePath rundir (Zone h _ _)   = rundir </> "bind9" </> ("db." <> convertString h)

zoneTypeString :: Zone -> Text
zoneTypeString (Zone _ t _) = go t
  where go Hint   = "hint"
        go Master = "master"

dnsResolverConfiguration :: RunDir -> AddressPlan -> Bind9Configuration -> Text
dnsResolverConfiguration rundir plan conf =
  let zones = bind9Zones conf in
  let (Remoted (Remote forwarder) _) = bind9Forwarder conf in
  Text.unlines [
    "// Fixed, autogenerated configuration."
  -- option fragment
  , "options {"
  , "  directory \"/var/cache/bind\";"
  , "  forwarders { "<> forwarder <>"; };"
  , "  dnssec-validation auto;"
  , "  auth-nxdomain no;"
  , "  listen-on { "<> localNetwork plan <>"; };"
  , "};"
  -- zone includes
  , Text.unlines (map (zoneIncludeConfiguration rundir) zones)
  ]

zoneIncludeConfiguration :: RunDir -> Zone -> Text
zoneIncludeConfiguration rundir z = Text.unlines [
    Text.pack $ printf "zone \"%s\" {"  $ zoneHostString z
  , Text.pack $ printf "  type %s;"     $ Text.unpack $ zoneTypeString z
  , Text.pack $ printf "  file \"%s\";" $ zoneFilePath rundir z
  , "};"
  ]

zoneDataConfiguration :: Zone -> Text
zoneDataConfiguration z = Text.unlines $ fmap recordLineConfiguration (zoneRecords z)
  where recordLineConfiguration (ZoneRecord h t ttl klass dat) = Text.unwords [
               h , showt ttl , maybe "" showt klass , showt t , dat ]

-- | Computes a Bind9 given an AddressPlan and a DnsForwarder.
-- TODO: parametrize the zone-name rather than overloading localhost
staticConfig :: AddressPlan -> DnsForwarder -> Bind9Configuration
staticConfig plan fwd =
    Bind9Configuration fwd zones
  where
    zones = [ rootZone , localhostZone plan, rev127Zone
            , rev0Zone , rev255Zone , revLocalZone plan
            ]

rootZone :: Zone
rootZone = Zone "." Hint [
    record "." NS 360000 (Just IN) "A.ROOT-SERVERS.NET."
  , record "A.ROOT-SERVERS.NET." A 360000 Nothing "198.41.0.4"
  , record "." NS 360000 (Just IN) "B.ROOT-SERVERS.NET."
  , record "B.ROOT-SERVERS.NET." A 360000 Nothing "192.228.79.201"
  , record "." NS 360000 (Just IN) "C.ROOT-SERVERS.NET."
  , record "C.ROOT-SERVERS.NET." A 360000 Nothing "192.33.4.12"
  , record "." NS 360000 (Just IN) "D.ROOT-SERVERS.NET."
  , record "D.ROOT-SERVERS.NET." A 360000 Nothing "199.7.91.13"
  , record "." NS 360000 (Just IN) "E.ROOT-SERVERS.NET."
  , record "E.ROOT-SERVERS.NET." A 360000 Nothing "192.203.230.10"
  , record "." NS 360000 (Just IN) "F.ROOT-SERVERS.NET."
  , record "F.ROOT-SERVERS.NET." A 360000 Nothing "192.5.5.241"
  , record "." NS 360000 (Just IN) "G.ROOT-SERVERS.NET."
  , record "G.ROOT-SERVERS.NET." A 360000 Nothing "192.112.36.4"
  , record "." NS 360000 (Just IN) "H.ROOT-SERVERS.NET."
  , record "H.ROOT-SERVERS.NET." A 360000 Nothing "128.63.2.53"
  , record "." NS 360000 (Just IN) "I.ROOT-SERVERS.NET."
  , record "I.ROOT-SERVERS.NET." A 360000 Nothing "192.36.148.17"
  , record "." NS 360000 (Just IN) "J.ROOT-SERVERS.NET."
  , record "J.ROOT-SERVERS.NET." A 360000 Nothing "192.58.128.30"
  , record "." NS 360000 (Just IN) "K.ROOT-SERVERS.NET."
  , record "K.ROOT-SERVERS.NET." A 360000 Nothing "193.0.14.129"
  , record "." NS 360000 (Just IN) "L.ROOT-SERVERS.NET."
  , record "L.ROOT-SERVERS.NET." A 360000 Nothing "199.7.83.42"
  , record "." NS 360000 (Just IN) "M.ROOT-SERVERS.NET."
  , record "M.ROOT-SERVERS.NET." A 360000 Nothing "202.12.27.33"
  ]

localhostZone :: AddressPlan -> Zone
localhostZone plan =
    let idxs = indices plan in
    Zone "localhost" Master ([
        record "@" SOA 360000 (Just IN) "localhost. root.localhost. (2 604800 86400 2419200 604800)"
      , record "@" NS 360000 (Just IN) "localhost."
      , record "@" A 360000 (Just IN) "127.0.0.1"
      ] <> [record (fixedHostName plan idx) A 300 (Just IN) (convertString $ fixedIp plan idx) | idx <- idxs]
        <> [record (fixed0xHostName plan idx) A 300 (Just IN) (convertString $ fixedIp plan idx) | idx <- idxs])

rev127Zone :: Zone
rev127Zone = Zone "127.in-addr.arpa" Master [
    record "@" SOA 360000 (Just IN) "localhost. root.localhost. (1 604800 86400 2419200 604800)"
  , record "@" NS 360000 (Just IN) "localhost."
  , record "1.0.0" PTR 360000 (Just IN) "localhost."
  ]
rev0Zone :: Zone
rev0Zone = Zone  "0.in-addr.arpa" Master [
    record "@" SOA 360000 (Just IN) "localhost. root.localhost. (1 604800 86400 2419200 604800)"
  , record "@" NS 360000 (Just IN) "localhost."
  ]
rev255Zone :: Zone
rev255Zone = Zone  "255.in-addr.arpa" Master [
    record "@" SOA 360000 (Just IN) "localhost. root.localhost. (1 604800 86400 2419200 604800)"
  , record "@" NS 360000 (Just IN) "localhost."
  ]
revLocalZone :: AddressPlan -> Zone
revLocalZone plan =
    Zone zoneName Master ([
      record "@" SOA 360000 (Just IN) "localhost. root.localhost. (1 604800 86400 2419200 604800)"
    , record "@" NS 360000 (Just IN) "localhost."
    ] <> ptrRecords)
  where
    firstNetChar :: Text
    firstNetChar = Text.takeWhile ((/=)'.') (localNetwork plan)

    zoneName :: Name
    zoneName = firstNetChar <> ".in-addr.arpa"

    ptrRecords :: [ZoneRecord]
    ptrRecords = fmap mkPtrRecord (indices plan)

    mkPtrRecord :: Index -> ZoneRecord
    mkPtrRecord idx =
       record (revfixedIpBackSlash24 idx) PTR 300 (Just IN)
              (fixedHostName plan idx <> ".localhost.")

    revfixedIpBackSlash24 :: Index -> Text
    revfixedIpBackSlash24 idx =
        let ip = fixedIp plan idx
        in Text.intercalate "." (reverse (take 3 (Text.splitOn "." ip)))

mkBind9ConfigFile :: RunDir
                  -> AddressPlan
                  -> DevOp DirectoryPresent
                  -> DevOp Bind9Configuration
                  -> DevOp FilePresent
mkBind9ConfigFile rundir plan mkRoot mkCfg = do
  (DirectoryPresent bind9dir) <- mkRoot
  let fp = fmap snd $ fileContent (bind9dir </> "bind9.conf") (convertString . dnsResolverConfiguration rundir plan <$> mkCfg)
  filePermissions bind9Owner fp

mkBind9ZoneFile :: RunDir -> DevOp DirectoryPresent -> DevOp Zone -> DevOp (FileContent, FilePresent)
mkBind9ZoneFile rundir mkRoot mkZone = do
  _ <- mkRoot
  zone <- mkZone
  fileContent (zoneFilePath rundir zone) (convertString . zoneDataConfiguration <$> mkZone)

mkBind9ZoneFiles :: RunDir -> DevOp DirectoryPresent -> DevOp [Zone] -> DevOp [FilePresent]
mkBind9ZoneFiles rundir mkRoot mkZones = do
  let mkOne zone = fmap snd $ mkBind9ZoneFile rundir mkRoot (pure zone)
  zones <- mkZones
  traverse (filePermissions bind9Owner . mkOne) zones

dnsResolverCommandArgs :: DaemonConfig DnsResolver -> CommandArgs
dnsResolverCommandArgs (User u,FilePresent path,_) = [
    "-4" -- ipv4 only
  , "-f" -- keep in foreground
  , "-n", "1" -- single thread
  , "-u", convertString u
  , "-c", path  -- configfile
  ]

bind9Group :: DevOp Group
bind9Group = group "dnsuser"

bind9User :: DevOp User
bind9User = user "dnsuser" bind9Group (return [])

bind9Owner :: DevOp Ownership
bind9Owner = (,) <$> bind9User <*> bind9Group

dnsResolver :: RunDir -> AddressPlan -> DevOp DnsForwarder -> DevOp (Daemon DnsResolver)
dnsResolver rundir plan mkForwarder = daemon "bind9" Nothing bind9 dnsResolverCommandArgs $ do
  let config = fmap (staticConfig plan) mkForwarder
  let bind9dir = directoryPermissions bind9Owner
                                      (subdirectory (directory rundir) "bind9")
  cfgFile <- mkBind9ConfigFile rundir plan bind9dir config
  dataFiles <- mkBind9ZoneFiles rundir bind9dir (fmap bind9Zones config)
  u <- bind9User
  return (u,cfgFile,dataFiles)

listeningDnsResolverDaemon :: Daemon DnsResolver -> Listening DnsService
listeningDnsResolverDaemon _ = Listening port53 DnsService

port53 :: Port DnsService
port53 = 53

-- | One of the Google PublicDNS resolver IP address.
googlePublicDns :: DevOp DnsForwarder
googlePublicDns = Remoted <$> existingRemote "8.8.4.4" <*> pure (Listening port53 DnsService)
