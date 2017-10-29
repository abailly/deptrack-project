{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Provides bootstrapping nodes on Digital Ocean's cloud infrastructure
module Devops.Bootstrap.DO where

import           Data.Functor      (void)
import           Data.Maybe
import           Data.Monoid
import           Data.Text         (pack, unpack)
import           Devops.Base
import           Devops.Bootstrap
import           Devops.Networking
import           Devops.Ref
import           Network.DO

-- | A Digital Ocean node type
data DOcean = DOcean { doDropletConfig :: !BoxConfiguration }
        deriving (Show)

type instance NodeConfig DOcean = DOcean

deriving instance Show (Node DOcean)

instance HasResolver Remote (Node DOcean) where
  resolve k _ = runDOAuth False $ do
    -- assume k is the name of the droplet to resolve
    ls <- listDroplets emptyQuery
    case (mapMaybe publicIP . findByIdOrName (unpack k) <$> ls) of
        Right droplets -> case droplets of
                              (ip:_) -> return $ Remote (pack $ show ip)
                              []      -> fail $ "cannot resolve Remote for droplet with name " <> unpack k
        Left err      -> fail $ "cannot resolve Remote for droplet with name " <> unpack k <> ", got error: " <> show err


ubuntuXenialSlug :: ImageSlug
ubuntuXenialSlug =  "ubuntu-16-04-x64"

-- | A more-or-less standard droplet definition.
-- This `BoxConfiguration` has 1GB of RAM, is deployed on `Ams2` region and has *no* ssh keys.
-- it should probably be configured before being usable as a dependency.
standardDroplet :: BoxConfiguration
standardDroplet = BoxConfiguration "deptrack-default" (RegionSlug "ams2") G1 defaultImage [] False

-- | Describe a dependency on a DOcean droplet instance.
--
-- TODO: uniqueness of config?
droplet :: Bool -> BoxConfiguration -> DevOp (Node DOcean)
droplet debug conf@BoxConfiguration{..} = devop id mkOp (pure $ Node $ DOcean conf)
  where
    mkOp _ = buildOp
      ("Digital Ocean droplet: " <> pack configName)
      ("creates DO droplet with configuration: " <> pack (show conf))
      (checkDropletExists debug configName)
      (startDroplet debug conf)
      (killDroplet debug configName)
      noAction

-- * DO Utilities

startDroplet :: Bool -> BoxConfiguration -> OpAction
startDroplet debug conf = runDOAuth debug $ (createDroplet conf) >>= either (fail . show) (const $ return ())

withDroplet :: String -> (Maybe Droplet -> Command IO a) -> Command IO a
withDroplet dropletName f = do
    ls <- listDroplets emptyQuery
    case ls of
        Right droplets -> f $ listToMaybe $ findByIdOrName dropletName $ droplets
        Left err -> fail $ "cannot find droplet " <> dropletName <> ":  " <> show err
        
checkDropletExists :: Bool -> String -> OpCheck
checkDropletExists debug dname = runDOAuth debug $ withDroplet dname $
  maybe (return $ Failure $ "no droplet with name " <> dname) (const $ return Success)

killDroplet :: Bool -> String -> OpAction
killDroplet debug dname = void $ runDOAuth debug $
  withDroplet dname $ maybe (return Nothing) (\ dr -> do
                                                     res <- destroyDroplet (dropletId dr)
                                                     either (\ err -> fail $ "cannot destroy droplet "<> dname <> ":  "<> show err) (pure . Just) res
                                             )

runDOAuth :: Bool -> Command IO a -> IO a
runDOAuth True command = do
  auth <- getAuthFromEnv
  runDODebug command auth
runDOAuth False command = do
  auth <- getAuthFromEnv
  runDO command auth
