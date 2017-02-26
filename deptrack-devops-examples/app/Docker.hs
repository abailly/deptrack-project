{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (void)
import           System.Environment (getArgs)

import           Devops.Base (DevOp)
import           Devops.BaseImage
import           Devops.Callback
import           Devops.Cli (defaultMain)
import           Devops.Debian (deb)
import           Devops.Docker
import           Devops.DockerBootstrap
import           Devops.Optimize (optimizeDebianPackages)

main :: IO ()
main = do
    args <- getArgs
    if isMagicArgv args then chrootNestedSetup else dockerSetup args
  where
    chrootNestedSetup :: IO ()
    chrootNestedSetup = void $ do
        defaultMain imageContent [optimizeDebianPackages] ["up"]

    dockerSetup :: [String] -> IO ()
    dockerSetup args = do
        self <- readSelf
        defaultMain (dock self) [optimizeDebianPackages] args

type Self = FilePath

readSelf :: IO Self
readSelf = takeWhile (/= '\NUL') <$> readFile "/proc/self/cmdline"

magicArgvTail = ["*", "*", "*"]
magicArgv = magicArgvTail
isMagicArgv args = args == magicArgvTail

tempdir = "/opt/dockbootstrap"
bootstrapBin = "/sbin/bootstrap-deptrack-devops"

dock :: Self -> DevOp ()
dock self = void $ dockerImage "deptrack-example" (simpleBootstrap tempdir baseImageConfig callback)
  where
    callback :: CallBackMethod
    callback = BinaryCall self magicArgv

    baseImageConfig :: BaseImageConfig DockerBase
    baseImageConfig =
        BaseImageConfig bootstrapBin xenial

imageContent :: DevOp ()
imageContent = void $ do
    deb "git-core"
