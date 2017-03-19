{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Devops.Docker (
    DockerImage
  , dockerImage
  , preExistingDockerImage
  , Container (..)
  , ContainerCommand (..)
  , container
  , Dockerized (..)
  , dockerized
  , committedImage
  , fetchFile
  ) where

import           Control.Distributed.Closure (Closure)
import           Control.Distributed.Closure (unclosure)
import           Data.Monoid ((<>))
import           Data.String.Conversions (convertString)
import qualified Data.Text               as Text
import           DepTrack (declare)
import           System.FilePath.Posix   ((</>))

import           Devops.Base
import           Devops.BaseImage
import           Devops.Callback
import qualified Devops.Debian.Commands as Cmd
import           Devops.DockerBootstrap
import           Devops.Utils
import           Devops.Storage

data DockerImage = DockerImage !Name

-- | A named, pre-existing docker image.
preExistingDockerImage :: Name -> DevOp DockerImage
preExistingDockerImage name =
    declare op (return $ DockerImage name)
  where
    op = buildPreOp ("docker-image: " <> name)
                    ("pre-existing Docker image" <> name)
                    noCheck
                    noAction
                    noAction
                    noAction

-- | A named Docker image from a simple base.
--
-- TODO: consider relaxing BaseImage's FilePresent to a URL (e.g., using a
-- TypeFamily to specialize the type). Indeed, we can download a BaseImage from
-- a repository using Docker.
dockerImage :: Name -> DevOp (BaseImage DockerBase) -> DevOp (DockerImage)
dockerImage name mkBase = devop fst mkOp $ do
    base <- mkBase
    docker <- Cmd.docker
    return (DockerImage name, (docker, base))
  where
    mkOp (_, (docker, base)) =
        let path = getFilePresentPath (imagePath base) in
        let hasName n dat = n `elem` lines dat in
        buildOp ("docker-image: " <> name)
                ("imports " <> convertString path <> " as " <> name)
                (checkBinaryExitCodeAndStdout (hasName $ convertString name)
                     docker [ "images"
                            , "--format", "{{title .Repository}}"
                            ]
                            "")
                (blindRun docker ["import", path, convertString name] "")
                (blindRun docker ["rmi", convertString name] "")
                noAction

-- | Docker containers are built from an image and run a given command.
data Container =
    Container { containerName    :: !Name
              , containerCidPath :: !FilePath
              , containerImage   :: !DockerImage
              , containerCommand :: !ContainerCommand
              }

data ContainerCommand =
    ImportedContainerCommand !FilePresent [String]
  | ExistingContainerCommand !FilePath [String]

-- | Spawns a container for running a command.
-- The command is immediately started and the container is disposed only at
-- turndown.
--
-- It's cid is stored in a file in /var/run/devops.
container :: Name
          -> DevOp DockerImage
          -> DevOp ContainerCommand
          -> DevOp Container
container name mkImage mkCmd = devop fst mkOp $ do
    DirectoryPresent dirPath <- directory "/var/run/devops"
    let cidFile = dirPath </> Text.unpack name <> ".le-cid"
    image@(DockerImage imageName) <- mkImage
    cmd <- mkCmd
    docker <- Cmd.docker
    return $ (Container name cidFile image cmd, (docker, imageName))
  where
    mkOp (Container _ cidFile _ cmd, (docker, imageName)) =
        let (potentialCopy, callbackPath, args) = case cmd of
                (ExistingContainerCommand fp argv) -> (return (), fp, argv)
                (ImportedContainerCommand (FilePresent srcBin) argv) ->
                    let cb = "/devops-callback"
                        action = do
                            putStrLn "*******copying into docker*********"
                            blindRun docker [ "cp" , srcBin
                                            , convertString name <> ":" <> cb ] ""
                    in  (action, cb, argv)
        in
        buildOp ("docker-container: " <> name)
                ("creates " <> name <> " from image " <> imageName <> " with args: " <> convertString (show callbackPath) <> " " <> convertString (show args))
                (checkFilePresent cidFile)
                (blindRun docker ([ "create"
                                 , "--cidfile" , cidFile
                                 , "--name" , convertString name
                                 , convertString imageName
                                 , callbackPath
                                 ] ++ args )""
                 >> potentialCopy
                 >> blindRun docker [ "start" , convertString name ] ""
                 >> blindRun docker [ "wait" , convertString name ] "")
                (blindRemoveLink cidFile
                 >> blindRun docker [ "rm" , convertString name ] "")
                noAction

data Dockerized a = Dockerized !a !Container

-- | Continues setting up a DevOp from within a docker container.
dockerized :: Name
           -> ClosureCallBack a
           -> DevOp DockerImage
           -> Closure (DevOp a) 
           -> DevOp (Dockerized a)
dockerized name mkCb mkImage clo = declare op $ do
    let obj = runDevOp $ unclosure clo
    (BinaryCall selfPath args) <- mkCb clo
    let selfBin = preExistingFile selfPath
    let mkCmd = ImportedContainerCommand <$> selfBin <*> pure args
    let cbContainer = container name mkImage mkCmd
    Dockerized obj <$> cbContainer
  where
    op = buildPreOp ("dockerized-node: " <> name)
                    ("dockerize some node in the Docker image:" <> name)
                    noCheck
                    noAction
                    noAction
                    noAction

-- | Commit an image from a container.
committedImage :: DevOp (Dockerized a) -> DevOp DockerImage
committedImage mkDockerized = devop fst mkOp $ do
    (Dockerized _ cntner) <- mkDockerized
    let (DockerImage baseImageName) = containerImage cntner
    let name = baseImageName <> "--" <> containerName cntner
    docker <- Cmd.docker
    return $ (DockerImage name, (docker, cntner))
  where
    mkOp (DockerImage name, (docker, cntner)) =
        let hasName n dat = n `elem` lines dat in
        buildOp ("docker-image: " <> name)
                ("creates " <> name <> " based on container " <> containerName cntner)
                (checkBinaryExitCodeAndStdout (hasName $ convertString name)
                     docker [ "images"
                            , "--format", "{{title .Repository}}"
                            ]
                            "")
                (blindRun docker ["commit", convertString $ containerName cntner, convertString name] "")
                (blindRun docker ["rmi", convertString name] "")
                noAction

-- | Fetches a file from a container.
fetchFile :: FilePath -> DevOp (Dockerized FilePresent) -> DevOp FilePresent
fetchFile path mkFp = fmap f (generatedFile path Cmd.docker mkArgs)
  where
    f (_,_,filepresent) = filepresent
    mkArgs = do
        (Dockerized (FilePresent containerizedPath) cntnr) <- mkFp
        return [ "container"
               , "cp"
               , (convertString $ containerName cntnr) <> ":" <> containerizedPath
               , path
               ]
