{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Monad             (forM, forM_, void)
import           Data.ByteString.Lazy      (toStrict)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack, unpack)
import           Data.Text.Encoding        (decodeUtf8)
import           Data.Text.IO              (hPutStrLn)
import           Devops.Base               (DevOp, buildOp, devop, noAction,
                                            noCheck)
import           Devops.Bootstrap.Build
import           Devops.Bootstrap.DO
import           Devops.Bootstrap.Parasite
import           Devops.Callback           (binaryCall, continueConst)
import           Devops.Cli                (App (..), Method (..), SelfPath,
                                            appMain, appMethod, methodArg)
import           Devops.Debian.Base        (deb)
import           Devops.Debian.Commands    (git)
import           Devops.Debian.User        (Group (..), User (..),
                                            directoryPermissions, homeDirPath,
                                            mereUser, preExistingUser,
                                            userDirectory)
import           Devops.Git                (GitRepo (..), gitClone)
import           Devops.Haskell            (installStack)
import           Devops.Optimize           (optimizeDebianPackages)
import           Devops.OS
import           Devops.Ref
import           Devops.Storage            (DirectoryPresent (..),
                                            FilePresent (..), checkFilePresent,
                                            dirLink, directory, fileLink, touch,
                                            (</>))
import           Devops.Storage.Base       (blindRemoveLink)
import           Network.DO                hiding (error)
import           Network.HTTP.Simple
import           System.IO                 (BufferMode (..), IOMode (..),
                                            hSetBuffering, stderr, stdout,
                                            withFile)


-- | Stages of execution of this application
data Stage = Local String Int
           -- ^ The binary was called (e.g., by a human) on the local-host.
           | Remote
           -- ^ The binary was called remotely

parseStage :: [String] -> (Stage, Method)
parseStage = \case
    ["_remote_",arg] -> (Remote, appMethod arg)
    [host,doKey,arg] -> (Local host (read doKey),  appMethod arg)
    args             -> error $ "unparsed args: " ++ show args

unparseStage :: Stage -> Method -> [String]
unparseStage stage m = case stage of
    Local h k -> [ h, show k, methodArg m ]
    Remote    -> [ "_remote_", methodArg m ]

--------------------------------------------------------------------

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    let app = App parseStage unparseStage stages [optimizeDebianPackages] :: App Stage
    appMain app

stages :: Stage -> SelfPath -> (Stage -> Method -> [String]) -> DevOp ()
stages Remote _ _                 = devtools
stages (Local hn userKey)  self fixCall = void $ do
    -- prepare callbacks for binary calls
    let remoteCallback = binaryCall self (fixCall Remote)

    -- the droplet
    let doDroplet = parasitedHost hn userKey

    -- the remotely configured droplet
    remoteContinued root
        (continueConst devtools remoteCallback)
        doDroplet

exe               = "deptrack-devops-example-devbox"
root              = preExistingUser "root"
devUser           = mereUser "curry"
allIps            = "0.0.0.0"
dropletConfig key = standardDroplet { size = G8, configImageSlug = ubuntuXenialSlug, keys = [key] }

parasitedHost :: String -> Int -> DevOp ParasitedHost
parasitedHost dropletName userkey = do
    let host     = droplet False ((dropletConfig userkey) { configName = dropletName } )
        built    = build ubuntu16_04 ".." exe
        doref    = saveRef (pack dropletName)
        resolved = resolveRef doref host
    parasite root (buildOutput built) resolved

devtools :: DevOp ()
devtools = void $ do
    packages
    installStack
    dotFiles devUser (dotFilesDir devUser)
    spacemacs devUser (spacemacsDir devUser)
    usersKeys [User "runeaune", User"abailly"] devUser
    -- stack install intero
    -- retrieve binary packges -> unpack


-- | Add github usesr keys to authorized_keys file
usersKeys :: [User] -> DevOp User -> DevOp ()
usersKeys users mkUsr = do
    User u <- mkUsr
    let sshDir = userDirectory ".ssh" mkUsr
        userAndGroup = (,Group u) <$> mkUsr
        dir = directoryPermissions userAndGroup sshDir
        authorizedKeys = touch dir "authorized_keys"
    forM_ users (copySshKeyFromGithub authorizedKeys)


copySshKeyFromGithub :: DevOp FilePresent -> User -> DevOp ()
copySshKeyFromGithub mkPath (User u) = devop (const ()) mkop $ do
    FilePresent fp <- mkPath
    return (fp,u)
    where
      getKey :: Text -> IO Text
      getKey u = do
          request <- parseRequest $ unpack $ "GET " <> "https://github.com/" <> u <> ".keys"
          decodeUtf8 . toStrict . getResponseBody <$> httpLBS request

      mkop (fp, u) =
          buildOp ("github-key: " <> u)
          ("copy github key " <> u <> " to " <> pack fp)
          noCheck
          (do
                  k <- getKey u
                  withFile fp AppendMode $ \ h ->
                      hPutStrLn h k
          )
          (blindRemoveLink fp)
          noAction


packages :: DevOp ()
packages = void $ do
    deb "git-core"
    deb "libtinfo-dev"
    deb "emacs"
    deb "tmux"
    deb "graphviz"
    deb "wget"

dotFilesDir :: DevOp User -> DevOp DirectoryPresent
dotFilesDir mkUsr = do
    User u <- mkUsr
    let dotfilesDir = userDirectory "dotfiles" mkUsr
        userAndGroup = (,Group u) <$> mkUsr
    directoryPermissions userAndGroup dotfilesDir

spacemacsDir :: DevOp User -> DevOp DirectoryPresent
spacemacsDir mkUsr = do
    User u <- mkUsr
    let spacemacsdir
            = userDirectory "spacemacs" mkUsr
        userAndGroup = (,Group u) <$> mkUsr
    directoryPermissions userAndGroup spacemacsdir

spacemacs :: DevOp User -> DevOp DirectoryPresent -> DevOp DirectoryPresent
spacemacs mkUsr sedir = do
   User u <- mkUsr
   let emacsdir = homeDirPath u </> ".emacs.d"
       repo = gitClone "https://github.com/abailly/spacemacs" "master" git sedir
   symlinkEmacsDir emacsdir repo


symlinkEmacsDir :: FilePath -> DevOp GitRepo -> DevOp DirectoryPresent
symlinkEmacsDir emacsdir mkRepo = do
    GitRepo dir _ _ <- mkRepo
    fst <$> dirLink emacsdir (directory $ getDirectoryPresentPath dir)

dotFiles :: DevOp User -> DevOp DirectoryPresent -> DevOp [FilePresent]
dotFiles mkUsr dotFiles = do
    let repo = gitClone "https://github.com/abailly/dotfiles" "master" git dotFiles
    forM [ ".tmux.conf"
         , ".gitconfig"
         , ".bash_profile"
         ] $ symlinkFile mkUsr repo

symlinkFile :: DevOp User -> DevOp GitRepo -> FilePath -> DevOp FilePresent
symlinkFile mkUser mkRepo filepath = do
    User u          <- mkUser
    let home = homeDirPath u
    fst <$> fileLink (home </> filepath) (preExistingFileIn mkRepo filepath)

preExistingFileIn :: DevOp GitRepo -> FilePath -> DevOp FilePresent
preExistingFileIn mkRepo fp = do
    GitRepo dir _ _ <- mkRepo
    return $ FilePresent (getDirectoryPresentPath dir </> fp)
