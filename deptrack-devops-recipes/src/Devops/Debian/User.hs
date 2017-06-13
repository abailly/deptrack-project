{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO extract generic part to Devops.Unix ?
module Devops.Debian.User (
    Group (..) , group
  , User (..) , user , mereUser
  , Account (..), simpleAccount, account
  , preExistingGroup , preExistingUser
  , noExtraGroup
  , Ownership , filePermissions
  , directoryPermissions
  , userDirectory, homeDirPath
  ) where

import           Control.Applicative     (liftA2)
import           Control.Exception
import           Data.Monoid             ((<>))
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           DepTrack                (track)
import           System.FilePath.Posix   ((</>))
import           System.Posix.Files      (setOwnerAndGroup)
import           System.Posix.User       (getGroupEntryForName,
                                          getUserEntryForName, groupID, userID)

import           Devops.Debian.Commands
import           Devops.Storage
import           Devops.Base
import           Devops.Utils

type UserName = Text
type GroupName = Text
newtype User = User { userName :: UserName }
newtype Group = Group { groupName :: GroupName }

group :: GroupName -> DevOp Group
group n = devop snd mkop $ do
    gadd <- groupadd
    gdel <- groupdel
    return $ ((gadd, gdel), Group n)
  where
    mkop ((gadd,gdel), _) =
             buildOp ("debian-group: " <> convertString n)
                     ("ensures that " <> convertString n <> " is a group.")
                     (raisesNot $ getGroupEntryForName (Text.unpack n))
                     (blindRun gadd [convertString n] "")
                     (blindRun gdel [convertString n] "")
                     noAction

preExistingGroup :: GroupName -> DevOp Group
preExistingGroup n = pure $ Group n

preExistingUser :: UserName -> DevOp User
preExistingUser n = pure $ User n

noExtraGroup :: DevOp [Group]
noExtraGroup = pure []

raisesNot :: IO a -> IO CheckResult
raisesNot act = (act >> return Success) `catch` (\(e::IOException) -> return $ Failure (show e))

user :: UserName -> DevOp Group -> DevOp [Group] -> DevOp User
user n mkBaseGroup mkGroups = devop snd mkop $ do
  uadd <- useradd
  udel <- userdel
  basegrp <- groupName <$> mkBaseGroup
  grps <- map groupName <$> mkGroups
  return $ ((uadd, udel, basegrp, grps), User n)
  where
    mkop ((uadd, udel, basegrp, grps), _) =
             buildOp ("debian-user: " <> convertString n)
                     ("ensures that " <> convertString n <> " is a user.")
                     (raisesNot $ getUserEntryForName (Text.unpack n))
                     (blindRun uadd (uaddParams basegrp grps n) "")
                     (blindRun udel [convertString n] "")
                     noAction

data Account = Account { accountName :: UserName
                       , accountShell :: FilePath
                       , accountHome :: FilePath
                       }
               deriving (Show, Read)


-- | Utility function to create simple account using bash as shell
simpleAccount:: UserName -> DevOp User
simpleAccount uname =
    let acc = Account uname defaultShell (homeDirPath uname)
        defaultShell = "/bin/bash"
    in User . accountName <$> account acc (pure [])


-- | Create a loging account for given `UserName`
--
-- This is different from `user` which merely ensures the `User` exists. This function creates a
-- full login account, creating the user's home directory, setting its shell, creating a group...
account :: Account -> DevOp [Group] -> DevOp Account
account acc@Account{..} mkAdditionalGroups = devop snd mkop $ do
  uadd <- useradd
  udel <- userdel
  grps <- map groupName <$> mkAdditionalGroups
  return ((uadd, udel, grps), acc)
  where
    accountParams []   = [ "-m", "-c", "user-added-via-devops", "-U", "-s", accountShell, "-d", accountHome]
    accountParams grps = accountParams [] ++ ["-G" , convertString $ Text.intercalate "," grps]
    
    mkop ((uadd, udel, grps), _) =
             buildOp ("debian-account: " <> convertString accountName)
                     ("ensures that " <> convertString accountName <> " is a proper account.")
                     (raisesNot $ getUserEntryForName (Text.unpack accountName))
                     (blindRun uadd (accountParams grps) "")
                     (blindRun udel [convertString accountName] "")
                     noAction

uaddParams :: GroupName -> [GroupName] -> UserName -> [String]
uaddParams grp [] u = ["-M", "-c", "user-added-via-devops", "-g", convertString grp, convertString u]
uaddParams grp grps n = ["-M", "-G", convertString $ extraGroups grps,"-c", "user-added-via-devops", "-g", convertString grp, convertString n]
  where extraGroups gs = Text.intercalate "," gs

type Ownership = (User,Group)

filePermissions :: DevOp Ownership -> DevOp FilePresent -> DevOp FilePresent
filePermissions mkOwner mkFile = fmap fst $ track (uncurry mkFilePathPermissionOp) $ do
  (User usr, Group grp) <- mkOwner
  fp@(FilePresent path) <- mkFile
  return (fp, (path,usr,grp))

directoryPermissions :: DevOp Ownership -> DevOp DirectoryPresent -> DevOp DirectoryPresent
directoryPermissions mkOwner mkDir = fmap fst $ track (uncurry mkFilePathPermissionOp) $ do
  (User usr, Group grp) <- mkOwner
  dir@(DirectoryPresent path) <- mkDir
  return (dir, (path,usr,grp))

mkFilePathPermissionOp :: a -> (FilePath, UserName, GroupName) -> PreOp
mkFilePathPermissionOp  _ (path,usr,grp) =
  buildPreOp ("change-permissions: " <> Text.pack path)
     ("changes permissions on " <> Text.pack path <> " to " <> usr <> ":" <> grp)
     (noCheck)
     (chownFile usr grp path)
     (noAction)
     (noAction)

chownFile :: UserName -> GroupName -> FilePath -> IO ()
chownFile username groupname path = do
            usr <- getUserEntryForName (convertString username)
            grp <- getGroupEntryForName (convertString groupname)
            setOwnerAndGroup path (userID usr) (groupID grp)

mereUser :: Name -> DevOp User
mereUser name = user name (group name) noExtraGroup

userDirectory :: FilePath -> DevOp User -> DevOp DirectoryPresent
userDirectory subpath mkUser = do
  -- just extract the name of the user, fine to not track here because we
  -- re-inject the dep inside directoryPermissions
  (User name) <- mkUser
  let user_group = liftA2 (,) mkUser (group name)
  let homedir = directoryPermissions (user_group) (directory (homeDirPath name))
  directoryPermissions (user_group) (subdirectory homedir subpath)

homeDirPath :: Name -> FilePath
homeDirPath "root" = "/root"
homeDirPath name   = "/home" </> Text.unpack name
