-- | This module defines data structures for QuickMod JSON files.
module Data.QuickMod where

import Prelude

import qualified Data.Text as T
import Data.String
import Data.Aeson
import Data.Aeson.Types
import Data.Default

-- This might change in the future.
type QMUrl = T.Text

-- | Adds the pair to the given list if its value isn't Null.
(??) :: Pair -> [Pair] -> [Pair]
(??) (_, Null)  = id
(??) p = (p :) -- Look, a smiley face!

infixr 1 ??


-- {{{ Main object

-- | Data structure for a QuickMod file.
data QuickMod =
    QuickMod { qmUid            :: T.Text
             , qmName           :: T.Text
             , qmRepo           :: T.Text
             , qmReferences     :: [Reference]
             , qmUpdateUrl      :: QMUrl
             , qmVerifyUrl      :: Maybe QMUrl
             , qmModId          :: Maybe T.Text
             , qmDescription    :: Maybe T.Text
             , qmIcon           :: Maybe QMUrl
             , qmLogo           :: Maybe QMUrl
             , qmWebsite        :: Maybe QMUrl
             , qmIssuesUrl      :: Maybe QMUrl
             , qmDonationsUrl   :: Maybe QMUrl
             , qmTags           :: [T.Text]
             , qmCategories     :: [T.Text]
             , qmAuthors        :: [Author]
             , qmVersions       :: [Version]
             } deriving (Show)

-- {{{ Default
instance Default QuickMod where
    def = QuickMod { qmUid = ""
                   , qmName = ""
                   , qmRepo = ""
                   , qmReferences = []
                   , qmUpdateUrl = ""
                   , qmVerifyUrl = Nothing
                   , qmModId = Nothing
                   , qmDescription = Nothing
                   , qmIcon = Nothing
                   , qmLogo = Nothing
                   , qmWebsite = Nothing
                   , qmIssuesUrl = Nothing
                   , qmDonationsUrl = Nothing
                   , qmTags = []
                   , qmCategories = []
                   , qmAuthors = []
                   , qmVersions = []
                   }
-- }}}

instance ToJSON QuickMod where
    toJSON qm = object $
           "uid"            .= qmUid qm
        ?? "name"           .= qmName qm
        ?? "repo"           .= qmRepo qm
        ?? "references"     .= qmReferences qm
        ?? "updateUrl"      .= qmUpdateUrl qm
        ?? "verifyUrl"      .= qmVerifyUrl qm
        ?? "modId"          .= qmModId qm
        ?? "description"    .= qmDescription qm
        ?? "icon"           .= qmIcon qm
        ?? "logo"           .= qmLogo qm
        ?? "websiteUrl"     .= qmWebsite qm
        ?? "issuesUrl"      .= qmIssuesUrl qm
        ?? "donationsUrl"   .= qmDonationsUrl qm
        ?? "tags"           .= qmTags qm
        ?? "categories"     .= qmCategories qm
        ?? "authors"        .= qmAuthors qm
        ?? "versions"       .= qmVersions qm
        ?? []

-- }}}

-- {{{ Small sub-objects in the QuickMod

-- | Data structure representing a reference to another QuickMod.
data Reference = Reference T.Text QMUrl deriving (Show)

instance ToJSON [Reference] where
    toJSON = object . map refPair
      where
        refPair (Reference uid url) = uid .= url


-- | Data structure representing an entry in a mod's author list.
data Author = Author T.Text [T.Text] deriving (Show)

instance ToJSON [Author] where
    toJSON = object . map aPair
      where
        aPair (Author name roles) = name .= roles

-- }}}

-- {{{ Version object

-- | Data structure representing a QuickMod version.
data Version =
    Version { vsnName           :: T.Text
            , vsnType           :: Maybe T.Text
            , vsnMCCompat       :: [T.Text]
            , vsnForgeCompat    :: Maybe (Interval T.Text)
            , vsnReferences     :: [VsnReference]
            , vsnDlType         :: Maybe DownloadType
            , vsnInstallType    :: Maybe InstallType
            , vsnMd5            :: Maybe T.Text
            , vsnUrl            :: Maybe T.Text
            } deriving (Show)

-- {{{ Default
instance Default Version where
    def = Version { vsnName = ""
                  , vsnType = Nothing
                  , vsnMCCompat = []
                  , vsnForgeCompat = Nothing
                  , vsnReferences = []
                  , vsnDlType = Nothing
                  , vsnInstallType = Nothing
                  , vsnMd5 = Nothing
                  , vsnUrl = Nothing
                  }
-- }}}

instance ToJSON Version where
    toJSON vsn = object $
           "name"           .= vsnName vsn
        ?? "type"           .= vsnType vsn
        ?? "mcCompat"       .= vsnMCCompat vsn
        ?? "forgeCompat"    .= vsnForgeCompat vsn
        ?? "references"     .= vsnReferences vsn
        ?? "downloadType"   .= vsnDlType vsn
        ?? "installType"    .= vsnInstallType vsn
        ?? "md5"            .= vsnMd5 vsn
        ?? "url"            .= vsnUrl vsn
        ?? []

-- {{{ Download & install types

data DownloadType = DirectDL | ParallelDL | SequentialDL deriving (Show, Read)
instance ToJSON DownloadType where
    toJSON DirectDL     = String "direct"
    toJSON ParallelDL   = String "parallel"
    toJSON SequentialDL = String "sequential"

data InstallType = ForgeMod | ForgeCoreMod | ExtractMod | ConfigPack | GroupMod deriving (Show, Read)
instance ToJSON InstallType where
    toJSON ForgeMod     = String "forgeMod"
    toJSON ForgeCoreMod = String "forgeCoreMod"
    toJSON ExtractMod   = String "extract"
    toJSON ConfigPack   = String "configPack"
    toJSON GroupMod     = String "group"

-- }}}

-- | Data structure representing a version reference.
data VsnReference =
    VsnReference { vrefUid :: T.Text
                 , vrefType :: VsnRefType
                 , vrefVersion :: Maybe (Interval T.Text)
                 } deriving (Show)

instance ToJSON VsnReference where
    toJSON ref = object $
           "uid"            .= vrefUid ref
        ?? "mcCompat"       .= vrefType ref
        ?? "version"        .= vrefVersion ref
        ?? []


-- | Version reference type.
data VsnRefType = Depends | Recommends | Suggests | Conflicts | Provides deriving (Show, Read)

instance ToJSON VsnRefType where
    toJSON Depends      = String "depends"
    toJSON Suggests     = String "suggests"
    toJSON Conflicts    = String "conflicts"
    toJSON Provides     = String "provides"


-- | Data structure representing an interval. It is always inclusive, meaning it includes the min and max value.
data Interval a = Interval a a deriving (Show)

showInterval :: Interval T.Text -> T.Text
showInterval (Interval start end) = "[" `T.append` start `T.append` "," `T.append` end `T.append` "]"

readInterval :: T.Text -> Interval T.Text
readInterval str' = Interval start end
  where
    str = T.tail $ T.init $ str'
    (start, end) = T.breakOn "," str

instance ToJSON (Interval T.Text) where
    toJSON = String . showInterval

-- }}}

