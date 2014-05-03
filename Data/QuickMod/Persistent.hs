-- Stuff for using Data.QuickMod things with Persistent.
module Data.QuickMod.Persistent
    ( DownloadType
    , InstallType
    , VsnRefType
    , Interval
    ) where

import Prelude
import Data.Either
import Data.Text (Text)
import Data.QuickMod
import Database.Persist.TH
import Database.Persist.Class
import Database.Persist.Sql

derivePersistField "DownloadType"
derivePersistField "InstallType"
derivePersistField "VsnRefType"

instance PersistField (Interval Text) where
    toPersistValue = PersistText . showInterval
    fromPersistValue (PersistText v) = Right $ readInterval v

instance PersistFieldSql (Interval Text) where
    sqlType _ = SqlString

