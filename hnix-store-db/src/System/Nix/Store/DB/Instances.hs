{-# OPTIONS_GHC -fno-warn-orphans       #-}

module System.Nix.Store.DB.Instances where

import           Database.Persist (PersistField(..), PersistValue(..), SqlType(..))
import Database.Persist.Sql (PersistFieldSql(..))
--import Database.Persist.Sqlite
--import           Database.Persist.Sql.Class (PersistFieldSQL)

import Data.Time (UTCTime)

import System.Nix.StorePath (StorePath, ContentAddressableAddress)
import System.Nix.StorePathMetadata (StorePathTrust(..))

import qualified Data.Text
import qualified System.Nix.StorePath
import qualified Data.Attoparsec.Text.Lazy


import qualified Data.Time.Clock.POSIX

instance PersistField StorePath where
  toPersistValue = PersistText . Data.Text.pack . show
  fromPersistValue (PersistText t) = either
    (Left . Data.Text.pack)
    Right
    $ Data.Attoparsec.Text.Lazy.parseOnly
      (System.Nix.StorePath.pathParser "/nix/store")
      t
  fromPersistValue wrongValue = Left
    $ Data.Text.pack
    $ "Received "
    ++ show wrongValue
    ++ " when a value of type PersistText was expected."

instance PersistFieldSql StorePath where
  sqlType _ = SqlString

-- XXX: handle errors
instance PersistField StorePathTrust where
  toPersistValue BuiltLocally     = PersistInt64 1
  toPersistValue BuiltElsewhere   = PersistNull
  fromPersistValue (PersistInt64 1) = pure BuiltLocally
  fromPersistValue _                = pure BuiltElsewhere

instance PersistFieldSql StorePathTrust where
  sqlType _ = SqlInt64

newtype NixUTCTime = NixUTCTime UTCTime
  deriving (Eq, Show, Ord)

instance PersistField NixUTCTime where
  toPersistValue (NixUTCTime u) = PersistInt64
    $ round $  Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds u
  fromPersistValue (PersistInt64 i) = pure $ NixUTCTime
    $ Data.Time.Clock.POSIX.posixSecondsToUTCTime $ fromIntegral i

instance PersistFieldSql NixUTCTime where
  sqlType _ = SqlInt64