module Database.Config (

) where 
  
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU

import System.Environment ( getEnv )

data SqlConfig = SqlConfig {
  hostname :: B.ByteString,
  database :: B.ByteString,
  username :: B.ByteString,
  password :: B.ByteString
}

loadSqlConfig :: IO SqlConfig
loadSqlConfig = 
  mapM getEnv ["DB_HOST", "DB_DATABASE_NAME", "DB_USERNAME", "DB_PASSWORD"] >>=
    \[a0, a1, a2, a3] -> pure $ SqlConfig {
      hostname = BU.fromString a0,
      database = BU.fromString a1,
      username = BU.fromString a2,
      password = BU.fromString a3
    }