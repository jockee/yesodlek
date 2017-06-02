module Company (Company (Company), name, catchPhrase) where

import Import
import Database.SQLite.Simple.FromRow
import Data.Aeson

type Name = Text
data Company = Company { name :: Name, catchPhrase :: Text } deriving (Show)

instance FromRow Company where
  fromRow = Company <$> field <*> field

instance FromJSON Company where
  parseJSON = withObject "Company" $ \o -> do
    name <- o .: "name"
    catchPhrase <- o .: "catchPhrase"
    return Company{..}

instance ToJSON Company where
  toJSON Company{..} = object [
    "name" .= name,
    "catchPhrase" .= catchPhrase ]
