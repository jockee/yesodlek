module Userx (Userx, uid, name, company) where

import Import
import Database.SQLite.Simple.FromRow
import Data.Aeson
import Company (Company(Company))

type Name = Text
data Userx = Userx { uid :: Integer, name :: Name, company :: Company} deriving (Show)

instance FromRow Userx where
  fromRow = Userx <$> field <*> field <*> liftM2 Company field field

instance FromJSON Userx where
  parseJSON = withObject "Userxs" $ \o -> do
    name <- o .: "name"
    company <- o .: "company"
    uid  <- o .: "id"
    return Userx{..}

instance ToJSON Userx where
  toJSON Userx{..} = object [
    "name" .= name,
    "company" .= company,
    "id"  .= uid  ]
