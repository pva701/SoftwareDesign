-- | Type corresponding to JSON for communication.

module Types
       ( Response
       , SearchEntry (..)
       ) where

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Universum

data SearchEntry = SearchEntry
    { url       :: !String
    , title     :: !String
    , searchSys :: !String
    } deriving (Generic)

type Response = [SearchEntry]

instance ToJSON SearchEntry

instance FromJSON SearchEntry
