module Exceptions (DuplicateData(..)) where

import Control.Exception
import Data.Typeable

data DuplicateData = DuplicateData
    deriving (Eq, Show, Typeable)

instance Exception DuplicateData
