module ByteStringInstance where

import Control.DeepSeq
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

instance NFData B.ByteString where
    rnf a = rnf (BC.unpack a)

