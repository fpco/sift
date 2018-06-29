-- |

module Sift.Types where

import           Data.Set (Set)
import qualified GHC

-- | Some binding declared top-level in a module.
data Binding = Binding
  { bindingId      :: BindingId   -- ^ A unique ID for this binding.
  , bindingFlagged :: Set String  -- ^ This binding was flagged by a predicate.
  , bindingSrcSpan :: GHC.SrcSpan -- ^ Location for the binding.
  , bindingRefs    :: [BindingId] -- ^ Bindings that I reference.
  } deriving (Show)

-- | Source span.
data Span = Span
  { spanStartLine :: Int
  , spanStartCol :: Int
  , spanEndLine :: Int
  , spanEndCol :: Int
  , spanFilePath :: FilePath
  } deriving (Show)

-- | ID for a binding declared in some package, in some module, with
-- some name.
data BindingId = BindingId
  { bindingIdPackage :: String
  , bindingIdModule :: String
  , bindingIdName :: String
  } deriving (Show, Ord, Eq)
