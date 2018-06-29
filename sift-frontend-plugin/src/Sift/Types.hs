-- |

module Sift.Types where

import Data.ByteString (ByteString)
import Data.Set (Set)

-- | Some binding declared top-level in a module.
data Binding = Binding
  { bindingId      :: {-# UNPACK #-} !BindingId   -- ^ A unique ID for this binding.
  , bindingFlagged :: !(Set ByteString)  -- ^ This binding was flagged by a predicate.
  , bindingSrcSpan :: !(Maybe Span)  -- ^ Location for the binding.
  , bindingRefs    :: ![BindingId] -- ^ Bindings that I reference.
  } deriving (Show)

-- | Source span.
data Span = Span
  { spanFile :: {-# UNPACK #-}!ByteString
  , spanStartLine :: !Int
  , spanStartCol :: !Int
  , spanEndLine :: !Int
  , spanEndCol :: !Int
  } deriving (Show)

-- | ID for a binding declared in some package, in some module, with
-- some name.
data BindingId = BindingId
  { bindingIdPackage :: !ByteString
  , bindingIdModule :: !ByteString
  , bindingIdName :: !ByteString
  } deriving (Show, Ord, Eq)
