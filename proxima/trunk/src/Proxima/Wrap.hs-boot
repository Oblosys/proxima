module Proxima.Wrap where

data Wrapped doc enr node clip token

instance (Show doc, Show enr, Show node, Show token) => Show (Wrapped doc enr node clip token)
