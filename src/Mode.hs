module Mode ( Mode ( LogicSig, Application ) ) where

data Mode = LogicSig | Application
  deriving (Show, Eq, Ord)
