module Error where

import Control.Monad.State (StateT(..))

data Error = Error {
    posError :: (Int, Int),
    messg :: String
}
    deriving (Show)

orElse :: StateT s (Either a1) a2 -> StateT s (Either a3) a2 -> StateT s (Either a3) a2
orElse p q = StateT $ \s ->
  case runStateT p s of
    Left _  -> runStateT q s
    Right x -> Right x