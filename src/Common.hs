module Common (MaybeIO) where
import Control.Monad.Trans.Maybe (MaybeT)

type MaybeIO = MaybeT IO