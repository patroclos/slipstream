module Common (MaybeIO, liftIODiscardExceptions) where
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Exception

type MaybeIO = MaybeT IO

liftIODiscardExceptions a io = liftIO $ (`catch` errToEmpty a) io
errToEmpty :: a -> SomeException -> IO a
errToEmpty a e = return a
