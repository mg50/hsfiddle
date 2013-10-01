
module Control.Concurrent.SafeMVar where
import Control.Monad
import Control.Concurrent.MVar

newtype SafeMVar a = SafeMVar { toMVar :: MVar a}

newSafeMVar :: a -> IO (SafeMVar a)
newSafeMVar a = liftM SafeMVar (newMVar a)

modifySafeMVar :: SafeMVar a -> (a -> IO a) -> IO ()
modifySafeMVar (SafeMVar mv) f = do modifyMVar_ mv f
                                    return ()
