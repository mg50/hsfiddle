module Util where
import Data.Time

withDiffTime :: IO a -> IO (a, NominalDiffTime)
withDiffTime m = do startTime <- getCurrentTime
                    x <- m
                    endTime <- getCurrentTime
                    return (x, diffUTCTime endTime startTime)
