module Main where

import           Control.Concurrent      (threadDelay)
import           Control.Monad.IO.Class
import           System.Hardware.Robko01

main :: IO ()
main = runRobko port 1 $ do
    start MotorBase Down FullStep 10
    liftIO $ threadDelay 1000000
    resetAndStop

    x <- getJointSteps MotorBase
    -- x <- getInputStatus
    liftIO $ print x
