module Main where

import           Control.Concurrent      (threadDelay)
import           Control.Monad.IO.Class
import           System.Hardware.Robko01

main :: IO ()
main = runRobko defaultPort 1 $ do

    -- Rotate of robot's base for one second
    start MotorBase Up FullStep 10
    liftIO $ threadDelay 1000000
    resetAndStop

    -- Print robot's base position
    x <- getJointSteps MotorBase
    liftIO $ print x
