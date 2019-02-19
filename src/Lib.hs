{-# LANGUAGE OverloadedStrings #-}
module Lib( RobkoCmd(..), initRobko, cmdRobko, doSomething ) where

import qualified Data.ByteString.Char8 as B
import Text.Printf (printf)
import System.Hardware.Serialport
import Control.Concurrent (threadDelay)

-- Robot low-level commands
data RobkoCmd = GetInputStatus |
                GetJointSteps Int    -- which joint

-- let port = "COM3"          -- Windows
-- let port = "/dev/ttyUSB3"  -- Linux
port = "/dev/tty.ROBKO01-RNI-SPP"  -- MacOS

portSettings = defaultSerialSettings { commSpeed = CS19200, timeout = 50 }
cmdTime = 200000 -- 200ms for Robko 01 to 

initRobko :: SerialPort -> IO ()
initRobko s = do 
    -- Wait to init and read initialization
    threadDelay cmdTime
    helloRobko <- recv s 255

    let isReady = B.isPrefixOf "!!! Controller is ready !!!" helloRobko && B.isSuffixOf "Valentin Nikolov, val_niko@yahoo.com\r\n" helloRobko
    putStrLn $ if isReady then "Robko 01 is READY!" else "Robko 01 is NOT READY!"

cmdRobko' :: SerialPort -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
cmdRobko' s deviceId cmd arg0 arg1 arg2 arg3 = do    
    let cmd' = printf ":%02d%02d%d%d%04d%04d\r\n" deviceId cmd arg0 arg1 arg2 arg3

    send s $ B.pack cmd'
    flush s
    threadDelay cmdTime

    -- this will get command back
    recv s 255 >>= print

-- Initiate controller command
cmdRobko :: SerialPort -> Int -> RobkoCmd -> IO ()
cmdRobko s deviceId GetInputStatus        = cmdRobko' s deviceId 2 0 0 0 0
cmdRobko s deviceId (GetJointSteps joint) = cmdRobko' s deviceId 8 joint 0 0 0

doSomething = withSerial port portSettings $ \s -> do
    initRobko s

    cmdRobko s 1 $ GetJointSteps 3