{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified Data.ByteString.Char8 as B
import Text.Printf (printf)
import System.Hardware.Serialport
import Control.Concurrent (threadDelay)

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

cmdRobko :: SerialPort -> Int -> Int -> Int -> Int -> Int -> Int -> IO B.ByteString
cmdRobko s deviceId cmd arg0 arg1 arg2 arg3 = do
    -- let cmd = printf ":%02d %02d %d %d %04d %04d \r\n" deviceId cmd arg0 arg1 arg2 arg3
    -- putStrLn cmd

    send s $ B.pack ":01020000000000\r\n"
    flush s
    threadDelay cmdTime

    -- this will get command back
    recv s 255 >>= print
    return ""

doSomething = withSerial port portSettings $ \s -> do
    initRobko s

    cmdRobko s 1 2 0 0 0 0