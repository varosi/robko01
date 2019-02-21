{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Lib( Joint(..), JointSteps(..), initRobko, doSomething, getJointSteps ) where

import Data.List.NonEmpty
import qualified Data.ByteString.Char8 as B
import Text.Printf                              (printf)
import System.Hardware.Serialport
import Control.Concurrent                       (threadDelay)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8         (char, digit)
import Control.Applicative

data Joint = Motor0 | Motor1 | Motor2 | Motor3 | Motor4 | Motor5 | Aux0 | Aux1 
    deriving (Show, Enum)

newtype JointSteps = JointSteps Int 
    deriving (Show)

parseBit :: Parser Bool
parseBit = do
    c <- char '0' <|> char '1'
    return (c == '1')

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

cmdRobko' :: SerialPort -> Int -> Int -> Int -> Int -> Int -> Int -> IO B.ByteString
cmdRobko' s deviceId cmd arg0 arg1 arg2 arg3 = do    
    let cmd' = printf ":%02d%02d%d%d%04d%04d\r\n" deviceId cmd arg0 arg1 arg2 arg3

    send s $ B.pack cmd'
    flush s
    threadDelay cmdTime
    recv s 255

-- Get status of inputs
getInputStatus :: SerialPort -> Int -> IO Int
getInputStatus s deviceId = do
    response <- cmdRobko' s deviceId 2 0 0 0 0 
    
    let result = parseOnly parseStatus response
        parseStatus = do
            string ":010200"
            (x :: Int) <- read <$> count 8 digit    
            return x

    either fail return result

-- Get status of joint
getJointSteps :: SerialPort -> Int -> Joint -> IO JointSteps
getJointSteps s deviceId joint = do
    response <- cmdRobko' s deviceId 8 (fromEnum joint) 0 0 0 
    print response
    
    let result = parseOnly (parseStatus (fromEnum joint)) response
        parseStatus joint = do
            string ":0108"
            string . B.pack . show $ joint
            neg        <- parseBit
            (x :: Int) <- read <$> count 8 digit    
            return . JointSteps $ if neg then -x else x

    either fail return result

doSomething = withSerial port portSettings $ \s -> do
    initRobko s

    x <- getJointSteps s 1 Motor2
    -- x <- getInputStatus s 1
    print x