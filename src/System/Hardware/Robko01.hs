{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module System.Hardware.Robko01( 
            Joint(..), JointSteps(..), 
            initRobko, doSomething, 
            resetAndStop, getJointSteps, getInputStatus ) where

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

data DriveDir  = Down | Up           deriving (Show, Enum)
data DriveMode = FullStep | HalfStep deriving (Show, Enum)

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

cmdStr :: Int -> Int -> Int -> Int -> Int -> Int -> B.ByteString
cmdStr deviceId cmd arg0 arg1 arg2 arg3 = B.pack $ printf ":%02d%02d%d%d%04d%04d\r\n" deviceId cmd arg0 arg1 arg2 arg3

cmdRobko' :: SerialPort -> Int -> Int -> Int -> Int -> Int -> Int -> IO (B.ByteString, B.ByteString)
cmdRobko' s deviceId cmd arg0 arg1 arg2 arg3 = do    
    let str = cmdStr deviceId cmd arg0 arg1 arg2 arg3 
    send s str
    flush s
    threadDelay cmdTime
    actual <- recv s 255
    return (actual, str)

-- | Reset and Stop moving
resetAndStop :: SerialPort -> Int -> IO ()
resetAndStop s deviceId = do
    (response, shouldBe) <- cmdRobko' s deviceId 0 0 0 0 0 
    
    let result = parseOnly parseStatus response
        parseStatus = string shouldBe

    either fail (\ _ -> return ()) result

-- | Start of movement. Stopping is done by resetAndStop
start :: SerialPort -> Int -> Joint -> DriveDir -> DriveMode -> Int -> IO ()
start s deviceId joint dir mode speed = do
    (response, shouldBe) <- cmdRobko' s deviceId 1 (fromEnum joint) (fromEnum dir) (1 + fromEnum mode) speed 

    let result = parseOnly parseStatus response
        parseStatus = string shouldBe

    either fail (\ _ -> return ()) result

-- | Get status of inputs
getInputStatus :: SerialPort -> Int -> IO Int
getInputStatus s deviceId = do
    (response, _) <- cmdRobko' s deviceId 2 0 0 0 0 
    
    let result = parseOnly parseStatus response
        parseStatus = do
            string ":010200"
            (x :: Int) <- read <$> count 8 digit    
            return x

    either fail return result

-- | Get status of joint
getJointSteps :: SerialPort -> Int -> Joint -> IO JointSteps
getJointSteps s deviceId joint = do
    (response,_) <- cmdRobko' s deviceId 8 (fromEnum joint) 0 0 0 
    
    let result = parseOnly (parseStatus (fromEnum joint)) response
        parseStatus joint = do
            string ":0108"
            string . B.pack . show $ joint
            neg        <- parseBit
            (x :: Int) <- read <$> count 8 digit    
            return . JointSteps $ if neg then -x else x

    either fail return result

-- | Test function
doSomething = withSerial port portSettings $ \s -> do
    initRobko s

    start s 1 Motor0 Down FullStep 10
    threadDelay 1000000
    resetAndStop s 1

    x <- getJointSteps s 1 Motor0
    -- x <- getInputStatus s 1
    print x