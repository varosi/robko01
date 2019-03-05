{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Hardware.Robko01(
    Robko01, Joint(..), JointSteps(..), DriveDir(..), DriveMode(..),
    runRobko, resetAndStop, getJointSteps, getInputStatus, start, port,
    cmdString ) where    -- Exported because of tests

import           Control.Applicative
import           Control.Concurrent               (threadDelay)
import           Control.Monad.IO.Class
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (char, digit)
import qualified Data.ByteString.Char8            as B
import           Control.Exception.Extra          (retry)
import           System.Hardware.Serialport
import           Text.Printf                      (printf)

data Joint = MotorBase | Motor1 | Motor2 | Motor3 | Motor4 | Motor5 | Aux0 | Aux1
    deriving (Show, Enum)

data DriveDir  = Down | Up           deriving (Show, Enum)
data DriveMode = FullStep | HalfStep deriving (Show, Enum)

newtype JointSteps = JointSteps Int
    deriving (Show)

-- | Robko01 monad for its commands
newtype Robko01 a = Robko01 { runRobko' :: SerialPort -> Int -> IO a }

-- We have do define our own instances for Functor, Applicative and Monad, because we cannot construct purely SerialPort.
instance Functor Robko01 where
    fmap f (Robko01 a) = Robko01 (\p deviceId -> do
        x <- a p deviceId
        pure $ f x)

instance Applicative Robko01 where
    pure a = Robko01 $ \_ _ -> pure a
    (Robko01 f) <*> (Robko01 a') = Robko01 (\p deviceId -> do
        g <- f p deviceId
        a <- a' p deviceId
        pure $ g a)

instance Monad Robko01 where
    return   = pure
    fail   s = Robko01 (\_ deviceId -> fail ("Robot #" ++ show deviceId ++ " failed with: " ++ s))
    (Robko01 x) >>= f = Robko01 $ \p deviceId -> do
        a <- x p deviceId
        let Robko01 second = f a
        second p deviceId

instance MonadIO Robko01 where
    liftIO io = Robko01 $ \_ _ -> io

-- | Run Robko01 program
runRobko :: String -> Int -> Robko01 a -> IO a
runRobko p deviceId commands =
    withSerial p portSettings $ \s -> do
        initRobko s
        runRobko' commands s deviceId

parseBit :: Parser Bool
parseBit = do
    c <- char '0' <|> char '1'
    return (c == '1')

-- let port = "COM3"          -- Windows
-- let port = "/dev/ttyUSB3"  -- Linux
port :: String
port = "/dev/tty.ROBKO01-RNI-SPP"  -- MacOS

portSettings :: SerialPortSettings
portSettings = defaultSerialSettings { commSpeed = CS19200, timeout = 50 }

cmdTime :: Int
cmdTime = 200000 -- 200ms for Robko 01 to

initRobko :: SerialPort -> IO ()
initRobko s = do
    helloRobko <- retry 5 $ do
        -- Wait to init and read initialization
        threadDelay cmdTime
        recv s 255

    let isReady = B.isPrefixOf "!!! Controller is ready !!!" helloRobko && B.isSuffixOf "Valentin Nikolov, val_niko@yahoo.com\r\n" helloRobko
    putStrLn $ if isReady then "Robko 01 is READY!" else "Robko 01 is NOT READY!"

cmdString :: Int -> Int -> Int -> Int -> Int -> Int -> B.ByteString
cmdString deviceId cmd arg0 arg1 arg2 arg3 = B.pack $ printf ":%02d%02d%d%d%04d%04d\r\n" deviceId cmd arg0 arg1 arg2 arg3

cmdRobko :: Int -> Int -> Int -> Int -> Int -> Robko01 (B.ByteString, B.ByteString)
cmdRobko cmd arg0 arg1 arg2 arg3 = Robko01 $ \s deviceId -> do
    let str = cmdString deviceId cmd arg0 arg1 arg2 arg3
    actual <- do
        send s str
        flush s
        threadDelay cmdTime
        recv s 255

    return (actual, str)

-- | Reset and Stop moving
resetAndStop :: Robko01 ()
resetAndStop = do
    (response, shouldBe) <- cmdRobko 0 0 0 0 0

    let result = parseOnly parseStatus response
        parseStatus = string shouldBe

    either fail (\ _ -> return ()) result

-- | Start of movement. Stopping is done by resetAndStop
start :: Joint -> DriveDir -> DriveMode -> Int -> Robko01 ()
start joint dir mode speed = do
    (response, shouldBe) <- cmdRobko 1 (fromEnum joint) (fromEnum dir) (1 + fromEnum mode) speed

    let result = parseOnly parseStatus response
        parseStatus = string shouldBe

    either fail (\ _ -> return ()) result

-- | Get status of inputs
getInputStatus :: Robko01 Int
getInputStatus = do
    (response, _) <- cmdRobko 2 0 0 0 0

    let result = parseOnly parseStatus response
        parseStatus = do
            string ":010200"
            (x :: Int) <- read <$> count 8 digit
            return x

    either fail return result

-- | Get status of joint
getJointSteps :: Joint -> Robko01 JointSteps
getJointSteps joint = do
    (response,_) <- cmdRobko 8 (fromEnum joint) 0 0 0

    let result = parseOnly (parseStatus (fromEnum joint)) response
        parseStatus j = do
            string ":0108"
            string . B.pack . show $ j
            neg        <- parseBit
            (x :: Int) <- read <$> count 8 digit
            return . JointSteps $ if neg then -x else x

    either fail return result
