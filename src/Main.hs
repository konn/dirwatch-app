{-# LANGUAGE DataKinds, OverloadedStrings, PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell                        #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Main where
import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forM_)
import           Control.Monad             (forever)
import           Data.ByteString           (ByteString)
import           Data.FileEmbed            (embedFile)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time                 (UTCTime, defaultTimeLocale)
import           Data.Time                 (formatTime)
import           Language.ObjC.Inline      (objcCtxWithClasses)
import           Language.ObjC.Inline      (ObjC)
import           Language.ObjC.Inline      (defClass)
import qualified Language.ObjC.Inline      as C
import           System.Directory          (canonicalizePath)
import           System.Environment        (getArgs)
import           System.FilePath           (takeFileName)
import           System.FSNotify           (Event (..), watchTree, withManager)
import           System.Notification.Cocoa (Notification (..), newNotification)
import           System.Notification.Cocoa (schedule)
import           System.Notification.Cocoa (defaultNotificationSound)

type NSImage = ObjC "NSImage"
type NSWorkspace = ObjC "NSWorkspace"
C.context (objcCtxWithClasses [defClass "NSImage"
                              ,defClass "NSWorkspace"])
C.import_ "<AppKit/AppKit.h>"

getFileIcon :: FilePath -> IO ByteString
getFileIcon fp =
  let loc = T.pack fp
  in [C.exp'| NSData * { [[NSWorkspace sharedWorkspace]
                          iconForFile: $obj:(NSString *loc)].TIFFRepresentation} |]

main :: IO ()
main = withManager $ \man -> do
  ds <- getArgs
  print ds
  forM_ ds $ \d0 -> do
    d <- canonicalizePath d0
    watchTree man d (const True) notify
  forever $ threadDelay maxBound

notify :: Event -> IO ()
notify (Added fp date) = signal "File Created" fp date
notify (Modified fp date) = signal "File Modified" fp date
notify (Removed fp date) = signal "File Removed" fp date

signal :: Text -> String -> UTCTime -> IO ()
signal ttl fp date = do
  icn <- getFileIcon fp
  schedule newNotification
           { notifTitle = ttl
           , notifAppIcon  = Just icn
           , notifSubtitle = Just $ T.pack $ takeFileName fp
           , notifInformativeText = T.pack $ formatTime defaultTimeLocale "%c" date
           , notifSoundName = Just defaultNotificationSound
           }
