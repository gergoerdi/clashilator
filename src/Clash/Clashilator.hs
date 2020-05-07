{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}
module Clash.Clashilator (generateFiles) where

import Clash.Driver.Types

import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import Control.Monad (forM_)

import Text.Regex.Applicative

import System.FilePath
import System.Directory

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Development.Shake (writeFileChanged)

import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH
import Data.Aeson hiding (Options)
import qualified Data.HashMap.Strict as H

data Port = Port Text Int
    deriving Show

data FFIType
    = FFIBit
    | FFIU8
    | FFIU16
    | FFIU32
    | FFIU64

ffiType :: Int -> FFIType
ffiType 1 = FFIBit
ffiType n
  | n <= 8 = FFIU8
  | n <= 16 = FFIU16
  | n <= 32 = FFIU32
  | n <= 64 = FFIU64
  | otherwise = error $ unwords ["ffiType:", show n]

cType :: FFIType -> Text
cType FFIBit = "bit"
cType FFIU8 = "uint8_t"
cType FFIU16 = "uint16_t"
cType FFIU32 = "uint32_t"
cType FFIU64 = "uint64_t"

hsType :: FFIType -> Text
hsType FFIBit = "Bit"
hsType FFIU8 = "Word8"
hsType FFIU16 = "Word16"
hsType FFIU32 = "Word32"
hsType FFIU64 = "Word64"

cName :: Text -> Text
cName = id

hsName :: Text -> Text -> Text
hsName tag s = tag <> s

parsePort :: Text -> Text -> Port
parsePort name ty = Port name $ fromMaybe (error err) $ match re (T.unpack ty)
  where
    err = unwords ["Invalid port type:", show ty]
    re = pure 1 <|> (bitWidth <$> bus)
    bus = sym '[' *> ((,) <$> num <* sym ':' <*> num) <* sym ']'
    num = read <$> many (psym isDigit)

    bitWidth (a, b)
        | a < b = b - a + 1
        | otherwise = a - b + 1

getClock :: Maybe Text -> [Port] -> (Maybe Text, [Port])
getClock (Just clk) (Port name 1 : ps) | name == clk = (Just name, ps)
getClock _ ps = (Nothing, ps)

portInfo :: Text -> Port -> Value
portInfo tag (Port name width) = object
    [ "cName" .= cName name
    , "cType" .= cType ty
    , "hsName" .= hsName tag name
    , "hsType" .= hsType ty
    ]
  where
    ty = ffiType width

manifestInfo :: Maybe String -> FilePath -> FilePath -> Maybe Text -> Manifest -> Value
manifestInfo cflags srcDir outputDir clkName Manifest{..} = object
    [ "inPorts"      .= (markEnds $ map (portInfo "i") ins)
    , "outPorts"     .= (markEnds $ map (portInfo "o") outs)
    , "clock"        .= fmap (\clock -> object ["cName" .= cName clock]) clock
    , "srcs"         .= [ object ["verilogPath" .= TL.pack (srcDir </> T.unpack component <.> "v")]
                        | component <- componentNames
                        ]
    , "verilator"    .= fmap (\cflags -> object [ "cflags" .= TL.strip (TL.pack cflags) ]) cflags
    , "outputDir"    .= outputDir
    ]
  where
    (clock, ins) = getClock clkName $ zipWith parsePort portInNames portInTypes
    outs = zipWith parsePort portOutNames portOutTypes

markEnds :: [Value] -> [Value]
markEnds [] = []
markEnds (v:vs) = markStart v : vs
  where
    markStart (Object o) = Object $ o <> H.fromList [ "first" .= True ]

templates =
    [ ("src/Interface.h", $(TH.compileMustacheFile "template/Interface.h.mustache"))
    , ("src/Impl.cpp", $(TH.compileMustacheFile "template/Impl.cpp.mustache"))
    , ("src/Impl.h", $(TH.compileMustacheFile "template/Impl.h.mustache"))
    , ("Makefile",  $(TH.compileMustacheFile "template/Makefile.mustache"))
    , ("src/Clash/Clashilator/FFI.hsc", $(TH.compileMustacheFile "template/FFI.hsc.mustache"))
    ]

generateFiles :: Maybe String -> FilePath -> FilePath -> Maybe Text -> Manifest -> IO ()
generateFiles cflags inputDir outputDir clkName manifest = do
    let vals = manifestInfo cflags inputDir outputDir clkName manifest
    forM_ templates $ \(fname, template) -> do
        writeFileChanged (outputDir </> fname) $ TL.unpack $ renderMustache template vals
