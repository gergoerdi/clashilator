{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}

import Clash.Driver.Types

import Data.Maybe (fromMaybe)

import Text.Regex.Applicative
import Data.Char (isDigit)

import System.FilePath
import System.Directory

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO

import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH
import Data.Aeson hiding (Options)
import qualified Data.HashMap.Strict as H

import Options.Applicative

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

removeClock :: [Port] -> (Maybe Text, [Port])
removeClock (Port clk 1 : ps) = (Just clk, ps)
removeClock ps = (Nothing, ps)

portInfo :: Text -> Port -> Value
portInfo tag (Port name width) = object
    [ "cName" .= cName name
    , "cType" .= cType ty
    , "hsName" .= hsName tag name
    , "hsType" .= hsType ty
    ]
  where
    ty = ffiType width

markEnds :: [Value] -> [Value]
markEnds [] = []
markEnds (v:vs) = markStart v : vs
  where
    markStart (Object o) = Object $ o <> H.fromList [ "first" .= True ]

ifaceTmpl  = $(TH.compileMustacheFile "template/Interface.h.mustache")
implTmpl   = $(TH.compileMustacheFile "template/Impl.cpp.mustache")
hdrTmpl    = $(TH.compileMustacheFile "template/Impl.h.mustache")
bridgeTmpl = $(TH.compileMustacheFile "template/VerilatorFFI.hsc.mustache")

-- opts = strOption $
--     long "output" <>
--     short 'o' <>
--     metavar "DIRECTORY"

data Options = Options
    { manifestPath :: FilePath
    , outputDir :: FilePath
    }

options :: Parser Options
options = do
    manifestPath <- strOption $ mconcat
        [ long "input"
        , short 'i'
        , metavar "FILENAME"
        , help "Clash manifest file"
        ]
    outputDir <- strOption $ mconcat
        [ long "output"
        , short 'o'
        , metavar "DIRECTORY"
        , help "Where to put generated files"
        ]
    pure Options{..}

optionsInfo = info (options <**> helper) $ mconcat
    [ fullDesc
    , header "Clashilator - Clash <-> Verilator interface"
    , progDesc "Generate Verilator source files and FFI interface from Clash manifest"
    ]

main :: IO ()
main = do
    Options{..} <- execParser optionsInfo
    createDirectoryIfMissing True outputDir

    manifest@Manifest{..} <- read <$> readFile manifestPath

    let (clock, ins) = removeClock $ zipWith parsePort portInNames portInTypes
        outs = zipWith parsePort portOutNames portOutTypes

    let vals = object
            [ "inPorts"  .= (markEnds $ map (portInfo "i") ins)
            , "outPorts" .= (markEnds $ map (portInfo "o") outs)
            , "clock"    .= fmap (\clock -> object ["cName" .= cName clock]) clock
            ]

    TIO.writeFile (outputDir </> "Interface" <.> "h") $ renderMustache ifaceTmpl vals
    TIO.writeFile (outputDir </> "Impl" <.> "cpp") $ renderMustache implTmpl vals
    TIO.writeFile (outputDir </> "Impl" <.> "h") $ renderMustache hdrTmpl vals
    TIO.writeFile (outputDir </> "VerilatorFFI" <.> "hsc") $ renderMustache bridgeTmpl vals
