{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

import Clash.Driver.Types

import Data.Maybe (fromMaybe)

import Text.Regex.Applicative
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Aeson

import System.FilePath
import System.Directory

import Text.Mustache as M

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

portValue :: Text -> Port -> Value
portValue tag (Port name width) = object
    [ "cName" .= cName name
    , "cType" .= cType ty
    , "hsName" .= hsName tag name
    ]
  where
    ty = ffiType width

main :: IO ()
main = do
    ifaceTmpl <- compileMustacheFile "template/Interface.h.mustache"
    implTmpl <- compileMustacheFile "template/Impl.cpp.mustache"
    hdrTmpl <- compileMustacheFile "template/Impl.h.mustache"
    bridgeTmpl <- compileMustacheFile "template/Bridge.hsc.mustache"

    inFile <- return "specimen/topEntity.manifest"
    outDir <- return "specimen/verilator"
    createDirectoryIfMissing True outDir

    manifest@Manifest{..} <- read <$> readFile inFile

    let (clock, ins) = removeClock $ zipWith parsePort portInNames portInTypes
        outs = zipWith parsePort portOutNames portOutTypes

    let vals = object
            [ "inPorts"  .= map (portValue "i") ins
            , "outPorts" .= map (portValue "o") outs
            , "clock"    .= maybe [] (\clock -> [object ["cName" .= cName clock] ]) clock
            ]

    TIO.writeFile (outDir </> "Interface" <.> "h") $ renderMustache ifaceTmpl vals
    TIO.writeFile (outDir </> "Impl" <.> "cpp") $ renderMustache implTmpl vals
    TIO.writeFile (outDir </> "Impl" <.> "h") $ renderMustache hdrTmpl vals
    TIO.writeFile (outDir </> "Bridge" <.> "hsc") $ renderMustache bridgeTmpl vals
