{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

import Clash.Driver.Types

import Data.Maybe (fromMaybe)

import Text.Regex.Applicative
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.FilePath
import System.Directory

data Port = Port T.Text Int
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

cType :: FFIType -> T.Text
cType FFIBit = "bit"
cType FFIU8 = "uint8_t"
cType FFIU16 = "uint16_t"
cType FFIU32 = "uint32_t"
cType FFIU64 = "uint64_t"

parsePort :: T.Text -> T.Text -> Port
parsePort name ty = Port name $ fromMaybe (error err) $ match re (T.unpack ty)
  where
    err = unwords ["Invalid port type:", show ty]
    re = pure 1 <|> (bitWidth <$> bus)
    bus = sym '[' *> ((,) <$> num <* sym ':' <*> num) <* sym ']'
    num = read <$> many (psym isDigit)

    bitWidth (a, b)
        | a < b = b - a + 1
        | otherwise = a - b + 1

removeClock :: [Port] -> (Maybe T.Text, [Port])
removeClock (Port clk 1 : ps) = (Just clk, ps)
removeClock ps = (Nothing, ps)

genInterface :: [Port] -> [Port] -> T.Text
genInterface ins outs = T.unlines
    [ "#include <stdint.h>"
    , ""
    , "typedef int Bit;"
    , ""
    , "typedef struct"
    , "{"
    , fields ins
    , "} INPUT;"
    , ""
    , "typedef struct"
    , "{"
    , fields outs
    , "} OUTPUT;"
    ]
  where
    fields ports = T.unlines . map ("    " <>) $
        [ cType (ffiType width) <> " " <> name <> ";"
        | Port name width <- ports
        ]

main :: IO ()
main = do
    inFile <- return "specimen/topEntity.manifest"
    outDir <- return "specimen/verilator"
    createDirectoryIfMissing True outDir

    manifest@Manifest{..} <- read <$> readFile inFile

    -- mapM_ print $ zipWith parsePort portInNames portInTypes
    -- mapM_ print $ zipWith parsePort portOutNames portOutTypes
    let (clock, ins) = removeClock $ zipWith parsePort portInNames portInTypes
        outs = zipWith parsePort portOutNames portOutTypes

    T.writeFile (outDir </> "API" <.> "h") $ genInterface ins outs
