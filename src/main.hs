{-# LANGUAGE RecordWildCards, ApplicativeDo #-}

import Clash.Clashilator
import System.FilePath
import Options.Applicative
import Data.String (fromString)

data Options = Options
    { manifestPath :: FilePath
    , outputDir :: FilePath
    , clkName :: Maybe String
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
    clkName <- optional $ strOption $ mconcat
        [ long "clock"
        , short 'c'
        , metavar "NAME"
        , help "Clock signal name"
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

    manifest <- read <$> readFile manifestPath
    let inputDir = takeDirectory manifestPath

    generateFiles Nothing inputDir outputDir (fromString <$> clkName) manifest
