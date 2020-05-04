{-# LANGUAGE RecordWildCards, ApplicativeDo #-}

import Clash.Clashilator
import System.FilePath
import Options.Applicative

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

    manifest <- read <$> readFile manifestPath
    let inputDir = takeDirectory manifestPath

    generateFiles inputDir outputDir manifest
