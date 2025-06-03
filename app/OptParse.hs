module OptParse where

import Data.Maybe (fromMaybe)
import Options.Applicative

-- We want to be able to provide a single file/input stream to a single file/output stream
-- Or we want to be able to provide a whole directory and write to an entire directory

------------------------------------------------------------------------------------------

-- * CLI options Model

-- | Model
data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath

-- | Single Input source
data SingleInput
  = Stdin
  | InputFile FilePath
  deriving (Show)

-- | Single Output sink
data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving (Show)

------------------------------------------------------------------------------------------

-- * Parser

-- | Parse CLI options
parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts =
  info
    (helper <*> pOptions)
    ( fullDesc
        <> header "haskell-blog-generator - a static blog generator made to learn how to use haskell"
        <> progDesc "Convert markup files or directories to HTML."
    )

-- | Parser for all options
pOptions :: Parser Options
pOptions =
  subparser $
    command "convert" pConvertSingleInfo
      <> command "convert-dir" pConvertDirInfo

------------------------------------------------------------------------------------------

-- * Parsers for single source and directory level output

-- | Parsers with usage info
pConvertSingleInfo :: ParserInfo Options
pConvertSingleInfo = info (helper <*> pConvertSingle) (progDesc "Convert a single markup source to html")

pConvertDirInfo :: ParserInfo Options
pConvertDirInfo = info (helper <*> pConvertDir) (progDesc "Convert a directory of markup files to html")

-- | Parser for single source to sink options
pConvertSingle :: Parser Options
pConvertSingle = ConvertSingle <$> pSingleInput <*> pSingleOutput

-- | Parser for single source
pSingleInput :: Parser SingleInput
pSingleInput = fromMaybe Stdin <$> optional pInputFile

-- | Parser for single sink
pSingleOutput :: Parser SingleOutput
pSingleOutput = fromMaybe Stdout <$> optional pOutputFile

-- | Parser for dir to dir conversion options
pConvertDir :: Parser Options
pConvertDir = ConvertDir <$> pInputDir <*> pOutputDir

-- | Parsers for single source, sink files
pInputFile :: Parser SingleInput
pInputFile =
  InputFile
    <$> strOption
      (short 'i' <> long "input" <> metavar "FILE" <> help "Input file path")

pOutputFile :: Parser SingleOutput
pOutputFile =
  OutputFile
    <$> strOption
      (short 'o' <> long "output" <> metavar "FILE" <> help "Output file path")

-- | Parsers For input, output directories
pInputDir :: Parser FilePath
pInputDir =
  strOption $
    short 'i' <> long "input" <> metavar "DIRECTORY" <> help "Input directory"

pOutputDir :: Parser FilePath
pOutputDir =
  strOption $
    short 'o' <> long "output" <> metavar "DIRECTORY" <> help "Output directory"
