module OptParse where

-- We want to be able to provide a single file/input stream to a single file/output stream
-- Or we want to be able to provide a whole directory and write to an entire directory

-- We are specifying the allowed input shapes. Single file/stream inp-> single file/stream out
-- or dir in -> dir out
data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath

-- Here we specify that single inputs/outputs can be either the streams or filepaths.
data SingleInput
  = Stdin
  | InputFile FilePath
  deriving (Show)

data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving (Show)
