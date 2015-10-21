import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CL
import Safe
import System.Console.Docopt
import System.Environment (getArgs)
import System.IO

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

main = do
    args <- getArgs >>= parseArgsOrExit patterns

    -- Exit early if the user gave us anything other than a positive integer as the number of lines
    numLinesArgument <- getArgOrExitWith patterns args (argument "num_lines")
    let maybeLines = mfilter isPositive $ readMay numLinesArgument 
    numLines <- maybe (exitWithUsage patterns) (return . id) maybeLines

    -- If an input file or output file are not defined, we use standard in and out respectively
    let maybeInputFile  = getArg args (argument "input_file")
    let maybeOutputFile = getArg args (shortOption 'o')

    dropLines numLines maybeInputFile maybeOutputFile

dropLines :: Int -> Maybe String -> Maybe String -> IO ()
dropLines numLines inputFile outputFile = 
    runResourceT $ do
        let source = maybe (CB.sourceHandle stdin) CB.sourceFile inputFile
        let sink = maybe (CB.sinkHandle stdout) CB.sinkFile outputFile
        source =$= dropLinesConduit numLines $$ sink      

dropLinesConduit :: Monad m => Int -> Conduit B.ByteString m B.ByteString
dropLinesConduit 0 = awaitForever $ yield
dropLinesConduit dropLines = go
    where
        go =
            do 
                next <- await
                case next of
                    Nothing -> return ()
                    Just bs -> do
                        let remaining = (B.drop 1 $ B.dropWhile (not . isNewLine) bs)
                        if B.null remaining
                            then dropLinesConduit (dropLines - 1)
                            else leftover remaining >> dropLinesConduit (dropLines - 1)

        isNewLine chr = chr == 10

isPositive :: Int -> Bool
isPositive num = num >= 0


