import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
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

    let filePath = getArg args (argument "file")
    case filePath of
        Nothing ->
            do
                CB.sourceHandle stdin $$ dropLines numLines $= CL.mapM_ C.putStrLn
        Just path ->
            do
                runResourceT $ 
                    CB.sourceFile path $$ dropLines numLines $= CL.mapM_ (liftIO . C.putStrLn)

dropLines :: Monad m => Int -> Conduit B.ByteString m B.ByteString
dropLines dropLines = CB.lines =$= drop dropLines
    where
        drop dropNum = CL.drop dropNum >> awaitForever yield

isPositive :: Int -> Bool
isPositive num = num >= 0


