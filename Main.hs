
import Text.ProseDoc
import System.Environment

main = fmap head getArgs >>= generatePage >>= putStrLn
