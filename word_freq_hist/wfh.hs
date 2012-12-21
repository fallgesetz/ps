-- http://www.scs.stanford.edu/11au-cs240h/labs/lab1.html
import qualified Data.Map as M
import Data.List
import System.IO

main :: IO ()
main = interact histogram

histogram :: String -> String
histogram = make_hist . uniquify_words .  group_words

group_words :: String -> [(String, Int)]
group_words = (map (\x -> ((head x), (length x)))) . (group . words)

uniquify_words :: [(String, Int)] -> [(String, Int)]
uniquify_words = (sortBy (\x y -> compare (snd y) (snd x))) . M.toList . (M.fromListWith (+))

make_hist :: [(String, Int)] -> String
make_hist = unlines . (map (\(s, i) -> s ++ ['\t'] ++ (replicate i '#')))

