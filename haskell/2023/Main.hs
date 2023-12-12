import System.Environment ( getArgs )
import System.Environment.MrEnv ( envAsString )
import Days (getDay)
import System.TimeIt ( timeItT )
import Text.Printf ( printf )

main :: IO()

loadFile dayNum [_, "sample"] = do
  let fileName = "inputs/" ++ dayNum ++ ".sample.txt"
  putStrLn $ "Loading input from " ++ fileName
  readFile fileName
loadFile dayNum _ = do
  let fileName = "inputs/" ++ dayNum ++ ".txt"
  putStrLn $ "Loading input from " ++ fileName
  readFile fileName

execPart input day "1"  = fst day input
execPart input day "2" = snd day input

run input args dayString = do
  let result = execPart input (getDay dayString) (head args)
  printf "\nDay %s Part %s solution: %s\n\n" dayString (unwords args) result

main = do
  args <- getArgs
  dayString <- envAsString "DAY" "01"
  input <- loadFile dayString args
  (time, _) <- timeItT $ run input args dayString
  printf "CPU Time: %6.3fms\n" $ time * 1000

