module Main (
  main
) where
import LibertyServ.ClientDispatcher

main :: IO ()
main = do
  putStrLn "LibertyServ starting..."
  runClientDispatcher

