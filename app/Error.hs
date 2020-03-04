module Error where

data Error = Other String

type OrError = Either Error

errMsg :: Error -> String
errMsg (Other msg) = msg

run :: OrError a -> IO ()
run (Left err) = putStrLn $ "Error: " ++ (errMsg err)
run (Right _) = return ()
