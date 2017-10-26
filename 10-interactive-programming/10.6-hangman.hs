hangman :: IO ()
hangman = do
  putStrLn "Think of a word:"
  word <- sgetLine
  putStrLn "Try to guess it:"
  play word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then 
                 do putChar x
                    return []
              else 
                 do putChar '_'
                    xs <- sgetLine
                    return (x:xs)
