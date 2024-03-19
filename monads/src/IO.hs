module IO where 

myGetLine :: IO String 
myGetLine = do
    c <- getChar
    if c == '\n' then return [] else do
        cs <- myGetLine
        return (c:cs)

helloUser :: IO () 
helloUser = do
    putStrLn "Please enter your name:"
    name <- myGetLine
    putStrLn("Hello, " ++ name)

helloUser' :: IO () 
helloUser' = interact(\input -> "Hello, " ++ input ++ "\n")
