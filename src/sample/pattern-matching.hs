makeMaybe :: String -> Maybe String
makeMaybe s = if s == "Nothing" 
                    then Nothing 
                    else Just s

printMaybe :: Maybe String -> String
printMaybe Nothing = "Nothing"
printMaybe (Just s) = s

useString :: String -> (String -> a) -> a -> a
useString str onJust onNothing =
    if str == "Nothing"
        then onNothing
        else onJust str