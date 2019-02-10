isValidPassword :: String -> Bool
isValidPassword password = l >= 4 && l <= 8
                 where l = length password