import Data.Char (isUpper, isLower, isDigit)


forte :: String -> Bool
forte x = length x >= 8 && hasUpper x && hasLower x && hasDigit x
  where
    hasUpper [] = False
    hasUpper (y:ys) = (y >= 'A' && y <= 'Z') || hasUpper ys

    hasLower [] = False
    hasLower (y:ys) = (y >= 'a' && y <= 'z') || hasLower ys

    hasDigit [] = False
    hasDigit (y:ys) = (y >= '0' && y <= '9') || hasDigit ys


forte2 :: String -> Bool
forte2 x = length x >= 8 && any isUpper x && any isLower x && any isDigit x
