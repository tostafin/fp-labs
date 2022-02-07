isPalindrome :: [Char] -> Bool
isPalindrome s = if take halfLen s == reverse (drop halfLen s)
                 then True
                 else False
                 where halfLen = length s `div` 2
