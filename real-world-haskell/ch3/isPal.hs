isPalindrome xs = isPalindromeImpl 0 l [] xs
    where l = length xs
          isPalindromeImpl len1 len now (x:xs) | 2 * len1 + 1 == len =
              now == xs
          isPalindromeImpl len1 len now xs | 2 * len1 == len =
              now == xs
          isPalindromeImpl len1 len now (x:xs) =
              isPalindromeImpl (len1 + 1) len (x : now) xs
