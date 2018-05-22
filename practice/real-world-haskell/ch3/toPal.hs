toPalindrome = toPalindromeImpl []
    where toPalindromeImpl la (x: xs) = x : toPalindromeImpl (x: la) xs
          toPalindromeImpl la []      = la

