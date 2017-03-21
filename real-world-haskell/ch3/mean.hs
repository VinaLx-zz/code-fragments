mean (x : xs) = meanImpl 1 x xs
    where meanImpl l s (x: xs) = meanImpl (l + 1) (s + x) xs
          meanImpl l s [] = s / l
