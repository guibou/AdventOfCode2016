module Day2Golf where

-- code
getKeyPad (s, (x, y)) = s !! y !! x

moveKeyPad (s, c@(x, y)) i = (s, (if validCase then c' else c))
  where c'@(x', y') = case i of
          'U' -> (x, y - 1)
          'D' -> (x, y + 1)
          'L' -> (x - 1, y)
          'R' -> (x + 1, y)
        validCase = y' >= 0 && y' < length s && x' >= 0 && x' < length (s !! y') && getKeyPad (s, (x', y')) /= ' '


makeKeyPad s coord = ((lines s), coord)

-- data
keyPad = makeKeyPad "123\n\
                    \456\n\
                    \789" (1, 1)

keyPad' = makeKeyPad "  1  \n\
                     \ 234 \n\
                     \56789\n\
                     \ ABC \n\
                     \  D   " (0, 2)

-- utils
genericDay keypad code = map getKeyPad (tail (scanl (foldl moveKeyPad) keypad code))

-- problems
day code = genericDay keyPad code
day' code = genericDay keyPad' code

-- reading
content = lines <$> readFile "content/day2"
