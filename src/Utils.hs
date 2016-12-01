module Utils where

-- So I can use it in the shell
-- dayX <$$> content

(<$$>) f x = (fmap . fmap) f x

infixl 4 <$$>
