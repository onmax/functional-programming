-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab2(iota,inter,interOrdered,isMatch) where

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- 1

    1. head :: [a] -> a
    2. tail :: [a] -> [a]
    3. \x -> x :: p -> p
    4. (,) :: a -> b -> (a, b)
    5. (:) :: a -> [a] -> [a]
    6. [[]] :: [[a]]
    7. tail [[]] :: [[a]]
    8. id : []  :: [a -> a]
    9. id id :: a -> a
   10. head [id] "foo" :: [Char]

   The polymorphic expressions are all of them
 -}

{- 2 -}
{- iota n
     Create a list with the values: 0 to n-1
     PRE: n >= 0
     RETURNS: [0 1 .. n-1]
     EXAMPLES: iota 5 = [0 1 2 3 4]
     iota 0 = []
-}

iota n = [0..n-1]
--The expression above is not a function. However, for testing purposes(task 3.3), it is much faster to execute. Therefore, we use it here.
--ota 0 = [] 
--ota n = iota(n-1) ++ [n-1] 

{- 3.1 -}
{- inter a b
     Returns a list of the values that are in list a and b
     RETURNS: A list of numbers
     EXAMPLES: iota [3 1 2] [4 3 2] = [2 3]
     iota [1 4 3 9 8 2] [5 7 7] = []
-}
inter [] _ = []
inter a b = if elem (head a) b 
            then [head a] ++ inter (tail a) b
            else inter (tail a ) b 

{- 3.2 -}
{- interOrdered a b
     Returns a list of the values that are in list a and b
     PRE: a1 < a2 < .. < an && b1 < b2 < .. < bn; where aX and bX represent the value of the list in position X
     RETURNS: A list of numbers
     EXAMPLES: interOrdered [1 2 3] [2 3 4] = [2 3]
     interOrdered [1 2 3 4 6 9] [5 7 7] = []
-}
interOrdered _ [] = []
interOrdered [] _ = []
interOrdered a b = if elem (head a) b 
            then [head a] ++ inter (tail a) (tail b)
            else inter (tail a) b 

{- 3.3 -}

s1 = iota 100000
s2 = iota 1000000

t1 = inter s1 s2
t2 = interOrdered s1 s2

{- 4 -}
{- isMatch a b
     Easy implementation of regular expression. a will a string and b will be the pattern. Special chars:
     *: 0 or more chars. ?: Any char
     RETURNS: A boolean that represents if the pattern b is in the string a
     EXAMPLES: 
     isMatch "aa" "aa" = True
     isMatch "hello world!" "*w*" = True
     isMatch "Monday" "MMnd???" = False
     isMatch "Monday" "*???" = True
-}
isMatch [] [] = True
isMatch [] b = False || head(b) /= '*'
isMatch _ [] = False
isMatch a r = if head(a) == head(r) || head(r) == '?'
              then isMatch (tail(a)) (tail(r)) 
              else
                if head(r) == '*'
                then 
                  if length r > 1 && head(a) == head(tail(r)) && head(tail(r)) /= '*'
                  then isMatch (tail(a)) (tail(tail(r)))
                  else 
                    if length r == 1
                    then True
                    else
                      if head(tail(r)) ==  '*'
                      then isMatch a (tail(r))
                      else isMatch (tail(a)) r
                else
                  False

