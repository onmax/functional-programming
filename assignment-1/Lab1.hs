-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab1(minus,fun1,fun2,fun3,fun4,fun5,fun6,sumSquareDiff) where

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- 1.1

       product 2
   --> 2 * product (1) 
   --> 2 * 1
   --> 2

   1.2

   {- product n
     Returns the factorial of a number.
     PRE:  n >= 0
     RETURNS: n!
     SIDE EFFECTS: Negative numbers will loop infinitely
     EXAMPLES: product 0 = 1
               product 2 = 2
  -}
   product n = :: Integer -> Integer
   product n
   ...

   1.3

   VARIANT: n
   
            If n element of N (Natural Numbers) and n != 0
            -> n-1 element of N and n - 1 < n
 -}

{- 2.1 -}
-- remember to provide a function specification
minus :: Integer -> Integer -> Integer
minus = \x y -> x - y  

{- 2.2
   foo is of type Integer and returns 1 when executed.

   2.3
   bar is of type Integer -> Integer (Integer function)
   It returns an error when executed without an argument.
   The minus function wants a second argument. 
   If passed, the function works as expected.

   2.4

       minus 5 4
   --> (x = 5) - (y = 4)
       5 - 4
       1
 -}

{- 3.1 -}
{- fun1 x
     Adds 1 to x.
     RETURNS: x + 1
     EXAMPLES: fun1 0 = 1
               fun1 3 = 4
-}
fun1 :: Integer -> Integer
fun1 = \x -> (x + 1)

{- 3.2 -}
{- fun2 x y
     Adds x and y.
     RETURNS: x + y
     EXAMPLES: fun2 0 1 = 1
               fun2 3 4 = 7
-}
fun2 :: Integer -> Integer -> Integer
fun2 = \x y -> (x + y) :: Integer

{- 3.3 -}
{- fun3 x
     Returns a tuple with two x's.
     RETURNS: (x, x)
     EXAMPLES: fun3 0 = (0, 0)
               fun3 3 = (3, 3)
-}
fun3 :: Integer -> (Integer, Integer)
fun3 = \x -> (x, x)

{- 3.4 -}
{- fun4 x y
     Adds x and y.
     RETURNS: x + y
     EXAMPLES: fun4 0 0 = 0
               fun4 3 3 = 6
-}
fun4 :: (Integer, Integer) -> Integer
fun4 = \(x, y) ->  x + y

{- 3.5 -}
{- fun5 x y str
     Casts number arguments to string and concatenates them to the String argument.
     RETURNS: str(x) + str(y) + str
     EXAMPLES: fun5 1 0.1 "g" = "10.1g"
               fun5 33 -3.33 "3" = "33-3.333"
-}
fun5 :: Integer -> Double -> String -> String
fun5 = \x y str -> show x ++ show y ++ str

{- 3.6 -}
{- fun6 x, str1, str2, y
     Adds the two Integer arguments and concatenates the two String arguments. Returns them as a tuple.
     RETURNS: (x + y + str1 + str2)
     EXAMPLES: fun6 1 "a" "b" 0 = (1, "ab")
               fun6 -1 "-1" "z" 1 = (0, "-1z")
-}
fun6 :: Integer -> String -> String -> Integer -> (Integer, String)
fun6 = \x str1 str2 y -> (x + y, str1 ++ str2)




{- 4 -}

{- sumOfSquares n
     Computes the sum of squares.
     PRE: n >= 0
     RETURNS: (n^2 + (n-1)^2 + .. 1^2)
     EXAMPLES: sumOfSquares 0 = 0
               sumOfSquares 3 = 14
-}
sumOfSquares :: Integer -> Integer
-- VARIANT: n
sumOfSquares 0 = 0
sumOfSquares n = n^2 + sumOfSquares (n - 1)

{- sumToOne n
     Computes the sum of the numbers up to n.
     PRE: n >= 0
     RETURNS: (n + (n-1) + .. 1)
     EXAMPLES: sumToOne 0 = 0
     sumToOne 4 = 10
-}
sumToOne :: Integer -> Integer
-- VARIANT: n
sumToOne 0 = 0
sumToOne n = n + sumToOne (n - 1)

{- sumSquareDiff n
     Computes the difference between the square of the sum of numbers up to n 
     and the sum of the squares of numbers up to n.
     PRE: n >= 0
     RETURNS: (n + (n-1) + .. 1) - (n^2 + (n-1)^2 + .. 1^2)
     EXAMPLES: sumSquareDiff 0 = 0
               sumSquareDiff 10 = 2640
-}
sumSquareDiff :: Integer -> Integer
-- VARIANT: n
sumSquareDiff n = sumToOne(n)^2 - sumOfSquares(n)

