-- Exercise 15.4: Using a list comprehension, define an expression
-- fibs :: [Integer] that generates the infinite sequence of Fibonacci numbers
--
-- 0,1,1,2,3,5,8,13,21,34,...
--
-- using the following simple procedure:
-- - the first two numbers are 0 and 1;
-- - the next is the sum of the previous two;
-- - return to the second step.
--
-- Hint: make use of the library functions zip and tail. Not that numbers in the
-- Fibonacci sequence quickly become large, hence use the type of Integer of
-- arbitrary-precision integers alone.

fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]
