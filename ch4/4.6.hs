-- Exercise 4.6: Without using any other library functions or operators,
-- show how the meaning of the following pattern matching definition for
-- logical conjunction && can be formalized using conditional expressions:
--
-- True  && b = b
-- False && _ = False

(&&) :: Bool -> Bool -> Bool
(&&) a b = if a then b else False
