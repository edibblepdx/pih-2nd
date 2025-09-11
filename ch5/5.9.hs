-- Exercise 5.9: The scalar product of two lists of integers xs and ys of length
-- n is given by the sum of products of corresponding integers. In a similar
-- manner to chisqr, show how a list comprehension can be used to define a
-- function scalarproduct :: [Int] -> [Int] -> Int that returns the scalar
-- product of two lists.

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
