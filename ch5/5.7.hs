-- Exercise 5.7: Show how the list comprehension [(x,y) | x <- [1,2], y <- [3.4]]
-- with two generators can be re-expressed using two comprehensions with single
-- generators.

a = [(x, y) | x <- [1, 2], y <- [3, 4]]

b = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]
