## Step 1: Define the type

```hs
product :: [Int] -> [Int]
```

## Step 2: Enumerate the cases

For lists: [] {empty} and (n:ns) {non-empty}
For logical values: True and False
For non-negative integers: 0 and n

```hs
product []     =
product (n:ns) =
```

## Step 3: define the simple cases

Often becomes the base case.
1 is the identity for multiplication
0 is the identity for addition

```hs
product []     = 1
product (n:ns) =
```

## Step 4: define the other cases

Consider the function itself, the arguments, and
library functions of relevant types.

```hs
product []     = 1
product (n:ns) = n * product ns
```

## Step 5: generalize and simplify

```hs
product :: (Num a) => [a] -> a
product = foldr (*) 1
```
