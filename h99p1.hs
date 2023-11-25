{------------  Problem 1 ----------------------------

    (*) Find the last element of a list.

        Example in Haskell:

        λ> myLast [1,2,3,4]
        4
        λ> myLast ['x','y','z']
        'z'
------------------------------------------------------}
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

{------------  Problem 2 ----------------------------
    (*) Find the last-but-one (or second-last) element of a list.
    
    (Note that the Lisp transcription of this problem is incorrect.)

    Example in Haskell:

        λ> myButLast [1,2,3,4]
        3
        λ> myButLast ['a'..'z']
        'y'
------------------------------------------------------}
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [y, x] = y
myButLast (x:xs) = myButLast xs

{------------  Problem 3 ----------------------------
    (*) Find the K'th element of a list.
    
    The first element in the list is number 1. Example:

        * (element-at '(a b c d e) 3)
        c
    Example in Haskell:

        λ> elementAt [1,2,3] 2
        2
        λ> elementAt "haskell" 5
        'e'
------------------------------------------------------}
elementAt :: [a] -> Int -> a
elementAt list i    = list !! (i-1)

{------------  Problem 4 ----------------------------
    (*) Find the number of elements in a list.
    
    Example in Haskell:

        λ> myLength [123, 456, 789]
        3
        λ> myLength "Hello, world!"
        13
------------------------------------------------------}
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

{------------  Problem 5 ----------------------------
    Problem 5
    (*) Reverse a list.
    
    Example in Haskell:

        λ> myReverse "A man, a plan, a canal, panama!"
        "!amanap ,lanac a ,nalp a ,nam A"
        λ> myReverse [1,2,3,4]
        [4,3,2,1]
------------------------------------------------------}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

{------------  Problem 6 ----------------------------
    (*) Find out whether a list is a palindrome.
    
    Hint: A palindrome can be read forward or backward; e.g. (x a m a x).

    Example in Haskell:

        λ> isPalindrome [1,2,3]
        False
        λ> isPalindrome "madamimadam"
        True
        λ> isPalindrome [1,2,4,8,16,8,4,2,1]
        True
------------------------------------------------------}
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

{------------  Problem 7 ----------------------------
    (**) Flatten a nested list structure.
    
    Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

    Example:

        * (my-flatten '(a (b (c d) e)))
        (A B C D E)
        Example in Haskell:

    We have to define a new data type, because lists in Haskell are homogeneous.

        data NestedList a = Elem a | List [NestedList a]

        λ> flatten (Elem 5)
        [5]
        λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
        [1,2,3,4,5]
        λ> flatten (List [])
        []

        List [Elem 1, List [Elem 2, Elem 3]]
------------------------------------------------------}
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

{------------  Problem 8 ----------------------------
    (**) Eliminate consecutive duplicates of list elements.
    
    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

    Example:

        * (compress '(a a a a b c c a a d e e e e))
        (A B C A D E)
    Example in Haskell:

        λ> compress "aaaabccaadeeee"
        "abcade"
------------------------------------------------------}
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y = compress (y:xs)
    | otherwise = x : compress (y:xs)

{------------  Problem 9 ----------------------------
    (**) Pack consecutive duplicates of list elements into sublists.
    
    If a list contains repeated elements they should be placed in separate sublists.

    Example:

        * (pack '(a a a a b c c a a d e e e e))
        ((A A A A) (B) (C C) (A A) (D) (E E E E))

    Example in Haskell:

        λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
        ["aaaa","b","cc","aa","d","eeee"]
------------------------------------------------------}
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = [x : takeWhile (==x) xs ] ++ pack (dropWhile (==x) xs)

{------------  Problem 10 ----------------------------
    (*) Run-length encoding of a list.
    
    Use the result of Problem 9 to implement the so-called run-length encoding data compression method. 
    Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

    Example:

        * (encode '(a a a a b c c a a d e e e e))
        ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

    Example in Haskell:

        λ> encode "aaaabccaadeeee"
        [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
------------------------------------------------------}
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = [(length (head (pack (x:xs))), x)] ++ encode (dropWhile (== x) xs)
