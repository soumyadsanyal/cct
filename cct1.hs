square :: Int -> Int
square n = n*n

sign :: (Ord a, Num a) => a -> a
sign = \x -> if x < 0 then -1 else 1

absvalue :: (Num a, Ord a) => a -> a
absvalue x = x*(sign x)

max_pair :: (Ord a) => a -> a -> a
max_pair x y = if (x<y) then y else x

fib :: (Num b, Eq b) => b -> Int
fib n
    | (n == 0) || (n == 1) = 1
    | True = fib (n-1) + fib (n-2)

data Nat = Zero | Succ Nat
    deriving (Eq, Show)

natify :: Int -> Nat
natify n
    | n < 0 = error "negative!"
    | n == 0 = Zero
    | True = Succ $ natify $ n-1

intify :: Nat -> Int
intify n
    | n == Zero = 0
    | True = 1 + (intify rest)
    where Succ rest = n

max_list :: [Int] -> Int
max_list l
    | null l = error "wtf"
    | True = max_list_helper f l'
    where (f: l') = l

max_list_helper :: Int -> [Int] -> Int
max_list_helper f l = foldl max_ f l
    where max_ x y = if (x<y) then y else x


-- now I know there is at least one
-- thing in the list. let's take that
-- and call it f.
-- then let's run a helper function 
-- helper f l'
--
-- reminder of what foldl does
-- foldl f acc lst
-- if null lst then acc
-- else foldl (f acc h) rest
-- where h:rest = lst
-- and the types are:
-- acc is b
-- lst is [a]
-- f is b -> a -> b
-- and return type is b
--
--
--
--
-- h f l' if l' is null is just f
-- otherwise it's h (max_pair f f') l''
-- where (f', l'') = l'
--
 
