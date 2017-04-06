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

sum_list :: (Num a) => [a] -> a
sum_list = \l -> foldl (\acc -> \head -> acc+head) 0 l

eval_poly :: [Int] -> Int -> Int
eval_poly coeffs point = sum $ zipWith (*) coeffs pointpowers
    where
        n = length coeffs
        pointpowers = map (\i -> (pow point i)) [1..n]
         
pow :: Int -> Int -> Int
pow = \base -> \exp -> foldr (*) 1 $ take exp $ repeat base

rev :: [a] -> [a]
rev lst 
    | null lst = []
    | True = (rev rest) ++ h:[]
    where
        h:rest = lst

data Tree a = Leaf a | Node a (Tree a) (Tree a)
    deriving (Eq, Show)

numleaves :: Tree a -> Int
numleaves t = case t of
    Leaf _ -> 1
    Node _ left right -> 0 + (numleaves left) + (numleaves right)

depth :: Tree a -> Int
depth t = case t of 
    Leaf _ -> 0
    Node _ left right -> 1 + max (depth left) (depth right)

t0 = Leaf 0
t1 = Leaf 1
t2 = Node 2 t0 t1
t3 = Node 3 t0 t2

{--
 - choosing the fractional representation
 - --}

data Rat = Frac Int Int
    deriving (Eq, Show)

frac_sum :: Rat -> Rat -> Rat
frac_sum f s
    | (b /= 0) && (d /= 0) = frac_reduce $ Frac (a*d + b*c) (b*d)
    | True = Frac 1 0
    where
        Frac a b = f
        Frac c d = s

frac_negative :: Rat -> Rat
frac_negative f = frac_reduce $ Frac c b
    where
        Frac a b = f
        c = -a

frac_minus :: Rat -> Rat -> Rat
frac_minus f s = frac_sum (frac_negative f) s


frac_times :: Rat -> Rat -> Rat
frac_times f s
    | (b /= 0) && (d /= 0) = frac_reduce $ Frac (a*c) (b*d)
    | True = Frac 1 0
    where
        Frac a b = f
        Frac c d = s

frac_divide :: Rat -> Rat -> Rat
frac_divide f s = frac_reduce $ frac_times f $ frac_flip s

frac_flip :: Rat -> Rat
frac_flip f
    | a==0 = Frac 1 0
    | True = Frac b a
    where
        Frac a b = f

frac_reduce :: Rat -> Rat
frac_reduce (Frac a b) = if (a==0) then Frac 0 1 else Frac c d
    where
        g = gcd' a b
        c = div a g
        d = div b g

gcd' :: Int -> Int -> Int
gcd' x y = max_list $ common_divisors x y

common_divisors :: Int -> Int -> [Int]
common_divisors x y = filter (\d -> divisor d x y) [1..n]
    where
        n = min x y
        divisor d a b = ((mod a d) == 0) && ((mod b d)==0)
        

f0 = Frac 0 1
f1 = Frac 1 1
f2 = Frac 2 1
f3 = Frac 1 2
f9 = Frac 1 9
f5 = Frac 2 5

 
{--reminder of what foldl does
 foldl f acc lst
 if null lst then acc
 else foldl (f acc h) rest
 where h:rest = lst
 and the types are:
 acc is b
 lst is [a]
 f is b -> a -> b
 and return type is b

 reminder of what foldr does
 foldr f acc lst where
 acc is b
 lst is [a]
 f is a -> b -> b
 if lst is null then acc
 else foldr f acc lst is
 f h (foldr f acc rest)
 where h:rest = lst
 and acc is b, lst is [a],
 f is a -> b -> b

 best to work out an example

 foldr f acc lst
 foldr times 1 [1..5]
 times 1 (foldr times 1 [2..5])
 times 1 (times 2 (foldr times 1 [3..5]))
 times 1 (times 2 (times 3 (foldr times 1 [4..5])))
 times 1 (times 2 (times 3 (times 4 (
   foldr times 1 [5]))))
 times 1 (times 2 (times 3 (times 4 (times 5 (foldr times 1 [])))))
 times 1 (times 2 (times 3 (times 4 (times 5 (1)))))



 h f l' if l' is null is just f
 otherwise it's h (max_pair f f') l''
 where (f', l'') = l'
--}
 
