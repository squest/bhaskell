module Two where

x = 123

namafungsi x = x*x*x*x

mutlak x = if x >= 0 then x else (- x)

abs' x
  | x >= 0 = x
  | x < 0 = (- x)
  | otherwise = x*x*x

empty [] = True
empty _ = False

first (x:_) = x

faktorial 0 = 1
faktorial i = i * faktorial (pred i)

sum' [] = 0
sum' (x:xs) = x + sum' xs

fibo i
  | i == 0 = 1
  | i == 1 = 1
  | otherwise = (fibo $ i-1) + (fibo $ i-2)

fmua f [] = []
fmua f (x:xs) = (f x) : fmua f xs

gede x
  | x < 100 = "kecil"
  | 100 <= x && x <= 1000 = "mayan"
  | otherwise = "gede"

kecap a b c = (x1,x2)
  where det = sqrt $ b^2 - 4*a*c
        x1 = ((-b) + det) / (2*a)
        x2 = ((-b) - det) / (2*a)

null' [] = True
null' _ = False

head' [] = error "Guoblok!!"
head' (x:_) = x

rev [] = []
rev lst = iter [] lst
  where iter res [] = res
        iter res (x:xs) = iter (x:res) xs


len [] = 0
len (x:xs) = 1 + len xs

-- len [1,2,3] = 1 + len [2,3]
-- len [2,3] = 1 + len[3]
-- len [3] = 1 + len []

fak 1 = 1
fak i = i * (fak $ i-1)

-- fak 4 = 4*fak 3
-- fak 3 = 3* fak 2
-- fak 2 = 2* fak 1

square 0 = 1
square 1 = 0
square x = x*x

elem' e [] = False
elem' e (x:xs)
  | e == x = True
  | otherwise = elem' e xs 

-- elem' 3 [1,2,3,4,5] = elem' 3 [2,3,4,5]
-- elem' 3 [2,3,4,5] = elem' 3 [3,4,5]
-- elem' 3 [3,4,5] = True

notElem' e [] = True
notElem' e (x:xs)
  | e == x = False
  | otherwise = notElem' e xs

insert' e [] = [e]
insert' e (x:xs)
  | e < x = x : insert' e xs
  | otherwise = e:x:xs

union' ls [] = ls
union' ls (x:xs)
  | elem' x ls = union' ls xs
  | otherwise = union' (insert' x ls) xs

add x 0 = x
add a b
  | b > 0 = add (succ a) (pred b)
  | otherwise = sub a (- b)

sub a 0 = a
sub a b
  | b > 0 = sub (pred a) (pred b)
  | otherwise = add a (- b)

cats ls1 ls2 = iter (rev ls1) ls2
  where x = 100
        iter [] res = res
        iter (x:xs) res = iter xs (x:res)

coba x
  | x < 10 = 1
  | True = 5

cycle' xs = cats xs $ cycle' xs

delete' e [] = []
delete' e (x:xs)
  | e == x = delete' e xs
  | otherwise = x : delete' e xs

nub' [] = []
nub' (x:xs) = x:nub' (delete' x xs)

max' a b
  | a > b = a
  | otherwise = b















                
















