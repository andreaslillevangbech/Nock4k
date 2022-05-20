module Nock.Spec (
    Noun(..),
    nock
) where 

data Noun = Atom Integer | Noun :- Noun deriving Eq

infixr 1 :-

nock, tar, wut, lus, tis, hax :: Noun -> Noun

nock = tar

fas (Atom 1 :- a)      = a 
fas (Atom 2 :- a :- b) = a
fas (Atom 3 :- a :- b) = b
fas (Atom n :- b) = if even n 
    then fas $ Atom 2 :- fas (Atom q :- b)
    else fas $ Atom 3 :- fas (Atom q :- b)
        where q = n `div` 2
fas a = error "inf loop in /a"

wut (Atom a) = Atom 1
wut _ = Atom 0

lus (Atom a) = Atom (a + 1)
lus _ = error "inf loop on +[a b]"

tis (a :- b) = if a == b then Atom 0 else Atom 1
tis _ = error "inf loop on =a"

hax (Atom 1 :- a :- b) = a
hax (Atom n :- b :- c) = if even n
    then hax $ Atom q :- (b :- fas (Atom (n + 1) :- c)) :- c
    else hax $ Atom q :- (fas (Atom (n - 1) :- c) :- b) :- c
        where q = n `div` 2
hax a = error "inf loop on #a"

tar (a :- (b :- c) :- d)             = tar (a :- b :- c) :- tar (a :- d)

tar (a :- Atom 0 :- b)               = fas $ b :- a
tar (a :- Atom 1 :- b)               = b
tar (a :- Atom 2 :- b :- c)          = tar $ tar (a :- b) :- tar (a :- c)
tar (a :- Atom 3 :- b)               = wut $ tar (a :- b)
tar (a :- Atom 4 :- b)               = lus $ tar (a :- b)
tar (a :- Atom 5 :- b :- c)          = tis $ tar (a :- b) :- tar (a :- c)

tar (a :- Atom 6 :- b :- c :- d)     = tar $ a :- tar ((c :- d) :- Atom 0 :- 
                                            tar ((Atom 2 :- Atom 3) :- Atom 0 :-
                                                tar (a :- Atom 4 :- Atom 4 :- b)))
tar (a :- Atom 7 :- b :- c)          = tar $ tar (a :- b) :- c
tar (a :- Atom 8 :- b :- c)          = tar $ (tar (a :- b) :- a) :- c
tar (a :- Atom 9 :- b :- c)          = tar $ tar (a :- c) :- Atom 2 :- (Atom 0 :- Atom 1) :- Atom 0 :- b
tar (a :- Atom 10 :- (b :- c) :- d)  = hax $ b :- tar (a :- c) :- tar (a :- d)
tar (a :- Atom 11 :- (b :- c) :- d)  = tar $ (tar (a :- c) :- tar (a :- d)) :- Atom 0 :- Atom 3
tar (a :- Atom 11 :- b :- c)         = tar $ a :- c

tar a                                = error "inf loop on *a"
