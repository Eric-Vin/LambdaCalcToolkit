# Boolean Logic
True		\x \y -> x
False		\x \y -> y
and		\x \y -> x y x
or		\x \y -> x x y
not		\a \x \y -> a y x
IsZero		\n-> n (\x->False) True
=		\x \y -> isZero (add (sub x y) (sub y x))
<		\x \y -> not (isZero y x)
IfElse		\p \a \b -> p a b

# Natural Numbers
0 = 		\f \x -> x
1 = 		\f \x -> f x
n = 		\f \x -> f^n x
Succ		\n \f \x -> f (n f x)
Add		\m \n -> m Succ n
Mult		\m \n \f -> m (Add n) 0
Pred		\n \f \x -> n (\g \h -> h (g f)) (\u->x) (\u->u)
Sub		\m \n -> n Pred m

Integers:
		TODO Implement as Pairs

# Commands
While 		TODO
Skip 		???
x := e		???
c1 ; c2		???