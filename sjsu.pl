student(smith).
student(jones).
student(nguyen).
student(young).
student(sherman).

instructor(pearce).
instructor(godel).
instructor(escher).
instructor(bach).

taken(smith, cs46a, pass).
taken(smith, cs46b, fail).
taken(smith, cs146, pass).
taken(jones, cs46a, pass).
taken(jones, cs46b, pass).
taken(jones, cs146, pass).
taken(nguyen, cs46a, pass).
taken(nguyen, cs46b, pass).
taken(nguyen, cs151, pass).

teaches(pearce, cs152).
teaches(godel, cs151).
teaches(escher, cs146).
teaches(bach, cs46b).
teaches(bach, cs46a).

prerequisite(cs152, cs151).
prerequisite(cs152, cs46b).
prerequisite(cs151, cs46b).
prerequisite(cs146, cs46b).
prerequisite(cs46b, cs46a).


%lab 1
academic(X) :- student(X); instructor(X).

passed(X, Y) :-  taken(X, Y, pass).
studentOf(X, Y) :- taken(X, V, _), teaches(Y,V).
upperDivisionCSstudent(X) :- taken(X, cs46a, pass), taken(X, cs46b, pass).
teachesPreReq(X) :- teaches(X, Y), prerequisite(_, Y).
canTake(X, Y) :- foreach(prerequisite(Y, Z), taken(X, Z, pass)).

%lab 3
add(zero, X, Y) :- X = Y.
add(inc(X), Y, Z) :- add(X, Y, V), Z = inc(V).

mul(zero, _, zero).
mul(inc(X), Y, Z) :- mul(X, Y, V), add(Y, V, Z).


exp(_X, zero, inc(zero)).
exp(X, inc(Y), Z) :- exp(X, Y, V), mul(X, V, Z).

less(zero, inc(_)).
less(inc(X), inc(Y)) :- less(X, Y).

%lab 5
eval(num(X), Y) :- Y = X.
eval(sum(X, Y), Z) :- eval(X, V), eval(Y, W), Z is V + W.
eval(mul(X, Y), Z) :- eval(X, V), eval(Y, W), Z is V * W.

%lab 8

%Problem 1
class(closure).
class(number).
class(boole).
class(identifier).
class(conditional).
class(funcall).

interface(value).
interface(literal).
interface(expression).
interface(specialform).

extends(value, literal).
extends(expression, literal).
extends(expression, specialform).

implements(value, closure).
implements(literal, number).
implements(literal, boole).
implements(expression, identifier).
implements(expression, funcall).
implements(specialform, conditional).

dep(X, Y) :- X = Y.
dep(X, Y) :- extends(Y, X).
dep(X, Y) :- implements(Y, X).

depends(X, Y) :- dep(X, Y).
depends(X, Y) :- dep(X, Z), dep(Z, Y).

%Problem 2

condition(X):- X.
consequent(X, W) :- W  = X.
alternative(X, W):- W = X.
conditional(X, Y, Z, V, W) :- condition(X), consequent(Y,V), alternative(Z,W).











