
Inv: inv toc < ( < "bla" , #1 > : ( < "bla" , #2 > : ( < "blaa" , #3 > : [] ) ) ) , ( "bla" : ( "bla" : ( "blaa" : [] ) ) ) >

Inv: inv mySnoc ( "a" : ("b" : ("c" : [] ) ) ) 

inc = \x -> x+1;

h = [(1,False),(2,True),(3,False)];

s = \f -> \g -> \x -> f x (g x);

x = let local = 1+2+3+4; h = 1; in if True then local + h else 0;

test = f 4;

f = \x -> x^2+2*x+(3+x)*(2+x)*1%(x+1)^2;


