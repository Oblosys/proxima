h = [(1,False),(2,True),(3,False)];

increase = \x -> x + 1;

s = \f -> \g -> \x -> f x (g x);

test = f True;

c = 1;

x = let local = 1+2+3+4; h = 1; in if True then local + h else 0;

f = \x -> x^2+2*x+(3+x)*(2+x)*1%(x+1)^2;

