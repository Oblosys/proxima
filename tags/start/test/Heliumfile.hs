inc = \x -> x+1;

h = [(1,False),(2,True),(3,False)];

x = let local = 1+2+3+4; h = 1; in if True then inc 18 else 0;

g = case 5*7 of aaa -> 2+4; b -> 10;;

ttest = f 3;

f = \x -> x^2+2*x+(3+x)*(2+x)*1%(x+1)^2;
