inc = \x -> x+1;

h = [(1,False),(2,True),(3,False)];

x = let local = 1+2+3+4; h = 1; in if True then local + h else 0;

g = case 5*7 of aaa -> 2+4; b -> 10;;

h1 = ((((((((((1))))))))));
h2 = (let x = 1*2+3;in ((4)+5*(6+7),8));
h2 = case x of a -> (((1)));;

test = f 3;

f = \x -> x^2+2*x+(3+x)*(2+x)*1%(x+1)^2;
