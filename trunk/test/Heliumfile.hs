
test = f 4;

f = \x -> (x^2+2*x+(3+x)*(2+x)*1%(x+1)^2)%2;

x = let local = 1+2+3+4; h = 1; in if True then local + h else 0;
