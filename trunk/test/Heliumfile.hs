inc = \x -> x+1;
val = \num -> (num inc) 0; 
test = val ((add one) one);

zero = \s -> \z -> z;
one =  \s -> \z -> s z;
two =  \s -> \z -> s (s z);
three =  \s -> \z -> s ((two s) z);

add = \n -> \m -> \s -> \z -> (n s) ((m s) z);
mult = \n -> \m -> \s -> \z -> (n (m s)) z;
y = \h -> (\x -> h (x x)) (\x -> h (x x));