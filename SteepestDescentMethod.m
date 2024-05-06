clc;
clear all;
format short

a=0
b=0

%syms x y a1;
%f= @(x,y) x*y;

f= @(x,y) 3*x^2-4*x*y+2*y^2+4*x+6

grad=@(x,y) [6*x-4*y+4 , -4*x+4*y]
for k=1:4
grad(a,b)
d=-grad(a,b)/norm(grad(a,b))

%fun=@(z)(a+z*d(1))*(b+z*d(2)) 
fun=@(z) 3*(a+z*d(1))^2-4*(a+z*d(1))*(b+z*d(2))+2*(b+z*d(2))^2+4*(a+z*d(1))+6;
x1 = fminbnd(fun,0,10000)
a=a+x1*d(1)
b=b+x1*d(2)
f(a,b)
end
