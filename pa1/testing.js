let z = 2;
let f = function f(x) {return f(x + 1);} 
let h = f;
f = function(x) {z++; return x+1;}
let y = h(4)*z;
console.log(y);
console.log(h(4)*z);