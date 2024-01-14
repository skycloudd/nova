const n = 5;

let x = 1;

for i in 2..=n do
    x = x * i;
end

builtin_print__ x;
