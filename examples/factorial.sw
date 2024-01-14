const n = 5;

let x = 1;

let i = 2;

loop
    if i <= n then
        x = x * i;

        i = i + 1;
    else
        break;
    end
end

builtin_print__ x;
