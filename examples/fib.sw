func print |a|
    builtin_print__ a;
end

func fib |n|
    if n < 2 then
        return n;
    else
        return fib(n - 1) + fib(n - 2);
    end
end

func for |n, until, fn|
    fn(n);

    if n < until then
        for(n + 1, until, fn);
    end
end

for(0, 10, |n| print(fib(n)));
