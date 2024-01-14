let nth = 0;

loop
    let n = nth;

    let a = 0;
    let b = 1;

    loop
        if n == 0 then
            break;
        end

        let c = a + b;
        a = b;
        b = c;

        n = n - 1;
    end

    builtin_print__ a;

    nth = nth + 1;
end
