const one = 1;
const six = 2 * 3;
const number = one + six;

builtin_print__ number == 7;

let x = one;

loop
    builtin_print__ x;

    x = x + 1;

    if x == number then
        break;
    end
end
