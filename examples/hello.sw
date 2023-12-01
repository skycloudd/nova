func test |a, b, c|
    loop
        print 1;
        break;
    end

    return a + b * c;
end

print test(1, 2, 3);
