
function m = test1(n)

    if ischar(n)
        n=str2double(n);
    end

    m = magic(n);
    disp(m)

end

