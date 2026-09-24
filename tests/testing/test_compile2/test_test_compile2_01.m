
function m = test2(n)

if ischar(n)
    n=str2double(n);
end

%disp('Test2');

m = magic(n);  %*2);
disp(m)

