% https://www.mathworks.com/help/matlab/matlab_oop/comparing-handle-and-value-classes.html

%
a = NumValue;
b = NumValue;
isequal(a,b)

%
a = NumValue;
b = NumValue;
b.Number = 7;
isequal(a,b)

%
a = NumHandle;
b = a;
a == b
isequal(a,b)

%
a = NumHandle;
b = NumHandle;
a == b
isequal(a,b)

