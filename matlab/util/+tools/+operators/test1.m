%
% codegen C:\Ultrasat\AstroPack.git\matlab\util\+tools\+operators\test1.m
%

function test1()
    Rows = 1000;
    Cols = 1000;
    A = double(rand(Rows, Cols));
    B = double(rand(Rows, Cols));                
    
    A = A .* B;
    if A(1,1) == 1
        fprintf('\n');
    end
    
    %R = isequal(A, B);
    %fprintf('Result: %d\n', int32(R));
end
