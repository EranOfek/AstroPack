% Test memory allocation of handle classes
%
% https://www.mathworks.com/help/matlab/matlab_oop/handle-class-destructors.html
%
%

fprintf('Test started\n');

allocTest1();
allocTest2();
allocTest3();

w = waitforbuttonpress;

fprintf('\nThe end.\n');


function allocTest1()

    H = HandleClass();
    H.Func();
    
end


function allocTest2()

    H = HandleClass();
    H.Func();
    
end


function allocTest3()

    H = HandleClass();
    H.Func();
    
end

