% Package Unit-Test
%
% ### Requirements:
%
%
%

function unitTest
    % Package Unit-Test   
	io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    test_bit_or_and();
    test_bit_or_and_mex();
	
	io.msgStyle(LogLevel.Test, '@passed', 'test passed');
end

%--------------------------------------------------------------------------

function test_bit_or_and()

	% Input
    Array     = uint32([ 0x0001, 0x0002, 0x0004, 0x000A;...
                         0x0011, 0x0022, 0x0014, 0x0018 ]);
                 
    % Expected results
    ArrayOr   = uint32([ 0x0011, 0x0022, 0x0014, 0x001A ]);
    ArrayAnd  = uint32([ 0x0001, 0x0002, 0x0004, 0x0008 ]);
    
    % OR
    Or = tools.array.bitor_array(Array);
    disp(Or);
    assert(strcmp(class(Or), 'uint32'));
    assert(isequal(Or, ArrayOr));
    
    % AND
    And = tools.array.bitand_array(Array);
    disp(And);
    assert(strcmp(class(And), 'uint32'));
    assert(isequal(And, ArrayAnd));

	Result = true;
end


function Result = test_bit_or_and_mex()
    %
    io.msgLog(LogLevel.Test, 'tools.array unitTest sarted');
    Iters = 10000;
    
    % ------------------------------------------- OR
    % 2D - Compare MATLAB and MEX
    for Iter=1:Iters
        rows = int32(rand*100);
        cols = int32(rand*100);
        Array = int32(double(0xFFFFFFFF) * rand(rows, cols));
        for dim=1:2
            Output = tools.array.bitor_array(Array, dim, false);
            MexOutput = tools.array.bitor_array(Array, dim, true);
            assert(isequal(Output, MexOutput));   
        end
    end
       
    % 3D - Compare MATLAB and MEX
    for Iter=1:Iters
        rows = int32(rand*100);
        cols = int32(rand*100);
        deps = 2 + int32(rand*100);        
        Array = uint32(double(0xFFFFFFFF) * rand(rows, cols, deps));
        for dim=1:3
            Output = tools.array.bitor_array(Array, dim, false);
            MexOutput = tools.array.bitor_array(Array, dim, true);
            assert(isequal(Output, MexOutput));   
        end
    end    


    % ------------------------------------------- AND
    % 2D - Compare MATLAB and MEX
    for Iter=1:Iters
        rows = int32(rand*100);
        cols = int32(rand*100);
        Array = int32(double(0xFFFFFFFF) * rand(rows, cols));
        for dim=1:2
            Output = tools.array.bitand_array(Array, dim, false);
            MexOutput = tools.array.bitand_array(Array, dim, true);
            assert(isequal(Output, MexOutput));   
        end
    end
       
    % 3D - Compare MATLAB and MEX
    for Iter=1:Iters
        rows = int32(rand*100);
        cols = int32(rand*100);
        deps = 2 + int32(rand*100);        
        Array = uint32(double(0xFFFFFFFF) * rand(rows, cols, deps));
        for dim=1:3
            Output = tools.array.bitand_array(Array, dim, false);
            MexOutput = tools.array.bitand_array(Array, dim, true);
            assert(isequal(Output, MexOutput));   
        end
    end    
    
    io.msgStyle(LogLevel.Test, '@passed', 'tools.array test passed')
    Result = true;
end



