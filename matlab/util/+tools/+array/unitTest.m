function Result = unitTest
    % Package Unit-Test
	io.msgStyle(LogLevel.Test, '@start', 'tools.array test started');   
    test_bitset();
    %test_countVal();
    return; 
    test_bit_or();
    test_bit_or_and();
    test_bit_or_and_mex();	
	io.msgStyle(LogLevel.Test, '@passed', 'tools.array test passed');
	Result = true;
end

function Result = test_bitset()
    %
    io.msgLog(LogLevel.Test, 'tools.array.test_bitset test started');

    %
    % Windows: 
    %
    %   mex  mex_bitsetFlag_int32.cpp  COMPFLAGS="$COMPFLAGS /openmp"
    %
    % Linux:
    %
    %   mex mex_bitsetFlag_int32.cpp CXXFLAGS='$CXXFLAGS -fopenmp' LDFLAGS='$LDFLAGS -fopenmp'
    %
	
	
    % -------------------------------------------    
    A=randi(1700,1700,'int32'); 
    F=rand(1700,1700) > 0.9;
    A(F) = bitset(A(F), 1, true);

    array = zeros(3, 3, 'int32');
    flag = false(3, 3);
    flag(1,1) = true;
    array = bitset(array, 5, true);
    %disp(array);    
    %array(flag) = bitset(array(flag), 2, true);    
    %disp(array);    
    % 
    %b = tools.array.mex.mex_bitsetFlag_int32(array, flag, int32(2), int32(false));
    %disp(array);
    %return;
    
    % -------------------------------------------    
    Iters = 10;
    Loop = 1;    
    Rows = 100;
    Cols = 100;
    Bit = 1;
    Value = 1;
      
    for SizeIter=1:10
      
        Array = zeros(Rows, Cols, 'int32');
        Flag = rand(Rows, Cols) > 0.9;
        Rows = Rows*2;
        Cols = Cols*2;
    
        fprintf('\n[%d] Array Size: %d MB\n', SizeIter, int32(numel(Array)*4 / 1024 / 1024));
        % -------------------------------------------
        for Iter=1:Iters

            % MATLAB version
            MatlabResult = Array;
            t = tic;
            for L=1:Loop
                MatlabResult(Flag) = bitset(Array(Flag), Bit, Value);
            end
            MatlabTime = toc(t);

            % MEX version
            t = tic;
            for L=1:Loop        
                MexResult = tools.array.mex.mex_bitsetFlag_int32(Array, Flag, int32(Bit), int32(Value), int32(false));
                %MexResult = tools.array.bitsetFlag(Array, Flag, Bit, Value);            
            end
            MexTime = toc(t);        
            
            % MEX with OpenMP
            t = tic;
            for L=1:Loop        
                MpResult = tools.array.mex.mex_bitsetFlag_int32(Array, Flag, int32(Bit), int32(Value), int32(true));
            end
            MpTime = toc(t);                    

            % MEX via bitsetFlag
            t = tic;
            for L=1:Loop        
                WrapperResult = tools.array.bitsetFlag(Array, Flag, Bit, Value);            
            end
            WrapperTime = toc(t);                    

            
            fprintf('Matlab: %.6f, Mex: %.6f, MexMP: %.6f, Wrapper: %0.6f, Ratio: %0.2f\n', MatlabTime, MexTime, MpTime, WrapperTime, MatlabTime/WrapperTime);
            %fprintf('isequal...\n');
            assert(isequal(MatlabResult, MexResult));               
            assert(isequal(MatlabResult, MpResult));                           
            assert(isequal(MatlabResult, WrapperResult));                           
        end
    end
    
    
    io.msgStyle(LogLevel.Test, '@passed', 'tools.array.test_bitset passed')
    Result = true;
end




%--------------------------------------------------------------------------
function test_bit_or()
    io.msgLog(LogLevel.Test, 'tools.array.test_bit_or test started');
    
    for Iter=1:5
        Array = uint32(randi(2^16,1600,1600,20));
        t = tic;
        Val = tools.array.bitor_array(Array,3,false);
        MatlabTime = toc(t);
        t = tic;
        ValMex = tools.array.bitor_array(Array,3,true);
        MexTime = toc(t);
        fprintf('Matlab: %.6f, Mex: %.6f\n', MatlabTime, MexTime);                    
        assert(isequal(Val, ValMex));
    end

    io.msgLog(LogLevel.Test, 'tools.array.test_bit_or test done');
    Result = true;    
end

%--------------------------------------------------------------------------

function Result = test_bit_or_and()
    io.msgLog(LogLevel.Test, 'tools.array.test_bit_or_and test started');
    
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
    io.msgLog(LogLevel.Test, 'tools.array.test_bit_or_and test done');    
end


function Result = test_bit_or_and_mex()
    %
    io.msgLog(LogLevel.Test, 'tools.array.test_bit_or_and_mex test started');
    Iters = 10;
    
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
    
    io.msgStyle(LogLevel.Test, '@passed', 'tools.array.test_bit_or_and_mex test passed')
    Result = true;
end

%--------------------------------------------------------------------------

function Result = test_countVal()
    %
    io.msgLog(LogLevel.Test, 'tools.array.test_countVal test started');
    Iters = 10;
    
    Array = zeros(10000, 10000, 'double');
    Val = 12345;
    Array(1) = Val;
    Array(10) = Val;
    Array(100) = Val;
    Array(1000) = Val;
    % -------------------------------------------
    for Iter=1:Iters
    
        t = tic;
        MatlabResult = sum(Array(:) == Val);
        MatlabTime = toc(t);
        
        t = tic;
        MexResult = tools.array.countVal(Array, Val);
        MexTime = toc(t);        
                
        fprintf('Matlab: %.6f, Mex: %.6f\n', MatlabTime, MexTime);
        assert(isequal(MatlabResult, MexResult));               
    end
           
    io.msgStyle(LogLevel.Test, '@passed', 'tools.array test passed')
    Result = true;
end

%--------------------------------------------------------------------------

