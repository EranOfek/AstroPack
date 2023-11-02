function Result = unitTest
    % Package Unit-Test
	io.msgStyle(LogLevel.Test, '@start', 'tools.array test started');
    
    test_onesExcept();

    test_onesCondition();

    test_init();
    
%     test_bitset();
    %test_countVal();
%     return; 
%     test_bit_or();
%     test_bit_or_and();
%     test_bit_or_and_mex();	
	io.msgStyle(LogLevel.Test, '@passed', 'tools.array test passed');
	Result = true;
end

function Result = test_onesExcept()
    io.msgLog(LogLevel.Test, 'tools.array.onesexcept test started');
    
    % Checking basic functionality and comparing mex and matlab
    UseMex = 0;
    UseMP = 0;
%     mat = [3 6 9];
%     mat = [3 6 9; 4 7 11];
    mat = rand(5,4,4)*10;
    scalar = 5;
    image = true;
    matlab_res = tools.array.onesExcept(mat, scalar, image, UseMex, UseMP);
    
    UseMex = 1;
    mex_res = tools.array.onesExcept(mat, scalar, image, UseMex, UseMP);
    
    assert(isequal(matlab_res, mex_res));
    
    
    iters = 50;
    
    for arr_sizes=1:3
        
        arr_size = power(10,arr_sizes);
        
        for var_types=1:6

            MatlabTimeTotal = 0;
            MexTimeTotal = 0;
            MexMPTimeTotal = 0;            
            MatlabTime = 0;
            MexTime = 0;
            MexMPTime = 0;

            for iter=1:iters

                image = 1;
                mat = rand(arr_size)*1000;
                scalar = rand(1)*1000;        

                switch var_types
                    case 1
                        mat = int8(mat);
                        scalar = int8(scalar);
                        var_name = 'int8';
                    case 2
                        mat = int16(mat);
                        scalar = int16(scalar);
                        var_name = 'int16';
                    case 3
                        mat = int32(mat);
                        scalar = int32(scalar);
                        var_name = 'int32';
                    case 4
                        mat = int64(mat);
                        scalar = int64(scalar);
                        var_name = 'int64';
                    case 5
                        mat = single(mat);
                        scalar = single(scalar);
                        var_name = 'single';
                    case 6
                        mat = double(mat);
                        scalar = double(scalar);
                        var_name = 'double';
                end

                UseMex = 0;
                UseMP = 0;
                t = tic;
                matlab_res = tools.array.onesExcept(mat, scalar, image, UseMex, UseMP);        
                MatlabTime = toc(t);
                MatlabTimeTotal = MatlabTimeTotal + MatlabTime;

                UseMex = 1;
                UseMP = 0;
                t = tic;
                mex_res = tools.array.onesExcept(mat, scalar, image, UseMex, UseMP);
                MexTime = toc(t);
                MexTimeTotal = MexTimeTotal + MexTime;

                UseMex = 1;
                UseMP = 1;
                t = tic;
                mex_mp_res = tools.array.onesExcept(mat, scalar, image, UseMex, UseMP);
                MexMPTime = toc(t);
                MexMPTimeTotal = MexMPTimeTotal + MexMPTime;                
                                        
                assert(isequal(matlab_res, mex_res));
            end

            MatlabTime = MatlabTimeTotal / iters;
            MexTime = MexTimeTotal / iters;
            MexMPTime = MexMPTimeTotal / iters;

            fprintf('Array_size: %d, Var_type: %s, Matlab: %.6f, Mex: %.6f, MexMP: %.6f, Ratio: %0.2f, MP_Ratio: %0.2f\n', arr_size, var_name, MatlabTime, MexTime, MexMPTime, MatlabTime/MexTime, MatlabTime/MexMPTime);

        end
    end
    
    io.msgStyle(LogLevel.Test, '@passed', 'tools.array.onesExcept passed')
    Result = true;    

end


function Result = test_onesCondition()
    io.msgLog(LogLevel.Test, 'tools.array.onesCondition test started');
    
    % Checking basic functionality and comparing mex and matlab
    UseMex = 0;
    UseMP = 0;
    MatR2 = rand(10,10,'double');
    MomRadius2 = 0.5;   
    matlab_res = tools.array.onesCondition(MatR2,MomRadius2,'double',UseMex,false);    
    UseMex = 1;
    mex_res = tools.array.onesCondition(MatR2,MomRadius2,'double',UseMex,false);
    
    assert(isequal(matlab_res, mex_res));
    
    
    iters = 10;
    
    for arr_sizes=1:2
        
        if arr_sizes == 1
            arr_size = "1700x1700";
        elseif arr_sizes == 2
            arr_size = "25x25x1000";
        end

        for var_types=1:2

            MatlabTimeTotal = 0;
            MexTimeTotal = 0;
            MexMPTimeTotal = 0;            
            MatlabTime = 0;
            MexTime = 0;
            MexMPTime = 0;

            for iter=1:iters
                
                MomRadius2 = 0.5;

                switch var_types
                    case 1
                        if arr_sizes == 1
                            MatR2 = rand(1700,1700,'single');
                        elseif arr_sizes == 2
                            MatR2 = rand(25,25,1000,'single');
                        end
                        Type = 'single';
                    case 2
                        if arr_sizes == 1
                            MatR2 = rand(1700,1700,'double');
                        elseif arr_sizes == 2
                            MatR2 = rand(25,25,1000,'double');
                        end
                        Type = 'double';                    
                end

                UseMex = 0;
                UseMP = 0;
                t = tic;
                matlab_res = tools.array.onesCondition(MatR2,MomRadius2,Type,UseMex,UseMP);        
                MatlabTime = toc(t);
                MatlabTimeTotal = MatlabTimeTotal + MatlabTime;

                UseMex = 1;
                UseMP = 0;
                t = tic;
                mex_res = tools.array.onesCondition(MatR2,MomRadius2,Type,UseMex,UseMP);        
                MexTime = toc(t);
                MexTimeTotal = MexTimeTotal + MexTime;

                UseMex = 1;
                UseMP = 1;
                t = tic;
                mex_res = tools.array.onesCondition(MatR2,MomRadius2,Type,UseMex,UseMP);        
                MexMPTime = toc(t);
                MexMPTimeTotal = MexMPTimeTotal + MexMPTime;                
                                        
                assert(isequal(matlab_res, mex_res));
            end

            MatlabTime = MatlabTimeTotal / iters;
            MexTime = MexTimeTotal / iters;
            MexMPTime = MexMPTimeTotal / iters;

            fprintf('Array_size: %s, Var_type: %s, Matlab: %.6f, Mex: %.6f, MexMP: %.6f, Ratio: %0.2f, MP_Ratio: %0.2f\n', arr_size, Type, MatlabTime, MexTime, MexMPTime, (MexTime/MatlabTime)*100, (MexMPTime/MatlabTime)*100);

        end
    end
    
    io.msgStyle(LogLevel.Test, '@passed', 'tools.array.onesCondition passed')
    Result = true;    

end

function Result = test_init()
    io.msgLog(LogLevel.Test, 'tools.array.init test started');
    
    % Checking basic functionality and comparing mex and matlab
    UseMex = 0;
    UseMP = 0;
    Size = [3];
    Val = 5;
    Type = "int64";

    matlab_res = tools.array.init(Size, Val, Type, UseMex, UseMP);
    
    UseMex = 1;
    mex_res = tools.array.init(Size, Val, Type, UseMex, UseMP);
    
    assert(isequal(matlab_res, mex_res));
    
    
    iters = 1;
        
            for var_types=1:7
    
                MatlabTimeTotal_1d = 0;
                MexTimeTotal_1d = 0;
                MexMPTimeTotal_1d = 0;            
                MatlabTime_1d = 0;
                MexTime_1d = 0;
                MexMPTime_1d = 0;

                MatlabTimeTotal_2d = 0;
                MexTimeTotal_2d = 0;
                MexMPTimeTotal_2d = 0;            
                MatlabTime_2d = 0;
                MexTime_2d = 0;
                MexMPTime_2d = 0;

                MatlabTimeTotal_3d = 0;
                MexTimeTotal_3d = 0;
                MexMPTimeTotal_3d = 0;            
                MatlabTime_3d = 0;
                MexTime_3d = 0;
                MexMPTime_3d = 0;

                for iter=1:iters
    
                    Size_1d = [2000];
                    Size_2d = [2000 2000];
                    Size_3d = [200 200 200];
                    Val = 5;
                                                
                    switch var_types
                        case 1
                            var_name = 'int8';
                        case 2
                            var_name = 'int16';
                        case 3
                            var_name = 'int32';
                        case 4
                            var_name = 'int64';
                        case 5
                            var_name = 'single';
                        case 6
                            var_name = 'double';
                        case 7
                            var_name = 'logical';
                    end
    
                    UseMex = 0;
                    UseMP = 0;
                    t = tic;
                    matlab_res_1d = tools.array.init(Size_1d, Val, Type, UseMex, UseMP);
                    MatlabTime_1d = toc(t);
                    MatlabTimeTotal_1d = MatlabTimeTotal_1d + MatlabTime_1d;
                    t = tic;
                    matlab_res_2d = tools.array.init(Size_2d, Val, Type, UseMex, UseMP);
                    MatlabTime_2d = toc(t);
                    MatlabTimeTotal_2d = MatlabTimeTotal_2d + MatlabTime_2d;
                    t = tic;
                    matlab_res_3d = tools.array.init(Size_3d, Val, Type, UseMex, UseMP);
                    MatlabTime_3d = toc(t);
                    MatlabTimeTotal_3d = MatlabTimeTotal_3d + MatlabTime_3d;
    
                    UseMex = 1;
                    UseMP = 0;
                    t = tic;
                    mex_res_1d = tools.array.init(Size_1d, Val, Type, UseMex, UseMP);
                    MexTime_1d = toc(t);
                    MexTimeTotal_1d = MexTimeTotal_1d + MexTime_1d;
                    t = tic;
                    mex_res_2d = tools.array.init(Size_2d, Val, Type, UseMex, UseMP);
                    MexTime_2d = toc(t);
                    MexTimeTotal_2d = MexTimeTotal_2d + MexTime_2d;
                    t = tic;
                    mex_res_3d = tools.array.init(Size_3d, Val, Type, UseMex, UseMP);
                    MexTime_3d = toc(t);
                    MexTimeTotal_3d = MexTimeTotal_3d + MexTime_3d;    
                    
                    UseMex = 1;
                    UseMP = 1;
                    t = tic;
                    mexMP_res_1d = tools.array.init(Size_1d, Val, Type, UseMex, UseMP);
                    MexMPTime_1d = toc(t);
                    MexMPTimeTotal_1d = MexMPTimeTotal_1d + MexMPTime_1d;
                    t = tic;
                    mexMP_res_2d = tools.array.init(Size_2d, Val, Type, UseMex, UseMP);
                    MexMPTime_2d = toc(t);
                    MexMPTimeTotal_2d = MexMPTimeTotal_2d + MexMPTime_2d;
                    t = tic;
                    mexMP_res_3d = tools.array.init(Size_3d, Val, Type, UseMex, UseMP);
                    MexMPTime_3d = toc(t);
                    MexMPTimeTotal_3d = MexMPTimeTotal_3d + MexMPTime_3d;
                                            
                    assert(isequal(matlab_res_1d, mex_res_1d));
                    assert(isequal(matlab_res_2d, mex_res_2d));
                    assert(isequal(matlab_res_3d, mex_res_3d));
                end
    
                MatlabTime_1d = MatlabTimeTotal_1d / iters;
                MexTime_1d = MexTimeTotal_1d / iters;
                MexMPTime_1d = MexMPTimeTotal_1d / iters;
                MatlabTime_2d = MatlabTimeTotal_2d / iters;
                MexTime_2d = MexTimeTotal_2d / iters;
                MexMPTime_2d = MexMPTimeTotal_2d / iters;
                MatlabTime_3d = MatlabTimeTotal_3d / iters;
                MexTime_3d = MexTimeTotal_3d / iters;
                MexMPTime_3d = MexMPTimeTotal_3d / iters;

                fprintf('Var_type: %s, Matlab 1D: %.6f, Mex 1D: %.6f, MexMP 1D: %.6f, Ratio 1D: %0.2f%%, MP_Ratio 1D: %0.2f%%\n', var_name, MatlabTime_1d, MexTime_1d, MexMPTime_1d, (MexTime_1d/MatlabTime_1d)*100, (MexMPTime_1d/MatlabTime_1d)*100);
                fprintf('Var_type: %s, Matlab 2D: %.6f, Mex 2D: %.6f, MexMP 2D: %.6f, Ratio 2D: %0.2f%%, MP_Ratio 2D: %0.2f%%\n', var_name, MatlabTime_2d, MexTime_2d, MexMPTime_2d, (MexTime_2d/MatlabTime_2d)*100, (MexMPTime_2d/MatlabTime_2d)*100);
                fprintf('Var_type: %s, Matlab 3D: %.6f, Mex 3D: %.6f, MexMP 3D: %.6f, Ratio 3D: %0.2f%%, MP_Ratio 3D: %0.2f%%\n', var_name, MatlabTime_3d, MexTime_3d, MexMPTime_3d, (MexTime_3d/MatlabTime_3d)*100, (MexMPTime_3d/MatlabTime_3d)*100);

    
            end

    io.msgStyle(LogLevel.Test, '@passed', 'tools.array.onesExcept passed')
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

