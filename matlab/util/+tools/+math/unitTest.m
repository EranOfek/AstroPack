function Result = unitTest()
    % unitTest for tools.math
    % Example: tools.math.unitTest
    
    
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    %test_sincos();
    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

%--------------------------------------------------------------------------


function Result = test_sincos()
	% Function Unit-Test
	io.msgStyle(LogLevel.Test, '@start', 'test started');
    
%     randomAngles = rand(1, 10) * 2 * pi;
%     UseMex = 0;
%     UseMP = 0;    
%     [matlab_sin, matlab_cos] = tools.math.fun.sincos(randomAngles, UseMex, UseMP);
%    
%     UseMex = 1;    
%     [mex_sin, mex_cos] = tools.math.fun.sincos(randomAngles, UseMex, UseMP);
%    
%     assert(isequal(round(matlab_sin,10), round(mex_sin,10)));    
%     assert(isequal(round(matlab_cos,10), round(mex_cos,10)));
    
    
    iters = 100;
    
    for arr_sizes=1:4
        
        arr_size = power(10,arr_sizes);
        
        for var_types=6:6

            MatlabTimeTotal = 0;
            MexTimeTotal = 0;
            MexMPTimeTotal = 0;            
            MatlabTime = 0;
            MexTime = 0;
            MexMPTime = 0;

            for iter=1:iters

            randomAngles = rand(1, arr_size) * 2 * pi;
               
                switch var_types
                    case 1
                        randomAngles = int8(randomAngles);
                        var_name = 'int8';
                    case 2
                        randomAngles = int16(randomAngles);                        
                        var_name = 'int16';
                    case 3
                        randomAngles = int32(randomAngles);
                        var_name = 'int32';
                    case 4
                        randomAngles = int64(randomAngles);
                        var_name = 'int64';
                    case 5
                        randomAngles = single(randomAngles);
                        var_name = 'single';
                    case 6
                        randomAngles = double(randomAngles);
                        var_name = 'double';
                end

                UseMex = 0;
                UseMP = 0;
                t = tic;
                [matlab_sin, matlab_cos] = tools.math.fun.sincos(randomAngles, UseMex, UseMP);
                MatlabTime = toc(t);
                MatlabTimeTotal = MatlabTimeTotal + MatlabTime;

                UseMex = 1;
                UseMP = 0;
                t = tic;
                [mex_sin, mex_cos] = tools.math.fun.sincos(randomAngles, UseMex, UseMP);                
                MexTime = toc(t);
                MexTimeTotal = MexTimeTotal + MexTime;

                UseMex = 1;
                UseMP = 1;
                t = tic;
                [mexmp_sin, mexmp_cos] = tools.math.fun.sincos(randomAngles, UseMex, UseMP);
                MexMPTime = toc(t);
                MexMPTimeTotal = MexMPTimeTotal + MexMPTime;                
                                        
                assert(isequal(round(matlab_sin,10), round(mex_sin,10)));    
                assert(isequal(round(matlab_cos,10), round(mex_cos,10)));
            end

            MatlabTime = MatlabTimeTotal / iters;
            MexTime = MexTimeTotal / iters;
            MexMPTime = MexMPTimeTotal / iters;

            fprintf('Array_size: %d, Var_type: %s, Matlab: %.6f, Mex: %.6f, MexMP: %.6f, Ratio: %0.2f, MP_Ratio: %0.2f\n', arr_size, var_name, MatlabTime, MexTime, MexMPTime, MatlabTime/MexTime, MatlabTime/MexMPTime);

        end
    end

        
	io.msgStyle(LogLevel.Test, '@passed', 'passed');
	Result = true;
end


%--------------------------------------------------------------------------

