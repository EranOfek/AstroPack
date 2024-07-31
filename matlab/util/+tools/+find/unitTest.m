% Package Unit-Test
%
% ### Requirements:
%
%
%


function Result = unitTest()
    % Package Unit-Test   
	io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    % test_mfind_bin();
    
	io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end


function Result = test_mfind_bin()
    io.msgLog(LogLevel.Test, 'tools.find.mfind_bin test started');

    iters = 10;
    
    arr_size = power(10,3);
    
    for var_types=1:2

        MatlabTimeTotal = 0;
        MexTimeTotal = 0;
        MexMPTimeTotal = 0;            
        MatlabTime = 0;
        MexTime = 0;
        MexMPTime = 0;

        for iter=1:iters

            R = [NaN;NaN;rand(1e4,1);NaN;NaN];
            R = sort(R);
            X = rand(10,1)';

            switch var_types
                case 1
                    R = single(R);
                    X = single(X);
                    var_name = 'single';
                case 2
                    R = double(R);
                    X = double(X);
                    var_name = 'double';
            end

            UseMex = 0;
            UseMP = 0;
            t = tic;
            matlab_res = tools.find.mfind_bin(R,X,UseMex,UseMP);
            MatlabTime = toc(t);
            MatlabTimeTotal = MatlabTimeTotal + MatlabTime;

            UseMex = 1;
            UseMP = 0;
            t = tic;
            mex_res = tools.find.mfind_bin(R,X,UseMex,UseMP);
            MexTime = toc(t);
            MexTimeTotal = MexTimeTotal + MexTime;

            UseMex = 1;
            UseMP = 1;
            t = tic;
            mex_mp_res = tools.find.mfind_bin(R,X,UseMex,UseMP);
            MexMPTime = toc(t);
            MexMPTimeTotal = MexMPTimeTotal + MexMPTime;                
                                    
            assert(isequal(matlab_res, mex_res));
        end

        MatlabTime = MatlabTimeTotal / iters;
        MexTime = MexTimeTotal / iters;
        MexMPTime = MexMPTimeTotal / iters;

        fprintf('Var_type: %s, Matlab: %.6fs, Mex: %.6fs, MexMP: %.6fs\n', var_name, MatlabTime, MexTime, MexMPTime);

    end
    
    io.msgStyle(LogLevel.Test, '@passed', 'tools.find.mfind_bin passed');
    Result = true;    
end   

