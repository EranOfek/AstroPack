function Result = unitTest
    % Package Unit-Test
    io.msgStyle(LogLevel.Test, '@start', 'tools.operators test started');

    test_times();

    io.msgStyle(LogLevel.Test, '@passed', 'tools.operators test passed');
    Result = true;
end


function Result = test_times()
    %
    io.msgLog(LogLevel.Test, 'tools.operators.times test started');

    A = [1 2; 3 4];
    B = [5 6; 7 8];

    C = A.*B;
    disp(C);

    tools.operators.times(A, B);
    disp(A);
    assert(isequal(A, C));
    
    ExpectedC = [5 12; 21 32];

    % -------------------------------------------
    Iters = 10;
    Loop = 1;
    Rows = 100;
    Cols = 100;

    for SizeIter=1:10

        A = rand(Rows, Cols, 'int32');
        B = rand(Rows, Cols, 'int32');
        Rows = Rows*2;
        Cols = Cols*2;

        fprintf('\n[%d] Array Size: %d MB\n', SizeIter, int32(numel(A)*4 / 1024 / 1024));
        % -------------------------------------------
        for Iter=1:Iters

            % MATLAB version
            MatlabResult = Array;
            t = tic;
            for L=1:Loop
                MatlabResult = A .* B;
            end
            MatlabTime = toc(t);

            % MEX version
            t = tic;
            for L=1:Loop
                MexResult = tools.operators.mex.mex_times32(A, B, int32(true));
            end
            MexTime = toc(t);

            % MEX with OpenMP
            t = tic;
            for L=1:Loop
                MpResult = tools.operators.mex.mex_times32(A, B, (true), int32(true));
            end
            MpTime = toc(t);

            % MEX via times
            t = tic;
            for L=1:Loop
                WrapperResult = tools.operators.mex.mex_times32(A, B);
            end
            WrapperTime = toc(t);


            fprintf('Matlab: %.6f, Mex: %.6f, MexMP: %.6f, Wrapper: %0.6f, Ratio: %0.2f\n', MatlabTime, MexTime, MpTime, WrapperTime, MatlabTime/WrapperTime);
            assert(isequal(MatlabResult, MexResult));
            assert(isequal(MatlabResult, MpResult));
            assert(isequal(MatlabResult, WrapperResult));
        end
    end

    io.msgStyle(LogLevel.Test, '@passed', 'tools.operators.times passed')
    Result = true;
end
