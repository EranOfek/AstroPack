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

    % Simple test to compare results
    A = [1 2; 3 4];
    B = [5 6; 7 8];
    C = A.*B;
    disp(C);
    tools.operators.times(A, B, true, true, true);
    disp(A);
    assert(isequal(A, C));       

    % -------------------------------------------
    for UseMP=0:1
        for UseAVX=0:0
            for TypeIter=4:4    
                fprintf('\n');
                Rows = 16384;  %512;
                Cols = 16384;  %512;
                for SizeIter=1:1
                    clear A;
                    clear B;
                    clear MatlabResult;
                    if TypeIter == 1
                        Type = 'int32';
                        A = int32(randi(1000, Rows, Cols));
                        B = int32(randi(1000, Rows, Cols));
                    elseif TypeIter == 2
                        Type = 'int64';
                        A = int64(randi(1000000, Rows, Cols));
                        B = int64(randi(1000000, Rows, Cols));
                    elseif TypeIter == 3
                        Type = 'single';            
                        A = single(rand(Rows, Cols));
                        B = single(rand(Rows, Cols));                
                    else
                        Type = 'double';            
                        A = double(rand(Rows, Cols));
                        B = double(rand(Rows, Cols));                
                    end

                    % MATLAB version - Note that it handles int overflow
                    t = tic;
                    %profile on;
                    MatlabResult = A .* B;
                    %profile off;
                    MatlabTime = toc(t);

                    for Iter=1:50
                    % MEX version with/without OpenMP
                    t = tic;
                    %tools.operators.times(A, B, true, UseMP, UseAVX);
                    if UseAVX
                        tools.operators.mex.mex_timesDouble_avx2(A, B, int32(UseMP));
                    else
                        tools.operators.mex.mex_timesDouble(A, B, int32(UseMP));
                    end
                    MexTime = toc(t);
                    fprintf('%s - Array Size: %3d M items - Matlab: %.6f --- OpenMP: %d, AVX: %d, Mex: %.6f, Ratio: %0.2f\n', Type, int32(numel(A) / 1024 / 1024), MatlabTime, UseMP, UseAVX, MexTime, MatlabTime/MexTime);
                    end
                    %fprintf('%s - Array Size: %3d M items - Matlab: %.6f --- OpenMP: %d, AVX: %d, Mex: %.6f, Ratio: %0.2f\n', Type, int32(numel(A) / 1024 / 1024), MatlabTime, UseMP, UseAVX, MexTime, MatlabTime/MexTime);
                    if ~isequal(MatlabResult, A)
                        %fprintf('NOT EQUAL\n');
                    end

                    %assert(isequal(MatlabResult, A));
                
                    Rows = int32(Rows*2);
                    Cols = int32(Cols*2); 
                end
            end
        end
    end
    
    io.msgStyle(LogLevel.Test, '@passed', 'tools.operators.times passed')
    Result = true;
end
