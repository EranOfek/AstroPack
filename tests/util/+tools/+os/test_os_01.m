% Package Unit-Test

function Result = unitTest()
    % Package Unit-Test
    io.msgStyle(LogLevel.Test, '@start', 'test started');

    if isunix()
        [~, cmdOut] = system('lscpu');
        if contains(cmdOut, 'avx512')
            disp('AVX-512 is supported')
        else
            disp('AVX-512 is not supported')
        end
    else
        [~, cmdOut] = system('path\to\coreinfo.exe');
        if contains(cmdOut, 'AVX512*')
            disp('AVX-512 is supported')
        else
            disp('AVX-512 is not supported')
        end
    end

    mex_result = tools.os.mex.is_avx512_supported()

    io.msgStyle(LogLevel.Test, '@passed', 'test passed');
    Result = true;
end

