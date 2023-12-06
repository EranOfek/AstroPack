function Result = getTestWorkDir()
    %
    % Eaxmple: DataTestWorkDir = tools.os.getTestDataDir
    Result = fullfile(tools.os.getTempDir(), 'test');
    if ~isfolder(Result)
        mkdir(Result);
    end
end
