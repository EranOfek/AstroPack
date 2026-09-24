function Result = unitTest()
    % Package Unit-Test   
    
    test_isempty_cell();
    
	Result = true;
end


function Result = test_isempty_cell()
    io.msgLog(LogLevel.Test, 'tools.cell.isempty_cell test started');
    
    Cell = {1, [], 'a', 123, [], 'asdf', 11, 1234, 12.23, [], 1, [], 'a', 123, [], 'asdf', 11, 1234, 12.23, []};

    iters = 100;

    MatlabTimeTotal = 0;
    MexTimeTotal = 0;
    MexMPTimeTotal = 0;

    for iter=1:iters
        t = tic;
        matlab_res = tools.cell.isempty_cell(Cell,false);
        MatlabTime = toc(t);
        MatlabTimeTotal = MatlabTimeTotal + MatlabTime;
    
        t = tic;
        mex_res = tools.cell.isempty_cell(Cell,true,false);
        MexTime = toc(t);
        MexTimeTotal = MexTimeTotal + MexTime;

        t = tic;
        mexmp_res = tools.cell.isempty_cell(Cell,true,true);
        MexMPTime = toc(t);
        MexMPTimeTotal = MexMPTimeTotal + MexMPTime;

        assert(isequal(matlab_res, mex_res));
        assert(isequal(mexmp_res, mex_res));
    end
    
    MatlabTime = MatlabTimeTotal / iters;
    MexTime = MexTimeTotal / iters;
    MexMPTime = MexMPTimeTotal / iters;

    % fprintf('Matlab: %.6f s, Mex: %.6f s, Mex MP: %.6f s  \n', MatlabTime, MexTime, MexMPTime);

    io.msgStyle(LogLevel.Test, '@passed', 'tools.cell.isempty_cell passed')
    Result = true;
end

