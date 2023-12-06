
function Result = perfTest()
    % AstroDb.perfTest
    
    io.msgStyle(LogLevel.Test, '@start', 'AstroDb perfTest started')              

    Cols = 40;
    ColNames = { 'recid' };
    for i = 1:Cols
        if i < Cols
            ColNames{end+1} = sprintf('F%02d', i);  % = strcat(ColNames, 
        else
            ColNames{end+1} = sprintf('F%02d', i);  % = strcat(ColNames, 
        end
    end
    %ColNames = strip(ColNames);
    Cols = numel(ColNames);

    Rows = 10;           
    for Iter=1:5

        data = rand(Rows, Cols);

        % Create random table
        io.msgLog(LogLevel.Test, 'Preparing rand Catalog: Rows: %d, Cols: %d', Rows, Cols);                
        tic();
        AC = AstroTable({data}, 'ColNames', ColNames);
        T = toc();
        io.msgLog(LogLevel.Test, 'Preparing rand Catalog: Rows: %d, Cols: %d: = %.4f', Rows, Cols, T);

        %
        tic();
        FileName = sprintf('c:\\temp\\Cat-%d.csv', Rows);
        AC.csvWrite(FileName);
        T = toc();
        io.msgLog(LogLevel.Test, 'csvWrite: Rows: %d, Cols: %d = %.4f', Rows, Cols, T);

        Rows = Rows * 10;
    end

    io.msgStyle(LogLevel.Test, '@passed', 'AstroDb perfTest done')
    Result = true;            
end

