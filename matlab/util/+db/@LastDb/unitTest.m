
function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains

    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug, 'type', 'all');
    io.msgStyle(LogLevel.Test, '@start', 'LastDb test started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');

    H = AstroHeader();
    FileName = 'c:/ultrasat/last/LAST.01.08.04_20230125.192423.674_clear_143+41_010_001_001_sci_raw_Image_1.txt';
    
    H = AstroHeaderFromTextFile(FileName);
    
    fid = fopen(FileName, 'rt');
    while true
        Line = fgetl(fid);
        
        % End of file
        if ~ischar(Line) 
            break; 
        end
        %fprintf('%s\n', Line);
        
        S = split(Line, '=');
        if numel(S) > 1
            Key = strip(S{1});        
            W = split(S{2}, '/');
            Value = strip(W{1});       
            fprintf('%s = %s\n', Key, Value);
        end
    end
    fclose(fid);
  
  
    H.insertKey({'FInt1',7777,'CommentA'; 'FInt2',2,'CommentB'}, 'end-1');
    H.insertKey({'FIntXX','XX','CommentXX'}, 'end-1');
    
    
    % Create object with default connection parameters
    LDB = db.LastDb();
    LDB.createTables();

    FileName = 'c:/ultrasat/last/LAST.01.08.04_20230125.192423.674_clear_143+41_010_001_001_sci_raw_Image_1.fits';
    AH = AstroHeader(FileName);
    LDB.addRawImage(FileName, AH);

    io.msgStyle(LogLevel.Test, '@passed', 'LastDb test passed')
    Result = true;
end


function Result = AstroHeaderFromTextFile(FileName)

    H = AstroHeader();
    fid = fopen(FileName, 'rt');
    while true
        Line = fgetl(fid);
        
        % End of file
        if ~ischar(Line) 
            break; 
        end
        %fprintf('%s\n', Line);
        
        S = split(Line, '=');
        if numel(S) > 1
            Key = strip(S{1});        
            W = split(S{2}, '/');
            Value = strip(W{1});
            Comment = '';
            if numel(W) > 1
                Comment = strip(W{2});
            end
            fprintf('%s = %s --- %s\n', Key, Value, Comment);
            
            Num = str2num(Value);
            if ~isempty(Num)
                Value = Num;
            end
            
            H.insertKey({Key, Value, Comment'}, 'end');
        end
    end
    fclose(fid);

    Result = H;
end

