function Result = unitTest()
    % DbRecord.unitTest   
    io.msgStyle(LogLevel.Test, '@start', 'DbRecord test started');
    
    %------------------- Test Constructor and convert2 with TEXT data

    % Construct from struct with TEXT data
    Stru = struct;
    for i=1:4    
        Stru(i).Str1 = sprintf('Str1_%03d', i);
        Stru(i).Str2 = sprintf('Str2_%03d', i);
        Stru(i).Str3 = sprintf('Str3_%03d', i);
    end
    R = db.DbRecord(Stru);
    for i=1:numel(Stru)
        assert(strcmp(Stru(i).Str1, R.Data(i).Str1));
        assert(strcmp(Stru(i).Str2, R.Data(i).Str2));
        assert(strcmp(Stru(i).Str3, R.Data(i).Str3));
    end

    % Convert to table
    Tab = R.convert2table();
    for i=1:numel(R.Data)
        assert(strcmp(Tab(i, 'Str1').Str1{1}, R.Data(i).Str1));
        assert(strcmp(Tab(i, 'Str2').Str2{1}, R.Data(i).Str2));
        assert(strcmp(Tab(i, 'Str3').Str3{1}, R.Data(i).Str3));
    end
    
   % convert2cell
    Cel = R.convert2cell();
    for i=1:numel(R.Data)
        assert(strcmp(Cel{1, i}, R.Data(i).Str1));        
        assert(strcmp(Cel{2, i}, R.Data(i).Str2));
        assert(strcmp(Cel{3, i}, R.Data(i).Str3));
    end    
    
    % Construct from table
    R = db.DbRecord(Tab);
    for i=1:numel(R.Data)
        assert(strcmp(Tab(i, 'Str1').Str1{1}, R.Data(i).Str1));
        assert(strcmp(Tab(i, 'Str2').Str2{1}, R.Data(i).Str2));
        assert(strcmp(Tab(i, 'Str3').Str3{1}, R.Data(i).Str3));
    end

    % Construct from cell
    R = db.DbRecord(Cel, 'ColNames', {'Str1', 'Str2', 'Str3'});
    for i=1:numel(R.Data)
        assert(strcmp(Cel{1, i}, R.Data(i).Str1));        
        assert(strcmp(Cel{2, i}, R.Data(i).Str2));        
        assert(strcmp(Cel{3, i}, R.Data(i).Str3));                
    end    
           
    %------------------- Test Constructor and convert2 with NUMERIC data
    
    % Prepare record
    R = db.DbRecord;
    for i = 1:4
        R.Data(i).Double1 = 10*i + 1.1;
        R.Data(i).Double2 = 10*i + 1.2;        
        R.Data(i).Double3 = 10*i + 1.3;        
    end

    % Convert to Matrix
    Mat = R.convert2mat();
    for i=1:numel(R.Data)
        assert(Mat(1, i) == R.Data(i).Double1);
        assert(Mat(2, i) == R.Data(i).Double2);
        assert(Mat(3, i) == R.Data(i).Double3);
    end    
    
    % Construct from Matrix
    R = db.DbRecord(Mat, 'ColNames', {'Double1', 'Double2', 'Double3'});
    for i=1:numel(R.Data)
        assert(Mat(1, i) == R.Data(i).Double1);
        assert(Mat(2, i) == R.Data(i).Double2);
        assert(Mat(3, i) == R.Data(i).Double3);
    end    
  
    %------------------------------------- Construct/Convert AstroTable   
    % convert2AstroTable @Todo: better testing
    % @Eran, how should it work with multi-records
    AstTab = R.convert2AstroTable();
    assert(~isempty(AstTab.Catalog));    
    
    %------------------------------------- Construct/Convert AstroCatalog
    % convert2AstroCatalog @Todo: better testing
    AstCat = R.convert2AstroTable();
    assert(~isempty(AstCat.Catalog));
    
    %-------------------------------------- Merge 
    % Merge
    Stru = struct;
    for i=1:4    
        Stru(i).Str1 = sprintf('Str1_%03d', i);
        Stru(i).Str2 = sprintf('Str2_%03d', i);
    end
    R = db.DbRecord(Mat, 'ColNames', {'Double1', 'Double2', 'Double3'});    
    R.merge(Stru);
    for i=1:numel(R.Data)
        assert(R.Data(i).Double1 == Mat(1, i));
        assert(R.Data(i).Double2 == Mat(2, i));
        assert(strcmp(R.Data(i).Str1, Stru(i).Str1));
        assert(strcmp(R.Data(i).Str2, Stru(i).Str2));        
    end    

    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'DbRecord test passed');
    Result = true;
end
