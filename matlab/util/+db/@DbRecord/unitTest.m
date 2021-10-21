function Result = unitTest()
    % DbRecord.unitTest   
    io.msgStyle(LogLevel.Test, '@start', 'DbRecord test started');

    % Load from struct
    R = db.DbRecord;
    for i = 1:4
        R.Data(i).Str1 = sprintf('Str1_%03d', i);
        R.Data(i).Str2 = sprintf('Str2_%03d', i);
        R.Data(i).Str3 = sprintf('Str3_%03d', i);
    end
    
    % Table
    Tab = R.convert2table();
    for i=1:numel(R.Data)
        assert(strcmp(Tab(i, 'Str1').Str1{1}, R.Data(i).Str1));
    end
    
    % Cell
    Cel = R.convert2cell();
    for i=1:numel(R.Data)
        assert(strcmp(Cel{1, i}, R.Data(i).Str1));        
    end    

    % From struct
    Stru = R.Data;
    R = db.DbRecord(Stru);
    for i=1:numel(R.Data)
        assert(strcmp(Stru(i).Str1, R.Data(i).Str1));
        assert(strcmp(Stru(i).Str2, R.Data(i).Str2));
        assert(strcmp(Stru(i).Str3, R.Data(i).Str3));
    end

    % From table
    R = db.DbRecord(Tab);
    for i=1:numel(R.Data)
        assert(strcmp(Tab(i, 'Str1').Str1{1}, R.Data(i).Str1));
        assert(strcmp(Tab(i, 'Str2').Str2{1}, R.Data(i).Str2));
        assert(strcmp(Tab(i, 'Str3').Str3{1}, R.Data(i).Str3));
    end

    % From cell
    R = db.DbRecord(Cel, 'ColNames', {'Str1', 'Str2', 'Str3'});
    for i=1:numel(R.Data)
        assert(strcmp(Cel{1, i}, R.Data(i).Str1));        
        assert(strcmp(Cel{2, i}, R.Data(i).Str2));        
        assert(strcmp(Cel{3, i}, R.Data(i).Str3));                
    end    
    
       
    % Select and load to matrix
    R = db.DbRecord;
    for i = 1:4
        R.Data(i).Double1 = 10*i + 1.1;
        R.Data(i).Double2 = 10*i + 1.2;        
        R.Data(i).Double3 = 10*i + 1.3;        
    end

    Mat = R.convert2mat();
    for i=1:numel(R.Data)
        assert(Mat(1, i) == R.Data(i).Double1);
        assert(Mat(2, i) == R.Data(i).Double2);
        assert(Mat(3, i) == R.Data(i).Double3);
    end    

    % 
    AstTab = R.convert2AstroTable();     
    AstCat = R.convert2AstroCatalog();
    
    R = db.DbRecord(Mat, 'ColNames', {'Double1', 'Double2', 'Double3'});
    for i=1:numel(R.Data)
        assert(Mat(1, i) == R.Data(i).Double1);
        assert(Mat(2, i) == R.Data(i).Double2);
        assert(Mat(3, i) == R.Data(i).Double3);
    end    

    
    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'DbRecord test passed');
    Result = true;
end
