function Result = unitTest()
    % DbRecord.unitTest   
    io.msgStyle(LogLevel.Test, '@start', 'DbRecord test started');
    
    %------------------- Test Constructor and convert2 with TEXT data

    % Construct from struct with TEXT data
    Stru = struct;
    for i=1:4    
        Stru(i).Field1 = sprintf('Row_%02d_Field1', i);
        Stru(i).Field2 = sprintf('Row_%02d_Field2', i);
        Stru(i).Field3 = sprintf('Row_%02d_Field3', i);
    end
    R = db.DbRecord(Stru);
    for i=1:numel(Stru)
        assert(strcmp(Stru(i).Field1, R.Data(i).Field1));
        assert(strcmp(Stru(i).Field2, R.Data(i).Field2));
        assert(strcmp(Stru(i).Field3, R.Data(i).Field3));
    end

    % Convert to table
    Tab = R.convert2table();
    for i=1:numel(R.Data)
        assert(strcmp(Tab(i, 'Field1').Field1{1}, R.Data(i).Field1));
        assert(strcmp(Tab(i, 'Field2').Field2{1}, R.Data(i).Field2));
        assert(strcmp(Tab(i, 'Field3').Field3{1}, R.Data(i).Field3));
    end
    
   % convert2cell
    Cel = R.convert2cell();
    for i=1:numel(R.Data)
        assert(strcmp(Cel{i, 1}, R.Data(i).Field1));       
        assert(strcmp(Cel{i, 2}, R.Data(i).Field2));
        assert(strcmp(Cel{i, 3}, R.Data(i).Field3));
    end    
    
    % Construct from table
    R = db.DbRecord(Tab);
    for i=1:numel(R.Data)
        assert(strcmp(Tab(i, ':').Field1{1}, R.Data(i).Field1));
        assert(strcmp(Tab(i, ':').Field2{1}, R.Data(i).Field2));
        assert(strcmp(Tab(i, ':').Field3{1}, R.Data(i).Field3));
    end

    % Construct from cell
    R = db.DbRecord(Cel, 'ColNames', {'Field1', 'Field2', 'Field3'});
    for i=1:numel(R.Data)
        assert(strcmp(Cel{i, 1}, R.Data(i).Field1));        
        assert(strcmp(Cel{i, 2}, R.Data(i).Field2));        
        assert(strcmp(Cel{i, 3}, R.Data(i).Field3));                
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
        assert(Mat(i, 1) == R.Data(i).Double1);
        assert(Mat(i, 2) == R.Data(i).Double2);
        assert(Mat(i, 3) == R.Data(i).Double3);
    end    
    
    % Construct from Matrix
    R = db.DbRecord(Mat, 'ColNames', {'Double1', 'Double2', 'Double3'});
    for i=1:numel(R.Data)
        assert(Mat(i, 1) == R.Data(i).Double1);
        assert(Mat(i, 2) == R.Data(i).Double2);
        assert(Mat(i, 3) == R.Data(i).Double3);
    end    
  
    %------------------------------------- Construct/Convert AstroTable   
    % convert2AstroTable @Todo: better testing
    % @Eran, how should it work with multi-records
    AstTab = R.convert2AstroTable();
    assert(~isempty(AstTab.Catalog));    
    
    % Construct from AstroTable
    Q = db.DbRecord(AstTab);
    Sz = size(AstTab.Catalog);
    assert(Sz(1) == numel(Q.Data));
    assert(Sz(2) == numel(fieldnames(Q.Data(1))));
    
    %------------------------------------- Construct/Convert AstroCatalog
    % convert2AstroCatalog @Todo: better testing
    AstCat = R.convert2AstroCatalog();
    assert(~isempty(AstCat.Catalog));
    
    % Construct from AstroCatalog
    Q = db.DbRecord(AstCat);
    Sz = size(AstTab.Catalog);
    assert(Sz(1) == numel(Q.Data));
    assert(Sz(2) == numel(fieldnames(Q.Data(1))));
    
    %-------------------------------------- Merge 
    % Merge struct into existing DbRecord
    Stru = struct;
    for i=1:4    
        Stru(i).Field1 = sprintf('Row_%02d_Field1', i);
        Stru(i).Field2 = sprintf('Row_%02d_Field2', i);
    end
    R = db.DbRecord(Mat, 'ColNames', {'Double1', 'Double2', 'Double3'});    
    R.merge(Stru);
    for i=1:numel(R.Data)
        assert(R.Data(i).Double1 == Mat(i, 1));
        assert(R.Data(i).Double2 == Mat(i, 2));
        assert(strcmp(R.Data(i).Field1, Stru(i).Field1));
        assert(strcmp(R.Data(i).Field2, Stru(i).Field2));        
    end    

    
    % writeCsv
    CsvFileName = fullfile(tools.os.getTempDir(), 'dbrecord_csv1.csv');
    R.writeCsv(CsvFileName);

    % readCSv
    R2 = db.DbRecord();
    R2.readCsv(CsvFileName);
    assert(all(size(R.Data) == size(R2.Data)));
    
    % Constructor from CSV
    R3 = db.DbRecord(CsvFileName);
    assert(all(size(R3.Data) == size(R.Data)));
    
    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'DbRecord test passed');
    Result = true;
end
