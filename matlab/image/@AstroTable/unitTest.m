function Result = unitTest()
    %To do:
    % -Add output checks for:
    %     -everything

    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);

    % Create an empty AstroTable
    %io.msgLog(LogLevel.Test, 'testing AstroTable constructor')
    AC = AstroTable;

    % Create four empty tables
    AC = AstroTable([2, 2]);

    % Create two tables with random data
    AC = AstroTable({rand(10,2),rand(10,2)});
    AC = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'a','b'});
    AC = AstroTable({rand(10,2),rand(10,2)},'ConvertTable2array',false);
    AC = AstroTable({array2table(rand(10,2))});                     
    AC = AstroTable({rand(10,2)},'ColNames',{'RA','Dec'});

    % @FAILED - @Eran
    A = AstCat; A(1).Cat=rand(10,2);
    A(2).Cat=rand(10,2); 
    A(1).ColCell={'RA','Dec'};
    A(1).ColUnits={'rad','rad'};
    AC = AstroTable(A);
    AC = AstroTable(A,'ColNames',{'RA','Dec'},'ColUnits',{'rad','rad'});
    AC=AstroTable('asu.fit','HDU',2); % read from FITS table

    % merge selected columns of AstroTable
    %io.msgLog(LogLevel.Test, 'testing AstroTable merge 1/2')
    MAC = merge([AC,AC],{'DEJ2000'});
    % merge two AstroTable (all columns)
    %io.msgLog(LogLevel.Test, 'testing AstroTable 2/2')
    MAC = merge([AC,AC]);
    if length(AC.Catalog)*2 ~= length(MAC.Catalog)
        error('Merge error: Bad row count');
    end    
    

    % Sort by second column
    %io.msgLog(LogLevel.Test, 'testing AstroTable sortrows')
    sortrows(MAC,'DEJ2000');
    ColIndDec = colname2ind(MAC,'DEJ2000');
    if ~(MAC.IsSorted && issorted(MAC.Catalog(:,ColIndDec)))
        error('Problem with sort flagging');
    end

    % get column
    %io.msgLog(LogLevel.Test, 'testing AstroTable getCol')
    getCol(MAC,1);
    getCol(MAC,'DEJ2000');
    getCol(MAC,{'DEJ2000','RAJ2000'});
    
%     % output as table
%     getCol(MAC,{'mag1','sep1'},true);     %%% <---- BUG
%     
%     % store result in original AstroTable
%     Result = getCol(MAC,{'DEJ2000','RAJ2000'},false,true);
%     if ~all(Result == MAC.Catalog,'all')
%         error('Get column error: Result should be identical');
%     end

    % colnameDict2ind
    %io.msgLog(LogLevel.Test, 'testing AstroTable colnameDict2ind')
    AC1 = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'a','b'});
    [ColInd, ColName, IndOfSelectedName] = colnameDict2ind(AC1(1),{'X','Y','a','Z'});
    if ColInd~=1 || IndOfSelectedName~=3
        error('Problem with colnameDict2ind');
    end

    % funUnary
    %io.msgLog(LogLevel.Test, 'testing AstroTable funUnary')
    funUnary(AC(1),@sin);
    funUnary(AC(1),@sin,'Columns','RAJ2000');

    % min & max
    %io.msgLog(LogLevel.Test, 'testing AstroTable min and max')
    AC=AstroTable('asu.fit','HDU',2);
    min([AC;AC],{'mag1','mag2'});
    max([AC;AC],{'mag1','mag2'});
    max([AC;AC],[1, 2]);
    max([AC;AC]);            

    % query
    %io.msgLog(LogLevel.Test, 'testing AstroTable query')
    AC=AstroTable('asu.fit','HDU',2);
    BC=query(AC,'mag1>8 & mag2<10');
    query(AC,'mag1>8 & mag2<10','CreateNewObj',false); % modifies AC
    AC=AstroTable('asu.fit','HDU',2);
    BC=query([AC;AC],'mag1>8 & mag2<10');
    AC=array2table(AC);
    BC=query(AC,'mag1>8 & mag2<10');

    % queryRange
    %io.msgLog(LogLevel.Test, 'testing AstroTable queryRange')
    AT = AstroTable({rand(100,2)},'ColNames',{'x','y'});
    [Result, Flag] = queryRange(AT, 'x',[0.2 0.3],'y',[0.0 0.5]);
    [Result, Flag] = queryRange(AT, {'x_win','x'},[0.2 0.3],'y',[0.0 1]);

    % plot
    %io.msgLog(LogLevel.Test, 'testing AstroTable plot')
    AT = AstroTable;
    AT.Catalog=rand(10,4);
    AT.ColNames={'a','b','c','d'};
    AT.plotFun(@plot,{'a','c'},'o');
    AT.plotFun(@plot,[1, 2],'o','Color','b');
    AT.plotFun(@hist,{'c'});
    AT.plotFun(@hist,'d');
    AT.plotFun(@plot3,[1, 2, 4],'o','Color','b');

    AT = AstroTable;
    AT.Catalog = [(1:1:10).',(1:1:10).', rand(10,1)];
    AT(2).Catalog = [(1:1:10).', (10:-1:1).', rand(10,1)];
    AT(1).ColNames = {'X','Y','Flux'};
    AT(2).ColNames = {'X','Y','Flux'};
    AT.plot({'X','Y'},'o','MarkerFaceColor','r');

    % defaultColNames
    %io.msgLog(LogLevel.Test, 'testing AstroTable defaultColNames')
    AstroTable.defaultColNames(5);

    % compareColNames
    %io.msgLog(LogLevel.Test, 'testing AstroTable compareColNames')
    compcols = AstroTable.compareColNames({'a','b'},{'a','b'});
    if ~all(compcols) 
        error('Problem with compareColNames');
    end

    % insertColumn
    %io.msgLog(LogLevel.Test, 'testing AstroTable insertColumn')
    AstroTable.insertColumn({'a','b','c'},{'d','e'},2);
    AstroTable.insertColumn(ones(5,3),zeros(5,2),2);
    TT=array2table(zeros(5,2));
    TT.Properties.VariableNames={'a','b'};
    AstroTable.insertColumn(array2table(ones(5,3)),TT,2);

    % searchSynonym
    %io.msgLog(LogLevel.Test, 'testing AstroTable searchSynonym')
    [Name, IndInCell, IndInSynonym] = AstroTable.searchSynonym({'Number','ra','dec','Flux','Color'},{'ALPHA_J2000','RAJ2000','RA','ALPHA'});

    % sizeCatalog
    %io.msgLog(LogLevel.Test, 'testing AstroTable sizeCatalog')
    AC = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'a','b'});
    [Nrow,Ncol] = sizeCatalog(AC);
    if ~(Nrow(1) == 10 && Ncol(1) == 2)
        error('Problem with sizeCatalog');
    end

    % isemptyCatalog
    %io.msgLog(LogLevel.Test, 'testing AstroTable isemptyCatalog')
    AC = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'a','b'});
    if isemptyCatalog(AC)
        error('Non empty catalog flagged as empty');
    end
    AC = AstroTable();
    if ~isemptyCatalog(AC)
        error('Empty catalog flagged as non empty');
    end

    % deleteCatalog
    %io.msgLog(LogLevel.Test, 'testing AstroTable deleteCatalog')
    AC = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'a','b'});
    AC2 = AC;
    deleteCatalog(AC);
    if AC(1).Catalog == AC2(1).Catalog
        error('Problem deleting catalog');
    end
    if ~isempty(AC(1).Catalog)
        error('Problem deleting catalog');
    end

    % isColumn
    %io.msgLog(LogLevel.Test, 'testing AstroTable isColumn')
    AC = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'a','b'});
    Res = isColumn(AC,'a');
    if ~Res
        error('Problem with checking is column');
    end

    % colname2ind
    %io.msgLog(LogLevel.Test, 'testing AstroTable colname2ind')
    AC = AstroTable({rand(10,2)},'ColNames',{'a','b'});
    colname2ind(AC, {'a','b'});
    AC = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'a','b'});
    colname2ind(AC(1), {'a','b'});

    % colind2name
    %io.msgLog(LogLevel.Test, 'testing AstroTable colind2name')
    AC = AstroTable({rand(10,2)},'ColNames',{'a','b'});
    if ~strcmpi(colind2name(AC,[2 1]), {'b', 'a'})
       error('Problem with colind2name'); 
    end
    AC = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'a','b'});
    if ~strcmpi(colind2name(AC(1),[2 1]), {'b', 'a'})
       error('Problem with colind2name'); 
    end

    % col2struct
    %io.msgLog(LogLevel.Test, 'testing AstroTable col2struct')
    AC = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'a','b'});
    col2struct(AC);

    % isColIdentical
    %io.msgLog(LogLevel.Test, 'testing AstroTable isColIdentical')
    AC = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'a','b'});
    if ~isColIdentical(AC,AC(1).ColNames)
        error('Identical columns flagged as different');
    end
    if isColIdentical(AC,{'b', 'c'})
        error('Different columns flagged as identical');
    end


    % table2array
    %io.msgLog(LogLevel.Test, 'testing AstroTable table2array')
    AC = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'a','b'});
    Res = table2array(AC);

    % array2table
    %io.msgLog(LogLevel.Test, 'testing AstroTable array2table')
    AC = array2table(Res);

    % insertCol
    %io.msgLog(LogLevel.Test, 'testing AstroTable insertCol')
    A = AstroTable; 
    A.Catalog=rand(10,3); 
    A.ColNames={'a','b','c'}; 
    insertCol(A,ones(10,2),Inf,{'c','d'});

    % replaceColNames
    %io.msgLog(LogLevel.Test, 'testing AstroTable replaceColNames')
    AC = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'a','b'});
    AC.replaceColNames([1 2],{'RA','Dec'});
    if ~all(strcmpi(AC(1).ColNames, {'RA', 'Dec'}))
       error('Problem with replacing col names'); 
    end

    % replaceCol
    %io.msgLog(LogLevel.Test, 'testing AstroTable replaceCol')
    AC = AstroTable;
    AC(1).Catalog = rand(100,3);
    AC(1).ColNames={'a','b','c'};
    AC=AC.replaceCol(nan(100,2),{'a','b'});

    % deleteCol
    %io.msgLog(LogLevel.Test, 'testing AstroTable deleteCol')
    AC.Catalog=array2table(rand(10,3));
    AC.ColNames={'RA','Dec','flux'}; AC.deleteCol('Dec');
    AC.Catalog=(rand(10,3));
    AC.ColNames={'RA','Dec','flux'}; AC.deleteCol({'RA','Dec'});

    % sortrows
    %io.msgLog(LogLevel.Test, 'testing AstroTable sortrows')
    AC=AstroTable; 
    AC.Catalog = rand(100,3); 
    AC=sortrows(AC,2);

    % flipud
    %io.msgLog(LogLevel.Test, 'testing AstroTable flipud')
    AC=AstroTable; 
    AC.Catalog = rand(100,3); 
    flipud(AC);
    
    % selectLines
    ATable = (1:1:5).'; 
    ATable = [ATable, rand(size(ATable))];
    Result = AstroTable.selectLines(ATable, [1;2]);
    
    % insertLines
    Table = (1:1:5).'; Table = [Table, rand(size(Table))];
    Table2 = (1:1:4).'; Table2 = [Table2, rand(size(Table2,1),2)];
    Result = AstroTable.insertLines(Table, Table2(3:4,:), [1 2]);
    Result = AstroTable.insertLines(Table, Table2(3:4,:), [1 2],'FillNaN',false);
    
    % getColUnits
    AC = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'RA','Dec'},'ColUnits',{'rad','rad'});
    Result = getColUnits(AC(1), 'RA');
    
    % Col2struct
    AT = AstroTable({rand(1000,2)},'ColNames',{'a','b'});
    S  = getCol2struct(AT, {'a'});
    
    % getColDic
    [Result, Units, Ind, ColName] = getColDic(AT,{'c','d','b'});
    
    % insertRows
    Table = (1:1:5).'; 
    Table = [Table, rand(size(Table))];
    AT = AstroTable({Table});
    Table2 = (1:1:4).'; 
    Table2 = [Table2, rand(size(Table2,1),2)];
    AT2 = AstroTable({Table2});
    Result = insertRows(AT, AT2, [1 2 3 4 5],[4 3 2 1 1]);
    Result = insertRows(AT, AT2, [1 2 3 4 ],[4 3 2 1 ]);    
    
    % selectRows
    Table = (1:1:5).'; 
    Table = [Table, rand(size(Table))];
    AT = AstroTable({Table});
    Result = selectRows(AT, [1;2]);
    Result = selectRows(AT, [2 3 2]);
    Result = selectRows(AT, [true, true, false, true, false]);
    Result = selectRows(AT, [2 3 2 NaN],'IgnoreNaN',true);
    Result = selectRows(AT, [2 3 NaN 2 NaN],'IgnoreNaN',false);
    
    % queryFun    
    AT = AstroTable({rand(10,2)},'ColNames',{'X','Y'});
    T = queryFun(AT, {'X', @(x) x>0.5, 'Y', @(x) x>0.5}, 'CreateNewObj',true);

    % csvWrite
    csvWrite(AT, 'AT_unittest_test.csv');
    delete('AT_unittest_test.csv');

    %toTable
    AT = AstroTable({rand(10,2)},'ColNames',{'a','b'});
    T  = AT.toTable;
    
    % write1
    write1(AT, 'AT_unittest_test.fits');
    delete('AT_unittest_test.fits');
    
    cd(PWD);
    %io.msgStyle(LogLevel.Test, '@passed', 'AstroTable test passed');
    Result = true;
end
