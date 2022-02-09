
Q = db.DbQuery('unittest');

% Select without COPY TO, load result set, return DbRecord
Data1 = Q.select('*', 'TableName', 'master_table', 'Limit', 10, 'Load', true);

%
Data2 = Q.select('*', 'TableName', 'master_table', 'Limit', 10, 'Load', true, 'UseCsv', true);

%
Data3 = Q.select('*', 'TableName', 'master_table', 'Limit', 10, 'Load', true, 'UseCopy', true);





Data1b = Q.select('*', 'TableName', 'master_table', 'Limit', 10, 'Load', false);

%
Data2b = Q.select('*', 'TableName', 'master_table', 'Limit', 10, 'Load', false, 'UseCsv', true);

%
Data3b = Q.select('*', 'TableName', 'master_table', 'Limit', 10, 'Load', false, 'UseCopy', true);



% Select, write output to CSV file
Result = Q.select('*', 'TableName', 'master_table', 'Limit', 10, 'CsvFileName', 'C:\Temp\abc.csv');



