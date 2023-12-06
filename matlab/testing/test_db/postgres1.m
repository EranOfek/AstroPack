% https://stackoverflow.com/questions/2698159/how-can-i-access-a-postgresql-database-from-matlab-with-without-matlabs-database

% https://undocumentedmatlab.com/blog_old/speeding-up-matlab-jdbc-sql-queries

% https://stackoverflow.com/questions/24438359/connecting-matlab-and-mysql-with-the-jdbc-driver

javaclasspath('d:\jdbc\postgresql-42.2.19.jar');
javaaddpath('d:\jdbc\postgresql-42.2.19.jar');

%d = com.postgresql.jdbc.Driver;

username = 'postgres';
password = 'pass';
schemaName = 'pipeline';
dbURL = 'localhost'; % 'jdbc:postgresql://localhost:5432/pipeline';

% Load the appropriate JDBC driver class into Matlab's memory
% (but not directly, to bypass JIT pre-processing - we must do it in run-time!)

driverClassName = 'com.postgresql.jdbc.Driver';
driverClassName = 'postgresql.jdbc.Driver';
driverClassName = 'org.postgresql.Driver';
%java.lang.Class.forName(driverClassName);

%driver = eval('com.postgresql.jdbc.Driver');  % or com.microsoft.sqlserver.jdbc.SQLServerDriver or whatever
 
%java.sql.Class.forName("org.postgresql.Driver");

% Connect to DB
dbPort = '5432'; %'3306'; % mySQL=3306; SQLServer=1433; Oracle=...
%connectionStr = ['jdbc:postgresql://' dbURL ':' dbPort '/' schemaName];  % or ['jdbc:sqlserver://' dbURL ':' dbPort ';database=' schemaName ';'] or whatever

connectionStr = 'jdbc:postgresql:pipeline';

dbConnObj = java.sql.DriverManager.getConnection(connectionStr, username, password);
 

sqlQueryStr = 'select * from raw_images'

% Send an SQL query statement to the DB and get the ResultSet
stmt = dbConnObj.createStatement(java.sql.ResultSet.TYPE_SCROLL_INSENSITIVE, java.sql.ResultSet.CONCUR_READ_ONLY);
try stmt.setFetchSize(1000); catch, end  % the default fetch size is ridiculously small in many DBs
rs = stmt.executeQuery(sqlQueryStr);
 
% Get the column names and data-types from the ResultSet's meta-data
MetaData = rs.getMetaData;
numCols = MetaData.getColumnCount;
data = cell(0,numCols);  % initialize
for colIdx = numCols : -1 : 1
    ColumnNames{colIdx} = char(MetaData.getColumnLabel(colIdx));
    ColumnType{colIdx}  = char(MetaData.getColumnClassName(colIdx));  % http://docs.oracle.com/javase/7/docs/api/java/sql/Types.html
end
ColumnType = regexprep(ColumnType,'.*\.','');
 
% Get the data from the ResultSet into a Matlab cell array
rowIdx = 1;
while rs.next  % loop over all ResultSet rows (records)
    for colIdx = 1 : numCols  % loop over all columns in the row
        switch ColumnType{colIdx}
            case {'Float','Double'}
                data{rowIdx,colIdx} = rs.getDouble(colIdx);
            case {'Long','Integer','Short','BigDecimal'}
                data{rowIdx,colIdx} = double(rs.getDouble(colIdx));
            case 'Boolean'
                data{rowIdx,colIdx} = logical(rs.getBoolean(colIdx));
            otherwise %case {'String','Date','Time','Timestamp'}
                data{rowIdx,colIdx} = char(rs.getString(colIdx));
        end
    end
    rowIdx = rowIdx + 1;
end
 
% Close the connection and clear resources
try rs.close();   catch, end
try stmt.close(); catch, end
try dbConnObj.closeAllStatements(); catch, end
try dbConnObj.close(); catch, end  % comment this to keep the dbConnObj open and reuse it for subsequent queries