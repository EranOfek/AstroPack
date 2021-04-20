%
% https://stackoverflow.com/questions/2698159/how-can-i-access-a-postgresql-database-from-matlab-with-without-matlabs-database
%

% Add jar file to classpath (ensure it is present in your current dir)
%javaclasspath('postgresql-9.0-801.jdbc4.jar');
javaclasspath('postgresql-42.2.19.jar');

% Username and password you chose when installing postgres
props = java.util.Properties;
props.setProperty('user', 'postgres');
props.setProperty('password', 'pass');

% Create the database connection (port 5432 is the default postgres chooses
% on installation)
driver = org.postgresql.Driver;

url = 'jdbc:postgresql://localhost:5432/pipeline';
conn = driver.connect(url, props)


% A test query
sql='select * from raw_images'; % Gets all records
ps=conn.prepareStatement(sql);
rs=ps.executeQuery();

% Read the results into an array of result structs
count=0;
result=struct;
while rs.next()
    disp(rs.getString(1));  %
    %count=count+1;
    %result(count).var1=char(rs.getString(2));
    %result(count).var2=char(rs.getString(3));
    %...
end

rs.close();



