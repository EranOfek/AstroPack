% http://www.java2s.com/Code/Java/Database-SQL-JDBC/UseDatabaseMetaDatatogettablecolumnnames.htm

// Use DatabaseMetaData to get table column names
  

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.DriverManager;
import java.sql.ResultSet;

public class Main {
  private static final String DRIVER = "com.mysql.jdbc.Driver";

  private static final String URL = "jdbc:mysql://localhost/testdb";

  private static final String USERNAME = "root";

  private static final String PASSWORD = "";

  public static void main(String[] args) throws Exception {
    Class.forName(DRIVER);
    Connection connection = DriverManager.getConnection(URL, USERNAME, PASSWORD);

    DatabaseMetaData metadata = connection.getMetaData();
    ResultSet resultSet = metadata.getColumns(null, null, "users", null);
    while (resultSet.next()) {
      String name = resultSet.getString("COLUMN_NAME");
      String type = resultSet.getString("TYPE_NAME");
      int size = resultSet.getInt("COLUMN_SIZE");

      System.out.println("Column name: [" + name + "]; type: [" + type + "]; size: [" + size + "]");
    }
    connection.close();
  }
}
 

   
    
  

  
  