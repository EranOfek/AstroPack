// https://www.programcreek.com/java-api-examples/?class=java.sql.Statement&method=executeUpdate

/**
 * Inserts a duplicate key of a deleted key within same transaction.
 * @throws java.sql.SQLException
 */
public void testWithDeletedKey() throws SQLException {
    Connection con = getConnection();
    Statement stmt = con.createStatement();
    //create unique constraint without not null
    stmt.executeUpdate("alter table constraintest add constraint " +
            "u_con unique (val1, val2, val3)");
    PreparedStatement ps  = con.prepareStatement("insert into " +
            "constraintest (val1, val2, val3, val4) values (?, ?, ?, ?)");
    ps.setString(1, "part1");
    ps.setString(2, "part2");
    ps.setString(3, "part3");
    ps.setString(4, "should pass");
    ps.execute();
    //delete a record within transaction and try inserting same record
    con.setAutoCommit(false);
    stmt.executeUpdate("delete from constraintest where " +
            "val1 = 'part1' and val2 = 'part2' and val3 = 'part3'");
    //insert same record
    ps.setString(1, "part1");
    ps.setString(2, "part2");
    ps.setString(3, "part3");
    ps.setString(4, "should pass");
    ps.execute();
    stmt.close();
    ps.close();
    con.commit();
}

