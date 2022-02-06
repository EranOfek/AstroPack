--
-- FirebirdSQL database
--

-- When executing the script on existing database, remove this 'CREATE DATABASE' statement
-- Use 'DB Browser for SQLite' as GUI for SQLite
CREATE DATABASE $DatabaseName$ USER 'SYSDBA'
   PAGE_SIZE 4096
   DEFAULT CHARACTER SET UTF8;
