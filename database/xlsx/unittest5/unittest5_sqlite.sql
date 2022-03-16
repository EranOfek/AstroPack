--
-- Automatic generated file by xlsx2sql.py
-- Origin file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest5.xlsx
--

--
-- SQLite database
--
-- Execute by command line:
--      sqlite3 db_file_name.sqlite < script.sql

-- When executing the script on existing database, remove this 'CREATE DATABASE' statement
-- Use 'DB Browser for SQLite' as GUI for SQLite

-- CREATE DATABASE unittest5 USER 'SYSDBA'
--    PAGE_SIZE 4096
--    DEFAULT CHARACTER SET UTF8;


-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest5\csv\unittest5 - details_table.csv
CREATE TABLE details_table (
RecID TEXT NOT NULL,
InsertTime REAL,
UpdateTime REAL,
FInt INTEGER,
FBigInt BIGINT,
FBool INTEGER,
FDouble REAL,
FTimestamp REAL,
FString TEXT,
FDouble1 REAL,
FDouble2 REAL,
FDouble3 REAL,
FDouble4 REAL,
FDouble5 REAL,
FDouble6 REAL,
FDouble7 REAL,
FDouble8 REAL,
FDouble9 REAL,
FDouble10 REAL,
FDouble11 REAL,
FDouble12 REAL,
FDouble13 REAL,
FDouble14 REAL,
FDouble15 REAL,
FDouble16 REAL,
FDouble17 REAL,
FDouble18 REAL,
FDouble19 REAL,
FDouble20 REAL,
FInt1 INTEGER,
FInt2 INTEGER,
FInt3 INTEGER,
FInt4 INTEGER,
FInt5 INTEGER,
FInt6 INTEGER,
FInt7 INTEGER,
FInt8 INTEGER,
FInt9 INTEGER,
FInt10 INTEGER,
FString1 TEXT,
FString2 TEXT,
FString3 TEXT,
FString4 TEXT,
FString5 TEXT,
FString6 TEXT,
FString7 TEXT,
FString8 TEXT,
FString9 TEXT,
FString10 TEXT,
dstring1 TEXT,
dstring2 TEXT,
dstring3 TEXT,

PRIMARY KEY(RecID)
);

CREATE INDEX details_table_idx_RecID ON details_table(RecID);
CREATE INDEX details_table_idx_InsertTime ON details_table(InsertTime);
CREATE INDEX details_table_idx_UpdateTime ON details_table(UpdateTime);
CREATE INDEX details_table_idx_FInt ON details_table(FInt);
CREATE INDEX details_table_idx_FBigInt ON details_table(FBigInt);
CREATE INDEX details_table_idx_FBool ON details_table(FBool);
CREATE INDEX details_table_idx_FDouble ON details_table(FDouble);
CREATE INDEX details_table_idx_FTimestamp ON details_table(FTimestamp);
CREATE INDEX details_table_idx_FString ON details_table(FString);
CREATE INDEX details_table_idx_FDouble1 ON details_table(FDouble1);
CREATE INDEX details_table_idx_FDouble2 ON details_table(FDouble2);
CREATE INDEX details_table_idx_FDouble3 ON details_table(FDouble3);
CREATE INDEX details_table_idx_FDouble4 ON details_table(FDouble4);
CREATE INDEX details_table_idx_FInt1 ON details_table(FInt1);
CREATE INDEX details_table_idx_FInt2 ON details_table(FInt2);
CREATE INDEX details_table_idx_FInt3 ON details_table(FInt3);
CREATE INDEX details_table_idx_FInt4 ON details_table(FInt4);
CREATE INDEX details_table_idx_FString1 ON details_table(FString1);
CREATE INDEX details_table_idx_FString2 ON details_table(FString2);
CREATE INDEX details_table_idx_FString3 ON details_table(FString3);
CREATE INDEX details_table_idx_FString4 ON details_table(FString4);
CREATE INDEX details_table_idx_dstring1 ON details_table(dstring1);
CREATE INDEX details_table_idx_dstring2 ON details_table(dstring2);
CREATE INDEX details_table_idx_dstring3 ON details_table(dstring3);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest5\csv\unittest5 - master_table.csv
CREATE TABLE master_table (
RecID TEXT NOT NULL,
InsertTime REAL,
UpdateTime REAL,
FInt INTEGER,
FBigInt BIGINT,
FBool INTEGER,
FDouble REAL,
FTimestamp REAL,
FString TEXT,
FDouble1 REAL,
FDouble2 REAL,
FDouble3 REAL,
FDouble4 REAL,
FDouble5 REAL,
FDouble6 REAL,
FDouble7 REAL,
FDouble8 REAL,
FDouble9 REAL,
FDouble10 REAL,
FDouble11 REAL,
FDouble12 REAL,
FDouble13 REAL,
FDouble14 REAL,
FDouble15 REAL,
FDouble16 REAL,
FDouble17 REAL,
FDouble18 REAL,
FDouble19 REAL,
FDouble20 REAL,
FInt1 INTEGER,
FInt2 INTEGER,
FInt3 INTEGER,
FInt4 INTEGER,
FInt5 INTEGER,
FInt6 INTEGER,
FInt7 INTEGER,
FInt8 INTEGER,
FInt9 INTEGER,
FInt10 INTEGER,
FString1 TEXT,
FString2 TEXT,
FString3 TEXT,
FString4 TEXT,
FString5 TEXT,
FString6 TEXT,
FString7 TEXT,
FString8 TEXT,
FString9 TEXT,
FString10 TEXT,
mstring1 TEXT,
mstring2 TEXT,
mstring3 TEXT,

PRIMARY KEY(RecID)
);

CREATE INDEX master_table_idx_RecID ON master_table(RecID);
CREATE INDEX master_table_idx_InsertTime ON master_table(InsertTime);
CREATE INDEX master_table_idx_UpdateTime ON master_table(UpdateTime);
CREATE INDEX master_table_idx_FInt ON master_table(FInt);
CREATE INDEX master_table_idx_FBigInt ON master_table(FBigInt);
CREATE INDEX master_table_idx_FBool ON master_table(FBool);
CREATE INDEX master_table_idx_FDouble ON master_table(FDouble);
CREATE INDEX master_table_idx_FTimestamp ON master_table(FTimestamp);
CREATE INDEX master_table_idx_FString ON master_table(FString);
CREATE INDEX master_table_idx_FDouble1 ON master_table(FDouble1);
CREATE INDEX master_table_idx_FDouble2 ON master_table(FDouble2);
CREATE INDEX master_table_idx_FDouble3 ON master_table(FDouble3);
CREATE INDEX master_table_idx_FDouble4 ON master_table(FDouble4);
CREATE INDEX master_table_idx_FInt1 ON master_table(FInt1);
CREATE INDEX master_table_idx_FInt2 ON master_table(FInt2);
CREATE INDEX master_table_idx_FInt3 ON master_table(FInt3);
CREATE INDEX master_table_idx_FInt4 ON master_table(FInt4);
CREATE INDEX master_table_idx_FString1 ON master_table(FString1);
CREATE INDEX master_table_idx_FString2 ON master_table(FString2);
CREATE INDEX master_table_idx_FString3 ON master_table(FString3);
CREATE INDEX master_table_idx_FString4 ON master_table(FString4);
CREATE INDEX master_table_idx_mstring1 ON master_table(mstring1);
CREATE INDEX master_table_idx_mstring2 ON master_table(mstring2);
CREATE INDEX master_table_idx_mstring3 ON master_table(mstring3);

