--
-- Automatic generated file by xlsx2sql.py
-- Origin file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest.xlsx
--

--
-- SQLite database
--
-- Execute by command line:
--      sqlite3 db_file_name.sqlite < script.sql

-- When executing the script on existing database, remove this 'CREATE DATABASE' statement
-- Use 'DB Browser for SQLite' as GUI for SQLite

-- CREATE DATABASE unittest USER 'SYSDBA'
--    PAGE_SIZE 4096
--    DEFAULT CHARACTER SET UTF8;


-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest\csv\unittest - big_table.csv
CREATE TABLE big_table (
RecID TEXT NOT NULL,
FDouble01 REAL,
FDouble02 REAL,
FDouble03 REAL,
FDouble04 REAL,
FDouble05 REAL,
FDouble06 REAL,
FDouble07 REAL,
FDouble08 REAL,
FDouble09 REAL,
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
FDouble21 REAL,
FDouble22 REAL,
FDouble23 REAL,
FDouble24 REAL,
FDouble25 REAL,
FDouble26 REAL,
FDouble27 REAL,
FDouble28 REAL,
FDouble29 REAL,
FDouble30 REAL,
FDouble31 REAL,
FDouble32 REAL,
FDouble33 REAL,
FDouble34 REAL,
FDouble35 REAL,
FDouble36 REAL,
FDouble37 REAL,
FDouble38 REAL,
FDouble39 REAL,
FDouble40 REAL,
FDouble41 REAL,
FDouble42 REAL,
FDouble43 REAL,
FDouble44 REAL,
FDouble45 REAL,
FDouble46 REAL,
FDouble47 REAL,
FDouble48 REAL,
FDouble49 REAL,
FDouble50 REAL,

PRIMARY KEY(RecID)
);

CREATE INDEX big_table_idx_FDouble01 ON big_table(FDouble01);
CREATE INDEX big_table_idx_FDouble02 ON big_table(FDouble02);
CREATE INDEX big_table_idx_FDouble03 ON big_table(FDouble03);
CREATE INDEX big_table_idx_FDouble04 ON big_table(FDouble04);
CREATE INDEX big_table_idx_FDouble05 ON big_table(FDouble05);
CREATE INDEX big_table_idx_FDouble06 ON big_table(FDouble06);
CREATE INDEX big_table_idx_FDouble07 ON big_table(FDouble07);
CREATE INDEX big_table_idx_FDouble08 ON big_table(FDouble08);
CREATE INDEX big_table_idx_FDouble09 ON big_table(FDouble09);
CREATE INDEX big_table_idx_FDouble10 ON big_table(FDouble10);
CREATE INDEX big_table_idx_FDouble11 ON big_table(FDouble11);
CREATE INDEX big_table_idx_FDouble12 ON big_table(FDouble12);
CREATE INDEX big_table_idx_FDouble13 ON big_table(FDouble13);
CREATE INDEX big_table_idx_FDouble14 ON big_table(FDouble14);
CREATE INDEX big_table_idx_FDouble15 ON big_table(FDouble15);
CREATE INDEX big_table_idx_FDouble16 ON big_table(FDouble16);
CREATE INDEX big_table_idx_FDouble17 ON big_table(FDouble17);
CREATE INDEX big_table_idx_FDouble18 ON big_table(FDouble18);
CREATE INDEX big_table_idx_FDouble19 ON big_table(FDouble19);
CREATE INDEX big_table_idx_FDouble20 ON big_table(FDouble20);
CREATE INDEX big_table_idx_FDouble21 ON big_table(FDouble21);
CREATE INDEX big_table_idx_FDouble22 ON big_table(FDouble22);
CREATE INDEX big_table_idx_FDouble23 ON big_table(FDouble23);
CREATE INDEX big_table_idx_FDouble24 ON big_table(FDouble24);
CREATE INDEX big_table_idx_FDouble25 ON big_table(FDouble25);
CREATE INDEX big_table_idx_FDouble26 ON big_table(FDouble26);
CREATE INDEX big_table_idx_FDouble27 ON big_table(FDouble27);
CREATE INDEX big_table_idx_FDouble28 ON big_table(FDouble28);
CREATE INDEX big_table_idx_FDouble29 ON big_table(FDouble29);
CREATE INDEX big_table_idx_FDouble30 ON big_table(FDouble30);
CREATE INDEX big_table_idx_FDouble31 ON big_table(FDouble31);
CREATE INDEX big_table_idx_FDouble32 ON big_table(FDouble32);
CREATE INDEX big_table_idx_FDouble33 ON big_table(FDouble33);
CREATE INDEX big_table_idx_FDouble34 ON big_table(FDouble34);
CREATE INDEX big_table_idx_FDouble35 ON big_table(FDouble35);
CREATE INDEX big_table_idx_FDouble36 ON big_table(FDouble36);
CREATE INDEX big_table_idx_FDouble37 ON big_table(FDouble37);
CREATE INDEX big_table_idx_FDouble38 ON big_table(FDouble38);
CREATE INDEX big_table_idx_FDouble39 ON big_table(FDouble39);
CREATE INDEX big_table_idx_FDouble40 ON big_table(FDouble40);
CREATE INDEX big_table_idx_FDouble41 ON big_table(FDouble41);
CREATE INDEX big_table_idx_FDouble42 ON big_table(FDouble42);
CREATE INDEX big_table_idx_FDouble43 ON big_table(FDouble43);
CREATE INDEX big_table_idx_FDouble44 ON big_table(FDouble44);
CREATE INDEX big_table_idx_FDouble45 ON big_table(FDouble45);
CREATE INDEX big_table_idx_FDouble46 ON big_table(FDouble46);
CREATE INDEX big_table_idx_FDouble47 ON big_table(FDouble47);
CREATE INDEX big_table_idx_FDouble48 ON big_table(FDouble48);
CREATE INDEX big_table_idx_FDouble49 ON big_table(FDouble49);
CREATE INDEX big_table_idx_FDouble50 ON big_table(FDouble50);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest\csv\unittest - big_table2.csv
CREATE TABLE big_table2 (
RecID TEXT NOT NULL,
FDouble01 REAL,
FDouble02 REAL,
FDouble03 REAL,
FDouble04 REAL,
FDouble05 REAL,
FDouble06 REAL,
FDouble07 REAL,
FDouble08 REAL,
FDouble09 REAL,
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
FDouble21 REAL,
FDouble22 REAL,
FDouble23 REAL,
FDouble24 REAL,
FDouble25 REAL,
FDouble26 REAL,
FDouble27 REAL,
FDouble28 REAL,
FDouble29 REAL,
FDouble30 REAL,
FDouble31 REAL,
FDouble32 REAL,
FDouble33 REAL,
FDouble34 REAL,
FDouble35 REAL,
FDouble36 REAL,
FDouble37 REAL,
FDouble38 REAL,
FDouble39 REAL,
FDouble40 REAL,
FDouble41 REAL,
FDouble42 REAL,
FDouble43 REAL,
FDouble44 REAL,
FDouble45 REAL,
FDouble46 REAL,
FDouble47 REAL,
FDouble48 REAL,
FDouble49 REAL,
FDouble50 REAL,

PRIMARY KEY(RecID)
);

CREATE INDEX big_table2_idx_FDouble01 ON big_table2(FDouble01);
CREATE INDEX big_table2_idx_FDouble02 ON big_table2(FDouble02);
CREATE INDEX big_table2_idx_FDouble03 ON big_table2(FDouble03);
CREATE INDEX big_table2_idx_FDouble04 ON big_table2(FDouble04);
CREATE INDEX big_table2_idx_FDouble05 ON big_table2(FDouble05);

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest\csv\unittest - details_table.csv
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

-- Source file: D:\Ultrasat\AstroPack.git\database\xlsx\unittest\csv\unittest - master_table.csv
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

