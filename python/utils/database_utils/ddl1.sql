CREATE TABLE public."table" (
  "RawImageID" INTEGER NOT NULL,
  "RA_Center" DOUBLE PRECISION,
  str1 VARCHAR(250),
  CONSTRAINT table_pkey PRIMARY KEY("RawImageID")
) ;

ALTER TABLE public."table"
  ALTER COLUMN "RawImageID" SET STATISTICS 0;

ALTER TABLE public."table"
  ALTER COLUMN "RA_Center" SET STATISTICS 0;

ALTER TABLE public."table"
  ALTER COLUMN str1 SET STATISTICS 0;

ALTER TABLE public."table"
  OWNER TO postgres;
  
  