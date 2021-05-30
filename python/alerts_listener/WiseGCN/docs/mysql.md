# Setting up the `mysql` database

Install `mysql`, and log in using the root user:
```
$ mysql -u root -p
```
Create a new user (e.g. `gcn`) and assign it a password:
```
mysql> CREATE USER 'gcn'@'localhost' IDENTIFIED BY 'password';
```
Create a new database, e.g. `gw`:
```
mysql> CREATE DATABASE gw;
```
Assign permissions to the new user on the new database:
```
mysql> GRANT ALL PRIVILEGES ON gw.* TO 'gcn'@'localhost';
```
Log out of `mysql`, and log in again as the new user:
```
mysql> quit
$ mysql -u gcn -p
```
To use the new database run:
```
mysql> USE gw;
```
Now you will need to create a few tables. You will only need to create them once.

## VOEvent storage tables

### voevent_lvc table
This table will be used to store each recieved LVC VOevent. To create it, run in `mysql`:
```
mysql> CREATE TABLE `voevent_lvc` (
	`id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
	`ivorn` text,
	`role` text,
	`version` text,
	`author_ivorn` text,
	`date_ivorn` text,
	`internal` int,
	`Packet_Type` int,
	`Pkt_Ser_Num` text,
	`GraceID` text,
	`AlertType` text,
	`HardwareInj` int,
	`OpenAlert` int,
	`EventPage` text,
	`Instruments` text,
	`FAR` float,
	`Group` text,
	`Pipeline` text,
	`Search` text,
	`skymap_fits` text,
	`BNS` float,
	`NSBH` float,
	`BBH` float,
	`Terrestrial` float,
	`HasNS` float,
	`HasRemnant` float,
	`observatorylocation_id` text,
	`astrocoordsystem_id` text,
	`isotime` text,
	`how_description` text,
	`datecreated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	`lastmodified` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
	PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```

### voevent_amon table

NOTE: No need to create the `voevent_amon` table for this version of the pipeline (no ICECUBE events).

This table will be used to store the ranked galaxies for each recieved ICECUBE event. To create it, run in `mysql`:
```
mysql> CREATE TABLE `voevent_amon` (
	`id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
	`ivorn` text NOT NULL,
	`role` varchar(45) NOT NULL,
	`version` text NOT NULL,
	`xmlns_voe` text NOT NULL,
	`xmlns_xsi` text NOT NULL,
	`xsi_schemalocation` text NOT NULL,
	`author_ivorn` text NOT NULL,
	`shortname` text,
	`contactname` text,
	`contactemail` text,
	`who_description` text,
	`date` text NOT NULL,
	`packet_type` text NOT NULL,
	`pkt_ser_num` text NOT NULL,
	`trig_id` text NOT NULL,
	`event_tjd` text NOT NULL,
	`event_sod` text NOT NULL,
	`nevents` text NOT NULL,
	`stream` text NOT NULL,
	`rev` varchar(45) NOT NULL,
	`false_pos` varchar(45) NOT NULL,
	`pvalue` varchar(45) NOT NULL,
	`deltat` text NOT NULL,
	`sigmat` text NOT NULL,
	`charge` text NOT NULL,
	`signalness` text NOT NULL,
	`hesetypeindex` text NOT NULL,
	`trigger_id` varchar(45) NOT NULL,
	`misc_flags` varchar(45) DEFAULT NULL,
	`subtype` text,
	`test` varchar(45) DEFAULT NULL,
	`radec_valid` varchar(45) DEFAULT NULL,
	`retraction` varchar(45) DEFAULT NULL,
	`internal_test` text NOT NULL,
	`observatorylocation_id` text NOT NULL,
	`astrocoordsystem_id` text NOT NULL,
	`timeunit` varchar(45) NOT NULL,
	`isotime` text NOT NULL,
	`ra0` text DEFAULT NULL,
	`dec0` text DEFAULT NULL,
	`error2radius` text DEFAULT NULL,
	`how_description` text,
	`reference_uri` text,
	`importance` text DEFAULT NULL,
	`inference_probability` text DEFAULT NULL,
	`concept` text,
	`datecreated` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
	`lastmodified` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
	PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```

NOTE: You might need to temporarily change the `sql_mode` to create this table, if `show variables like 'sql_mode' ; ` returns also `NO_ZERO_IN_DATE,NO_ZERO_DATE`. To disable it for this session, execute:
```
mysql> SET sql_mode = '';
```
then try again to create the table.

## Catalog table

`wisegcn` uses the [Glade](http://aquarius.elte.hu/glade/) (Galaxy List for the Advanced Detector Era) catalog to find all possible host galaxies and rank them.

First, download the lateset version of the catalog in text format: [Glade v2.3](http://aquarius.elte.hu/glade/GLADE_2.3.txt).

### glade_catalog table
In `mysql`, create an empty table to accommodate the catalog:
```
mysql> CREATE TABLE `glade_catalog` (
	`glade_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
	`pgc` double,
	`gwgc_name` text,
	`hyperleda_name` text,
	`2mass_name` text,
	`sdssdr12_name` text,
	`flag1` varchar(1),
	`ra0` double,
	`dec0` double,
	`dist` double,
	`dist_err` double,
	`z` double,
	`bmag` double,
	`bmag_err` double,
	`bmag_abs` double,
	`jmag` double,
	`jmag_err` double,
	`hmag` double,
	`hmag_err` double,
	`kmag` double,
	`kmag_err` double,
	`flag2` TINYINT(1),
	`flag3` TINYINT(1),
	PRIMARY KEY (`glade_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```

Then, import the catalog's text file to `mysql` (assuming the text file is stored in `/var/lib/mysql-files` - you should put it there to avoid changing the `--secure-file-priv` settings of `mysql`):
```
mysql> LOAD DATA INFILE '/var/lib/mysql-files/GLADE_2.3.txt'
	INTO TABLE `glade_catalog`
	FIELDS TERMINATED BY ' '
	LINES TERMINATED BY '\n'
(@pgc,@gwgc_name,@hyperleda_name,@2mass_name,@sdssdr12_name,@flag1,@ra0,@dec0,@dist,@dist_err,@z,@bmag,@bmag_err,@bmag_abs,@jmag,@jmag_err,@hmag,@hmag_err,@kmag,@kmag_err,@flag2,@flag3)
SET
	pgc = nullif(@pgc, "null"),
	gwgc_name = nullif(@gwgc_name, "null"),
	hyperleda_name = nullif(@hyperleda_name, "null"),
	2mass_name = nullif(@2mass_name, "null"),
	sdssdr12_name = nullif(@sdssdr12_name, "null"),
	flag1 = nullif(@flag1, "null"),
	ra0 = nullif(@ra0, "null"),
	dec0 = nullif(@dec0, "null"),
	dist = nullif(@dist, "null"),
	dist_err = nullif(@dist_err, "null"),
	z = nullif(@z, "null"),
	bmag = nullif(@bmag, "null"),
	bmag_err = nullif(@bmag_err, "null"),
	bmag_abs = nullif(@bmag_abs, "null"),
	jmag = nullif(@jmag, "null"),
	jmag_err = nullif(@jmag_err, "null"),
	hmag = nullif(@hmag, "null"),
	hmag_err = nullif(@hmag_err, "null"),
	kmag = nullif(@kmag, "null"),
	kmag_err = nullif(@kmag_err, "null"),
	flag2 = nullif(@flag2, "null"),
	flag3 = nullif(@flag3, "null");
```

NOTE: If you get an `ERROR 1045 (28000): Access denied for user load data infile`, you should first run (as `mysql` root user):
```
mysql> GRANT FILE ON *.* TO gcn@localhost;
```

### glade_2.3_RA_Dec.npy
The `wisegcn` code uses a reduced version of the catalog, stored in `.npy` format.
To create it, first export the selected columns to a `.csv` file:
```
mysql> SELECT glade_id, ra0, dec0, ifnull(dist, "None"), ifnull(bmag, "None")
	FROM glade_catalog
	INTO OUTFILE '/var/lib/mysql-files/glade_2.3_RA_Dec.csv'
	FIELDS TERMINATED BY ','
	LINES TERMINATED BY '\n';
```

Then, convert it to `npy` in `python` (make sure you run `python` under a user with write permissions to the folder where you create the `npy` file):
```
>>> import numpy as np
data = np.genfromtxt('/var/lib/mysql-files/glade_2.3_RA_Dec.csv', delimiter=",")
np.save('glade_2.3_RA_Dec.npy', data)
```
and move the 'glade_2.3_RA_Dec.npy' file to the catalog folder you defined in `config.ini`.

## Result table

### lvc_galaxies table

This table will be used to store the ranked galaxies for each recieved LVC event. To create it, run in `mysql`:
```
mysql> CREATE TABLE `lvc_galaxies` (
	`id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
	`voeventid` text NOT NULL,
	`gladeid` varchar(45),
	`score` text,
	PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
```

## Removing data from the tables
If you want to remove everything from a table, run:
```
mysql> DELETE FROM tablename;
```
If you want to reset the ID auto increment for a table  (e.g. to reset the `voeventid` in the `voevent_lvc` table), run:
```
mysql> ALTER TABLE tablename AUTO_INCREMENT = 1;
```

## Print galaxy rankings for a specific VOEvent
To display the galaxy rankings of the last recorded VOEvent, run:
```
mysql> SELECT glade_catalog.glade_id, glade_catalog.ra0, glade_catalog.dec0, glade_catalog.dist, glade_catalog.bmag, lvc_galaxies.score
	FROM lvc_galaxies
	INNER JOIN glade_catalog ON glade_catalog.glade_id = lvc_galaxies.gladeid
	WHERE lvc_galaxies.voeventid = (SELECT MAX(id) from voevent_lvc);
```

or replace `(SELECT MAX(id) from voevent_lvc)` by the requested VOEvent ID for a different event.
