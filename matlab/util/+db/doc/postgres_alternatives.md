# Top 10 Databases to Use in 2021

This research is related to storing our Sources table which is expected to
be 10^11 rows (50 double columns).

As we faced performance issues related to indexing, here are some
alternatives to Postgres for such big table with indexing.


https://towardsdatascience.com/top-10-databases-to-use-in-2021-d7e6a85402ba

https://en.pingcap.com/blog/how-to-efficiently-choose-the-right-database-for-your-applications/


### Best database and table design for billions of rows of data

https://dba.stackexchange.com/questions/188667/best-database-and-table-design-for-billions-of-rows-of-data


This is exactly what I do every day, except instead of using 
the hourly data, I use the 5 minute data. 
I download about 200 million records everyday, so the amount you 
talk about here is not a problem. 
The 5 minute data is about 2 TB in size and I have weather data going 
back 50 years at an hourly level by location. So let me answer you 
questions based on my experience:

    Don't use NoSQL for this. The data is highly structured and fits a relational database perfectly.
    I personally use SQL Server 2016 and I have no problems applying computations across that volume of data. It was originally on a PostgreSQL instance when I started my job and it couldn't handle the volume of data as it was on a small AWS instance.
    I would highly recommend extracting the hour portion of the date and storing it separate from the date itself. Believe me, learn from my mistakes!
    I store the majority of data list-wise (DATE,TIME,DATAPOINT_ID,VALUE) but that is not how people will want to interpret the data. Be prepared for some horrendous queries against the data and vast amounts of pivoting. Don't be afraid to create a de-normalized table for result sets that are just too large to compute on the fly.


### Binary Files

Check solution for large tables (10^12) - use binary data file + on-disk b-tree indexing

http://people.csail.mit.edu/jaffer/WB

http://abiusx.com/me/code/wb/

Python

https://github.com/NicolasLM/bplustree



