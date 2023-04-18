
library(DBI)
library(RPostgreSQL)

# library(RODBC)
# library(odbc)
# library(dbplyr)

con = dbCanConnect(dbDriver('PostgreSQL'),
                   dbname = 'Pescart',
                   host = '10.10.8.134',
                   port = '5432',
                   user = 'u1456',
                   password = 'u1456')

con

con =    dbConnect(dbDriver('PostgreSQL'),
                   dbname = 'Pescart',
                   host = '10.10.8.134',
                   port = '5432',
                   user = 'u1456',
                   password = 'u1456')

nomes = dbGetQuery(con,
                   "SELECT TABLE_NAME 
                    FROM INFORMATION_SCHEMA.TABLES")


pescart = dbGetQuery(con,
                     "select * from master.\"IMTVEND0_2002\" as i
                     WHERE i.\"EGRUPART\" = '7' ")
