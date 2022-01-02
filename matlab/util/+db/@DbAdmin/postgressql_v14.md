# PostgreSQL V14


### Installation

Installation instructions for Linux:


https://techviewleo.com/how-to-install-postgresql-database-on-ubuntu/



	sudo apt update && sudo apt upgrade
	sudo apt -y install gnupg2 wget vim
	sudo apt-cache search postgresql | grep postgresql
	sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
	wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc
	sudo apt -y update
	sudo apt -y install postgresql-14


Verify the installed PostgreSQL version


	sudo -u postgres psql -c "SELECT version();"


### Connect

Password: Passw0rd


	psql -h ubuntu -p 5432 -U admin -W -d template1
	
	
### Create database on remote server

	
	psql -h ubuntu -p 5432 -U admin -W -d postgres -f unittest.sql
	

### JDBC Driver

https://jdbc.postgresql.org/download.html

