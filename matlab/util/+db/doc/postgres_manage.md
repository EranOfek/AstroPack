#

### Elephant Shed

https://elephant-shed.io/project/ubuntu/

Install on Ubuntu 18.04

	# Install tools
	sudo apt-get install curl ca-certificates

	# Use official PostgreSQL repo, apt.postgresql.org
	echo "deb http://apt.postgresql.org/pub/repos/apt/ bionic-pgdg main" | sudo tee -a /etc/apt/sources.list.d/pgdg.list
	curl https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -

	# Use credativ repo, packages.credativ.com
	echo "deb https://packages.credativ.com/public/postgresql/ bionic-stable main" | sudo tee -a /etc/apt/sources.list.d/elephant-shed.list
	curl https://packages.credativ.com/public/postgresql/aptly.key | sudo apt-key add -

	# Install elephant-shed
	sudo apt-get update
	sudo apt-get install elephant-shed

	# Choose desired PostgreSQL versions to install
	sudo apt-get install postgresql-13

	# Every user in the group "elephant-shed" is allowed to login at the portal
	# Add all needed users to this group
	sudo adduser <USERNAME> elephant-shed

