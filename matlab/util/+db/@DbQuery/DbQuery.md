# Overview

% DbQuery - SQL Database query
%
% This class provides SQL functionality, currently tested with PostgreSQL v13.
%
% Functionality
%
% Related Classes:
%   DbDriver - Internally used to load Java package for Postgres
%   DbConnection - Used to connect to specific database on local or remote
%   server.
%   DbRecord - Class with dynamic properties, used as struct to store
%   values of database record.
%
% References:
%   https://www.tutorialspoint.com/java-resultset-movetoinsertrow-method-with-example
%
% Unit-Test:
%   Use unittest__tables from GDrive to test
%



# Usage



# Examples

### Example 1: Basic usage


    Q = db.DbQuery('unittest:master_table');
    Q.selectCount()
	
	
# Database Configuration File

Database.yml


# See Also

# Notes
