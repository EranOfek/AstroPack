function St = openVisitStatusDB(varargin)
%OPENVISITSTATUSDB  Construct StatusDB for LAST insertDB on Euclid.
%   St = pipeline.last.insertDB.status.openVisitStatusDB()
%   St = pipeline.last.insertDB.status.openVisitStatusDB(Args)  % struct from insertArchive*_pg
%   St = pipeline.last.insertDB.status.openVisitStatusDB('PgServer','euclid', ...)

p = inputParser;
p.KeepUnmatched = true;
p.addParameter('PgServer', "euclid");
p.addParameter('PgPort', 5432);
p.addParameter('PgDatabase', "obs");
p.addParameter('PgUser', "pipeline");
p.addParameter('PgPassKey', "euclid_pg_status");
p.addParameter('PgStatusRoot', "/bigdata2/projects/last/data");
p.addParameter('AstroDBPassFile', "~/.astropack/Passwords.yml");
if nargin == 1 && isstruct(varargin{1})
    p.parse(varargin{1});
else
    p.parse(varargin{:});
end
o = p.Results;

St = pipeline.last.insertDB.status.StatusDB( ...
    'Server', char(o.PgServer), ...
    'Port', o.PgPort, ...
    'Database', char(o.PgDatabase), ...
    'User', char(o.PgUser), ...
    'Root', char(o.PgStatusRoot), ...
    'Depth', 6, ...
    'PasswordPassKey', char(o.PgPassKey), ...
    'AstroDBPassFile', char(o.AstroDBPassFile));
end
