function conn = postgresql(user, pw, varargin)
%POSTGRESQL  Octave test double for the Database Toolbox function (see MockPgConn).
p = inputParser;
p.addParameter('Server', 'localhost');
p.addParameter('PortNumber', 5432);
p.addParameter('DatabaseName', '');
p.parse(varargin{:});
conn = MockPgConn();
conn.User = user;
conn.Password = pw;
conn.Server = p.Results.Server;
conn.Port = p.Results.PortNumber;
conn.Database = p.Results.DatabaseName;
end
