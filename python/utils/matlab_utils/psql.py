
import os, sys, json, yaml

ASTROPACK_PATH = os.getenv('ASTROPACK_PATH')


# Helper function for dict2obj()
class dict2obj_hook(object):
    def __init__(self, dict_):
        self.__dict__.update(dict_)


# Convert YAML object to Python object
# json.dumps() function converts a Python object into a json string.
#
# json.loads() method can be used to parse a valid JSON string and convert it
# into a Python Dictionary. It is mainly used for deserializing native string,
# byte, or byte array which consists of JSON data into Python Dictionary.
#
# object_hook is an optional function that will be called with the result of
# any object literal decoded (a dict). The return value of object_hook will be
# used instead of the dict. This feature can be used to implement custom
# decoders (e.g. JSON-RPC class hinting).
#
def dict2obj(d):
    json_obj = json.dumps(d)
    python_obj = json.loads(json_obj, object_hook=dict2obj_hook)
    return python_obj
    #return json.loads(json.dumps(d), object_hook=dict2obj_hook)


# Convert YAML object to Python object
def yml_to_obj(yml):
    return dict2obj(yml)

# Convert YAML file to object
def yml_file_to_obj(yml_filename):
    obj = {}
    with open(yml_filename, 'r') as f:
        try:
            yml = yaml.safe_load(f)
            obj = dict2obj(yml)
            return obj
        except yaml.YAMLError as ex:
            print(ex)

    return obj



# Configuration class, based on YML files
# Required packages: yaml
class Config(Base):

    def __init__(self):
        super().__init__()
        self.name = 'Config'
        self.filename = ''
        self.yml = None
        self.data = None


    def load(self, filename=''):

        if filename == '':
            path = os.getenv('ULTRASAT_PATH')
            if not path or path == '':
                path = 'd:/ultrasat/ultrasat.git/python/gcs/'

            if sys.platform == "win32":
                filename = os.path.join(path, 'python/gcs/gcs_conf_win.yml')
            else:
                filename = os.path.join(path, 'python/gcs/gcs_conf.yml')

            print('Config.load: %s' % filename)

            self.filename = filename
            self.data = yml_file_to_obj(filename)


    # Singleton
    config_ = None
    @staticmethod
    def get_config():
        if not Config.config_:
            config_ = Config()
        return config_


    # Unit test
    @staticmethod
    def unit_test(self):
        return True




def run_psql():


'''


        function Result = runPsql(Obj, Args)
            % Run 'psql' external utility with command line parameters.
            % Input:   XlsFileName
            % Output:
            % Example: db.DbAdmin.runPsql(
            % psql -h gauss -p 5432 -U admin -W -d postgres -f unittest.sql
            arguments
                Obj
                Args.Host          = ''        %
                Args.Port          = 0         %
                Args.DatabaseName  = ''        % Use 'postgres' to when creating databases or for general
                Args.UserName      = ''        %
                Args.Password      = ''        %
                Args.SqlFileName   = ''        %
                Args.Params        = ''        %
                Args.TableName     = ''        %
            end

            Result = false;

            if isempty(Args.Host)
                Args.Host = Obj.Host;
            end

            if Args.Port == 0
                Args.Port = Obj.Port;
            end

            if isempty(Args.DatabaseName)
                Args.DatabaseName = Obj.DatabaseName;
            end

            if isempty(Args.UserName)
                Args.UserName = Obj.UserName;
            end

            if isempty(Args.Password)
                Args.Password = Obj.Password;
            end

            try

                % Prepare command line
                Cmd = sprintf('psql -h %s -p %d -U %s -w', Args.Host, Args.Port, Args.UserName);

                % -d
                if ~isempty(Args.DatabaseName)
                    Cmd = sprintf('%s -d %s', Cmd, Args.DatabaseName);
                end

                % -f
                if ~isempty(Args.SqlFileName)
                    Cmd = sprintf('%s -f %s', Cmd, Args.SqlFileName);
                end

                % Additional params
                if ~isempty(Args.Params)
                    Cmd = sprintf('%s %s', Cmd, Args.Params);
                end

                % Password
                if ~isempty(Args.Password)

                    % Windows - note that we MUST NOT have a spaces next to '&&'
                    if tools.os.iswindows()
                        Cmd = sprintf('set PGPASSWORD=%s&&%s', Args.Password, Cmd);

                    % Linux - use 'export'
                    else

                        % Check which shell we use
                        if isempty(Obj.Shell)
                            Obj.Shell = getenv('SHELL');
                        end

                        % bash / tcsh
                        if contains(Obj.Shell, 'tcsh')
                            Cmd = sprintf('setenv PGPASSWORD ''%s'' ; %s', Args.Password, Cmd);
                        else
                            Cmd = sprintf('export PGPASSWORD=''%s'' ; %s', Args.Password, Cmd);
                        end
                    end
                end

                io.msgLog(LogLevel.Info, 'psql: system( %s )', Cmd);
                [Status, Output] = system(Cmd);
                io.msgLog(LogLevel.Info, 'psql: %d', Status);
                io.msgLog(LogLevel.Info, 'psql: %s', Output);
                Result = true;
            catch Ex
                io.msgLogEx(LogLevel.Info, Ex, 'psql');
            end
        end

    end



'''