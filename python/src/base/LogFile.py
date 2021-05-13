# Basic log file
                                   
class LogFile:

    # Constructor for LogFile
    def __init__(self):
        self.FileName = ''
        self.UserData = ''
        self.LogPath = ''; #"C:\\_Ultrasat\\log";


        function Obj = LogFile(FileName)

            
            arguments
                FileName = 'Default';
            end
            fn = sprintf('%s-%s.log', Obj.getFileNameTimestamp(), FileName);
            Obj.FileName = fullfile(Obj.LogPath, fn);
            Obj.write('=========================================== Started');


    # Log text line to file
    def write(self, varargin)
        Result = write2(Obj, '', varargin{:});


    # Log text line to file
    def write2(Obj, Title, varargin):

        if isempty(Title)
            Prompt = sprintf('%s > ', Obj.getTimestamp());
        else
            Prompt = sprintf('%s > %s ', Obj.getTimestamp(), Title);
        end

        Fid = fopen(Obj.FileName, 'at');
        fprintf(Fid, Prompt);
        fprintf(Fid, varargin{:});
        fprintf(Fid, '\n');
        fclose(Fid);
        Result = true;


    # Return singleton object
    def getSingleton():

        persistent PersObj
        if isempty(PersObj):
            PersObj = LogFile('');

        Result = PersObj;


    # Return current time as string
    def getTimestamp():
        Result = datestr(now, 'yyyy-mm-dd HH:MM:SS.FFF');

    def  getFileNameTimestamp():
        Result = datestr(now, 'yyyy-mm-dd');



    def unitTest():
        fprintf('LogFile test started\n');
        Lf = LogFile.getSingleton();
        Lf.write('LogFile test started');

        for i=1:1:3
            Lf.write('Line: %d', i);
        end

        Lf.write('LogFile test passed');
        fprintf('LogFile test passed\n');
        Result = true;
