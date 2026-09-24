function Result = unitTest
    % unitTest for FileManager
    % Example: FileManager.unitTest
    
    FM = FileManager;
    FM.FileName = 'try1';
    FM.isOpen;
    FM.open;
    if ~FM.isOpen
        error('Problem with opening file');
    end
    FM.writeLine('bla bla bla');
    FM.writeLine('bla bla bla','LevelStr','[DBG]');
    FM.writeLine('bla bla bla','LineFormat','%20s');
    FM.close;         
    FM.deleteFiles;
   
    Result = true;
end