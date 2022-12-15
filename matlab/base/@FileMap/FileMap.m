
classdef FileMap < Component
    %
    %  
    
    % Properties
    properties (SetAccess = public)
              
        % 
        DirList = {};       %
        FileList = {};      %
        Map = [];           % conainers.map
    end
    
    %-------------------------------------------------------- 
    methods  
               
        % Constructor    
        function Obj = FileMap()          
            % Constructor for FileProcessor
            % Input   : - struct array, table, cell array, matrix,
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %             'InputPath' - 
            %             'InputMask' - 
            %             'ProcessedPath' - 
            % Output  : - New instance of FileProcessor object
            % Author  : Chen Tishler (2021)
            % Example : 
            
            Obj.setName('FileMap');
            Obj.Map = containers.Map();
        end
                      
    end

    
    methods  
        
        function Result = addAll(Obj, AScan)
            % 
            % Input   : - Path
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %
            % Output  : - Full file name
            % Author  : Chen Tishler (Dec. 2022)
            % Example : 
            arguments
                Obj
                AScan = false;
            end
            
            Result = '';
            List = strsplit(path, ';');
            for i=1:numel(List)
                Path = List{i};
                if ~startsWith(Path, '/usr/local/MATLAB') && ~contains(Path, '\Matlab\')
                    Obj.add(List{i});
                end
            end
            if AScan
                Obj.scan();
            end                
        end
        
        
        
        function Result = add(Obj, Path)
            % 
            % Input   : - Path
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %
            % Output  : - Full file name
            % Author  : Chen Tishler (Dec. 2022)
            % Example : 
           
            Result = '';
            if ~any(strcmp(Obj.DirList, Path))
                Obj.DirList{end+1} = Path;
            end
        end
        
        
        function Result = findFile(Obj, FileName)
            % 
            % Input   : - File name
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %
            % Output  : - Full file name
            % Author  : Chen Tishler (Dec. 2022)
            % Example : 
           
            Result = '';
            if ~contains(FileName, '/') && ~contains(FileName, '\')            
                if Obj.Map.isKey(FileName)
                    F = Obj.Map(FileName);
                    Result = fullfile(F.folder, FileName);
                else
                end
            else
                Result = FileName;
            end                
        end
        

        function Result = findFile1(Obj, FileName)
            % 
            % Input   : - File name
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %
            % Output  : - Full file name
            % Author  : Chen Tishler (Dec. 2022)
            % Example : 
           
            Result = '';
            if ~contains(FileName, '/') && ~contains(FileName, '\')
                if Obj.Map.isKey(FileName)
                    F = Obj.Map(FileName);
                    if numel(F.folder) == 1
                        Result = fullfile(F.folder{1}, FileName);
                    else
                    end
                else
                end
            else
                Result = FileName;
            end
        end      

        
        function scan(Obj)
            % 
            % Input   : - 
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %
            % Output  : - Full file name
            % Author  : Chen Tishler (Dec. 2022)
            % Example : 
            for i=1:numel(Obj.DirList)
                Obj.scanPath(Obj.DirList{i});
            end
        end
        
        
        function scanPath(Obj, Path)
            
            % Get list of files and folders in any subfolder            
            Files = dir(fullfile(Path, '**\*.*'));
            for i=1:numel(Files)
                if Files(i).isdir == 0
                    fname = Files(i).name;
                    if ~Obj.Map.isKey(fname)
                        F = struct;
                        F.name = fname;
                        F.folder{1} = Files(i).folder;
                        Obj.Map(fname) = F;
                    else               
                        F = Obj.Map(fname);
                        F.folder{end+1} = Files(i).folder;
                        Obj.Map(fname) = F;
                    end
                end
            end
        end        
        
        
        function clear(Obj)
            Obj.DirList = {};
            Obj.Map = containers.Map();
        end
        
    end
    
    
    methods(Static)
        
        function Result = getSingleton()
            % Return singleton object, this is the default log file
            % to be used by current process (or workspace)
            % Input:   -
            % Output:             
            % Example: SysLogFile = LogFile.getSingleton();
            persistent PersObj
            if isempty(PersObj)
                PersObj = FileMap();
            end
            Result = PersObj;
        end
        
    end


    % Unit test
    methods(Static)   
        Result = unitTest()
    end    
        
end
