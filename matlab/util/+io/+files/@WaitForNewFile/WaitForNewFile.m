% io.files.WaitForNewFile class
% A class for fast recursive search of files with some criteria, including files created after some reference file.
%       
%
% Examples:
% % Create an object:
% WF=io.files.WaitForNewFile;     
%
% % set the file template name:
% WF.Template = 'LAST*_coadd_*.fits';
%
% % create a TouchFile with current time stamp
% WF.createTouchFile(false);  % the argument means that if file exist, then do not update
%
% % find all files (ignore touch file)
% F=WF.findFiles('IgnoreTouch',true);  % return a sorted by time list
%
% % find all files created after TouchFile
% F=WF.findFiles('IgnoreTouch',false); % return a sorted by time list
%
% % set the TouchFile/Newer to the file #1 in WF.NewFiles
% WF.setNewer(1);
%


classdef WaitForNewFile < Component
    
    
    properties
        NewFiles = [];
        
        BasePath = '.';
        Template = 'LAST*.fits';
    end
    
    properties (Hidden)
        Newer                    = [];
        TouchFileExist logical   = false;
        TouchFile                = '.temp.WaitForNewFile';
    end
    
    methods % constructor
        function Obj=WaitForNewFile()
            % Constractor for io.files.WaitForNewFile class
            
            Obj.Newer   = [];
            deleteTouchFile(Obj);
            
        end
        
        
    end
    
    methods % setters/getters
        
    end
    
    methods (Static)  % static functions
        function [Flag,CleanDir]=dirConstraint(Dir, Args)
            % Given a dir structure (output of dir function) apply filters
            % Input  : - A dir structure.
            %            Default is to execute the function dir on current
            %            location.
            %          * ...,key,val,...
            %            'IsFile' - Select only files. Default is true.
            %            'MinJD' - Min file JD or [D M Y H M S].
            %                   Default is -Inf.
            %            'MaxJD' - Max file JD or [D M Y H M S].
            %                   Default is Inf.
            %            'MinSize' - Min file size in bytes.
            %                   Default is -1.
            %            'MaxSize' - Max file size in bytes.
            %                   Default is Inf.
            % Output : - An array of logicals indicating if eacg file in
            %            dir strycture satisfy the constraints.
            %          - A dir structure with the selected entries.
            % Author : Eran Ofek (Jul 2024)
            % Example: io.files.WaitForNewFile.dirConstraint
            
           
            arguments
                Dir                   = dir;
                Args.IsFile logical   = true;
                Args.MinJD            = -Inf;
                Args.MaxJD            = Inf;
                Args.MinSize          = -1;
                Args.MaxSize          = Inf;
                
                %Args.MinFileNameJD    = [];
                %Args.MaxFileNamedate  = [];
            end
            
            Size  = [Dir.bytes];
            IsDir = [Dir.isdir];
            JD    = [Dir.datenum] + celestial.time.julday() - datenum(now);            
            
            if numel(Args.MinJD)>1
                Args.MinJD = celestial.time.julday(Args.MinJD);
            end
            if numel(Args.MaxJD)>1
                Args.MaxJD = celestial.time.julday(Args.MaxJD);
            end
                
            %if ~isempty(Args.MinFileNameJD) || ~isempty(Args.MaxFileNameJD)
            %    if numel(Args.MinFileNameJD)>1
            %        Args.MinFileNameJD = celestial.time.julday(Args.MinFileNameJD);
            %    end
            %    if numel(Args.MaxFileNameJD)>1
            %        Args.MaxFileNameJD = celestial.time.julday(Args.MaxFileNameJD);
            %    end
            %end
            
            Flag = Size>Args.MinSize & Size<Args.MaxSize & JD>Args.MinJD & JD<Args.MaxJD;
            
            if Args.IsFile
                Flag = Flag & ~IsDir;
            end
            
            if nargout>1
                CleanDir = Dir(Flag);
            end
                        
        end
        
    end
    
    
    methods % basic/internal functionality
        function deleteTouchFile(Obj)
            % Delete TouchFile if exist
           
            PWD = pwd;
            cd(Obj.BasePath);
            if isfile(Obj.TouchFile)
                delete(Obj.TouchFile);
            end
            Obj.TouchFileExist = false;
            cd(PWD);
            
        end
        
        function createTouchFile(Obj, Update)
            % create or update the touch file
            % Input  : - self.
            %          - A logical indicating if to update the file (if
            %            exist). Default is false.
            % Author : Eran Ofek (Jul 2024)
            % Example: WF=io.files.WaitForNewFile; WF.createTouchFile
            arguments
                Obj
                Update logical   = false;
            end
            
            PWD = pwd;
            cd(Obj.BasePath);
            if Update || ~isfile(Obj.TouchFile)
                [Stat]=system(sprintf('touch %s',Obj.TouchFile));
                if Stat==0
                    Obj.TouchFileExist = true;
                end
            end
            cd(PWD);
                
        end
    
        function Obj=setNewer(Obj, Ind)
            % Set the newer file to an entry in the NewFiles list.
            %   Can set the file by its index in the NewFiles property, or
            %   by a full file name.
            %   The newer property is used by the findFiles to look for
            %   files created after the "Newer" file.
            % Input  : - self.
            %          - Either file index in the NewFiles property, or a
            %            full file name.
            % Output : - self.
            % Author : Eran Ofek (Jul 2024)
            % Example: WF.setNewer(1)
            
            if isempty(Ind)
                Obj.Newer = [];
            else
                if isnumeric(Ind)
                    if Ind>numel(Obj.NewFiles)
                        error('Index (%d) is larger than the entries in NewFiles (%d)',Ind,numel(Obj.NewFiles));
                    end

                    Obj.Newer = fullfile(Obj.NewFiles(Ind).folder, Obj.NewFiles(Ind).name);
                else
                    Obj.Newer = Ind;
                end
            end
        end
    end
    
    methods % main functions
        function F=findFiles(Obj, Args)
            % Find files using various criteria
            %   Criterai including:
            %   File were not modified in the past Args.NotModified minutes
            %   Were created after the TouchFile
            %   Size is in some range.
            % Input  : - self.
            %          * ...,key,val,...
            %            'IgnoreTouch' - If true, then will ignore the
            %                   TouchFile and will look for all files.
            %                   If false, them will look only for files
            %                   created after the TouchFile.
            %                   Default is false.
            %            'IgnoreHidden' - Ignore hidden files.
            %                   Default is true.
            %            'NotModified' - If not 0, then check that the file
            %                   was not modified in the past X minutes.
            %                   Default is 0.1 (minutes).
            %            'MinSize' - Min. file size [bytes].
            %                   Default is 1e5.
            %            'MaxSize' - Max. file size [bytes].
            %                   Default is 1e7.
            %            'SortByTime' - A logical indicating if to sort the
            %                   output structure by time. Default is true.
            % Output : - A dir structure with all the found files.
            %            This will also be stored in the NewFiles property.
            % Author : Eran Ofek (Jul 2024)
            % Example: WF=io.files.WaitForNewFile;
            %          F=WF.findFiles('IgnoreTouch',true);
            
            arguments
                Obj
                Args.IgnoreTouch logical     = false;
                Args.IgnoreHidden logical    = true;
                Args.NotModified             = 0.1;
                Args.MinSize                 = 1e5;
                Args.MaxSize                 = 1e7;
                Args.SortByTime logical      = true;
            end
        
            if ~(isunix || ismac)
                error('io.files.WaitForNewFile class works only in linux based machines');
            end
            
            PWD = pwd;
            cd(Obj.BasePath)
            
            if isempty(Obj.Newer)
                Obj.createTouchFile(false);  % do not update
                NewerFile = Obj.TouchFile;
            else
                NewerFile = Obj.Newer;
            end
            
            if Args.IgnoreHidden
                Extra = "-not -path '*/.*'";
            else
                Extra = "";
            end
            if ~isempty(Obj.Template)
                ExtraName = sprintf("-name '%s'",Obj.Template);
            else
                ExtraName = "";
            end
            if Args.NotModified>0
                % make sure that the file was not modified in the last few
                % seconds
                ExtraMod = sprintf("-not -mmin %f",Args.NotModified);
            else
                ExtraMod = "";
            end
            if ~isempty(Args.MinSize)
                ExtraMinSize = sprintf("-size +%dc",Args.MinSize);
            else
                ExtraMinSize = "";
            end
            if ~isempty(Args.MaxSize)
                ExtraMaxSize = sprintf("-size -%dc",Args.MaxSize);
            else
                ExtraMaxSize = "";
            end
            
            
            
            if Args.IgnoreTouch
                [Stat, Ans] = system(sprintf("find . %s %s %s %s %s",Extra, ExtraName, ExtraMod, ExtraMinSize, ExtraMaxSize));
            else
                [Stat, Ans] = system(sprintf("find . %s %s %s %s %s -newer %s",Extra, ExtraName, ExtraMod, ExtraMinSize, ExtraMaxSize, NewerFile));
            end
            
            %find . -not -path '*/.*' -type f -name '*some text*'
            
            SpAns = splitlines(Ans);
            SpAns = SpAns(1:end-1);
            Nsp   = numel(SpAns);
            F = struct('name',cell(Nsp,1), 'folder',cell(Nsp,1), 'date',cell(Nsp,1), 'bytes',cell(Nsp,1), 'isdir',cell(Nsp,1), 'datenum',cell(Nsp,1));
            
                
            for Isp=1:1:Nsp
                F(Isp) = dir(SpAns{Isp});
            end
            
            if Args.SortByTime
                F = io.files.dirSortedByDate(F);
            end
              
            Obj.NewFiles = F;
            
            cd(PWD);
        end
    end
    
    
    methods (Static)  % in other files / unitTest
        Result = unitTest
    end 

end
    
