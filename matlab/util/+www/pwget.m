function Files=pwget(Links, Extra, MaxGet, Args)
% Parallel wget to retrieve multiple files simultanously
% Package: www
% Description: Parallel wget function designed to retrieve multiple files
%              using parallel wget commands.
%              If fast communication is available, running several wget
%              commands in parallel allows almost linear increase in the
%              download speed.
%              After exceuting pwget.m it is difficult to kill it. In
%              order to stop the execuation while it is running you
%              have to create a file name 'kill_pwget' in the directory
%              in which pwget is running (e.g., "touch kill_pwget").
% Input  : - Cell array of URL file links to download.
%            Alterantively a URL string.
%          - Additional string to pass to the wget command
%            e.g., '-q'. Default is empty string ''.
%          - Maxium wget commands to run in parallel.
%            Default is 5.
%          * ...,key,val,...
%            'OutFiles' - A cell array of output file names.
%                   Default is {}.
%            'BaseURL' - An optional URL base to concatenate to the begining of each
%                   link. This is useful if the Links cell array contains only
%                   relative positions. Default is empty string ''.
%                   If empty matrix then use default.
%            'UseFun' - Use function: 'wget'|'urlwrite'. Default is 'wget'.
% Output : Original names of retrieved files.
% Tested : Matlab 2012a
%     By : Eran O. Ofek                    Oct 2012
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Speed  : On my computer in the Weizmann network I get the following
%          results while trying to download 20 corrected SDSS fits images:
%          MaxGet=1  runs in 83 seconds
%          MaxGet=2  runs in 41 seconds
%          MaxGet=5  runs in 19 seconds
%          MaxGet=10 runs in 9 seconds
%          MaxGet=20 runs in 6 seconds
% Example: tic;www.pwget(Links,'',10);toc
%          % sometime wget will do nothing because the URL is untruste - in this case use:
%          www.pwget(Links,'--no-check-certificate -U Mozilla',10);
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    Links
    Extra          = '';
    MaxGet         = 5;
    Args.OutFiles  = {};
    Args.BaseURL   = '';
    Args.UseFun    = 'wget';
end
    
UseFun  = Args.UseFun;
BaseURL = Args.BaseURL;



% convert to a cell array
if (~iscell(Links))
    Links = {Links};
end

% remove empty links
Links = Links(~tools.cell.isempty_cell(Links));

if ~isempty(Args.OutFiles) && (numel(Args.OutFiles)~=numel(Links))
    error('Number of valid links must be equal to the number of files');
end
Files = Args.OutFiles;

Nlink = length(Links);
Nloop = ceil(Nlink./MaxGet);
Nget  = ceil(Nlink./Nloop);


Files = cell(Nlink,1);
Abort = false;
Ifile = 0;
for Iloop=1:1:Nloop
    
    switch lower(UseFun)
        case 'wget'
            % Use Linux wget
            Str = '';
            for Iget=1:1:Nget
              Ind = Nget.*(Iloop-1) + Iget;
              if (Ind<=Nlink)
                 if (~isempty(Str))
                     Str = sprintf('%s &',Str);
                 end
                 wget = 'wget';
                 
                 % Under Windows try to use wget.exe from our repo
                 if tools.os.iswindows
                    MyFileName = mfilename('fullpath');
                    [MyPath, ~, ~] = fileparts(MyFileName);
                    wget = fullfile(MyPath, '../../../deployment/install/windows/wget.exe');
                    if ~isfile(wget)
                        wget = 'wget';
                    end
                 end
                 
                 if isempty(Args.OutFiles)
                    Str = sprintf('%s %s %s "%s%s"',Str,wget,Extra,BaseURL,Links{Ind});
                 
                    Split=regexp(Links{Ind},'/','split');
                    Files{Ind} = Split{end};
                 else
                    Str = sprintf('%s %s %s -O %s "%s%s"',Str,wget,Extra,Args.OutFiles{Ind}, BaseURL,Links{Ind});
                 end
              end
           end
           % Check if user requested to kill process
           if (exist('kill_pwget','file')>0 || Abort)
               % Abort (skip wget)
               delete('kill_pwget');
               Abort = true;
           else
               
              %io.msgLog(LogLevel.Debug, 'pwget: %s', Str);
              [Stat,Res]=system(Str);
           end
        case 'urlwrite'
            % use matlab urlwrite
            error('urlwrite option not implemented yet');
            
        otherwise
            error('Unknown UseFun option');
    end
            
end

% check that all files arrived
if (Nlink>1)
    io.files.files_arrived(Files(end));
    
%     Arrived = false;
%     Dir1 = Util.files.dir_cell(Files);
%     while ~Arrived    
%         % check if all files arrived
%         pause(0.2);
%         Dir2 = Util.files.dir_cell(Files);
% 
%         if (all(([Dir1.bytes] - [Dir2.bytes])==0))
%             Arrived = true;
%         end
% 
%         Dir1 = Dir2;
%     end
%     pause(0.2);
end
