function [List,IsDir,FileName]=find_urls(URL, Args)
% Read the URL content and extract all the links within the URL
% Package: www
% Description: Given a URL, read the URL content and extract all the links
%              within the URL and return a cell array of all the links.
%              Optionaly, the program can filter URL using regular
%              expressions.
% Input  : - A URL to read.
%          * Arbitrary number of pairs of input arguments ...,key,val,...
%            The following keywirds are available:
%            'strfind'  - select only links which contains a specific
%                         specified string. Default is [].
%                         If empty matrix then do not apply this search.
%            'match'    - Use a regular expression matching.
%                         If empty matrix then do not apply this search.
%            'input'    - Input type of the first argument:
%                         'url' - Assumes that the first input is a URL
%                                 (default).
%                         'file'- Assumes that the first input is an html
%                                 file.
%            'base'     - Base URL (useful if read from file).
%                         Default is empty matrix.
%            'Method'   - Function to use in order to read URL.
%                         'webread'|'urlread'.
%                         Default is 'webread'.
% Output : - A cell array of links found within the URL.
%          - A flag indicating if link is a possible directory.
%            Directories are identified by the '/' sign in the end
%            of the link.
%          - A cell array of file names in the URL name (i.e., string after
%            last "/").
% Tested : Matlab 2011b
%     By : Eran O. Ofek                    Feb 2013
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: List=www.find_urls('http://www.weizmann.ac.il/home/eofek/matlab/');
%          List=www.find_urls(URL,'strfind','.m');
%          List= www.find_urls(URL,'match','http.*?\.m');
%          List= www.find_urls(URL,'match','.*?\.fits');
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    URL
    Args.strfind = [];
    Args.match   = [];
    Args.input   = 'url';
    Args.base    = [];
    Args.User    = '';
    Args.Pass    = '';
    Args.Method  = 'webread';
end



switch lower(Args.input)
    case 'url'
        switch lower(Args.Method)
            case 'webread'
                Options = weboptions('Username',Args.User,'Password',Args.Pass,'Timeout',15);
                Str = webread(URL,Options);
            case 'urlread'
                Str = urlread(URL);
            otherwise
                error('Unknown Method option');
        end
    case 'file'
        Str = io.files.file2str(URL,'str');
    otherwise
        error('Unknown input option');
end

if (~isempty(Args.base))
   BaseUrl = Args.base;
else
   BaseUrl = regexp(Str,'<base href=".*?">','match');
   if (isempty(BaseUrl))
      BaseUrl = URL;
   else
      Ib = strfind(BaseUrl{1},'"');
      BaseUrl = BaseUrl{1}(Ib(1)+1:Ib(2)-1);
   end
   Is = strfind(BaseUrl,'/');
   BaseUrl = BaseUrl(1:Is(end)-1);
end

if (~isempty(strfind(BaseUrl,'http://')))
   Is = strfind(BaseUrl,'/');
   if (length(Is)>2)
      RootURL = BaseUrl(1:Is(3)-1);
   else
      RootURL = BaseUrl;
   end
else
   Is = strfind(BaseUrl,'/');
   RootURL = BaseUrl(1:Is(1)-1);
end



[Tokens,Match] = regexp(Str,'<(a).*?>.*?</\1>','tokens','match');
I=find(tools.cell.isempty_cell(strfind(Match,'href'))==0);
Match = Match(I);

Location = strfind(Match,'href');
N = length(Match);
List = cell(N,1);
for Im=1:1:N
   StrM = Match{Im}(Location{Im}+5:end);
   if strcmp(StrM(1),'"')
       % URL starts with "
       Ind = strfind(StrM,'"');
       if (Ind<2)
          warning('Illegal href link - skip');
       else
          List{Im} = StrM(Ind(1)+1:Ind(2)-1);
       end
   else
       % URL not start with "
       Ind1 = strfind(StrM,' ');
       if (isempty(Ind1))
          Ind1 = Inf;
       end
       Ind2 = strfind(StrM,'>');
       Ind  = min(Ind1,Ind2);
       List{Im} = StrM(1:Ind-1);
   end

   % attache base url to link
   if (~isempty(List{Im}))
      switch List{Im}(1)
       case '/'
          List{Im} = sprintf('%s%s',RootURL,List{Im});
       otherwise
          if (length(List{Im})>4)
             if strcmp(List{Im}(1:4),'http')
                % full link - do nothing
             else
                List{Im} = sprintf('%s/%s',BaseUrl,List{Im});
             end
          else
             List{Im} = sprintf('%s/%s',BaseUrl,List{Im});
          end
      end
   end
end
List = List(tools.cell.isempty_cell(List)==0);

% match/find strings
if (~isempty(Args.strfind))
   List = List(~tools.cell.isempty_cell(strfind(List,Args.strfind)));
end

if (~isempty(Args.match))
   List = List(~tools.cell.isempty_cell(regexp(List,Args.match)));
end

% Check if http is in name:



if (nargout>1)
    % check if link is a directory
    Nlist = numel(List);
    IsDir = false(Nlist,1);
    for Ilist=1:1:Nlist
        if (strcmp(List{Ilist}(end),'/'))
            IsDir(Ilist) = true;
        end
    end
    
    if (nargout>2)
        % extract file name
        RegS = regexp(List,'/','split');
        FileName = cell(Nlist,1);
        for Ilist=1:1:Nlist
            FileName{Ilist} = RegS{Ilist}{end};
        end
    end
        
end
