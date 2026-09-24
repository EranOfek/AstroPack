function URL=wget_obsid(ObsID, Args)
% Get all the files associated with a Chandra ObsID
% Package: VO.Chandra
% Description: Get all the files associated with a Chandra ObsID
%              The Chandra observations catalog is stored in
%              cats.X.ChandraObs.
%              Use VO.Chandra.build_obsid_cat to build catalog.
% Input  : - ObsID
%          * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
%            'BasePath' - Directory in which to copy the data.
%                   If empty, then use current dir.
%                   Default is [].
%            'Unzip' - Logical indicating if to gunzip files.
%                   Default is false.
%            'Npwget' - Number of files to download simoltanously.
%                   Default is 10.
% Output : null
% Tested : Matlab R2014a
%     By : Eran O. Ofek                    Jan 2015
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: VO.Chandra.wget_obsid(366)
%          Cat = VO.Chandra.buildChandraObsIDcat();
%          VO.Chandra.wget_obsid(366,'Cat',Cat);
%          VO.Chandra.wget_obsid(Cat.obsid,'Cat',Cat);
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    ObsID
    Args.BasePath        = [];
    Args.Unzip           = false;
    Args.Npwget          = 10;
    Args.RemoveBase      = 'https://cxc.cfa.harvard.edu/cdaftp/science/';
    
    Args.Cat             = [];
    Args.SkipExist       = true;
    Args.Instrument      = {'ACIS-S','ACIS-I'};
    %Args.MinExpTime      = 500;
end

PWD = pwd;
%Cat = cats.X.ChandraObs;
if isempty(Args.Cat)
    Cat = VO.Chandra.buildChandraObsIDcat();
else
    Cat = Args.Cat;
end

% Search ObsID
%Iobs = find([Cat.ObsID]==ObsID);
%Iobs  = find(Cat.Cat.ObsID==ObsID);
%URL  = Cat(Iobs).URL;
%URL   = Cat.Cat.URL{Iobs};


Nobsid = numel(ObsID);
for I=1:1:Nobsid

    Iobs = find(Cat.obsid==ObsID(I));
    
    BaseStr = sprintf('ao%02d/cat%d/%d/', Cat.AO(Iobs), Cat.CatNum(Iobs), ObsID(I));
    % old format
    %URL = sprintf('%s%s', Args.RemoveBase, BaseStr);
    % new format
    URL = sprintf('https://cxc.cfa.harvard.edu/cdaftp/byobsid/%d/%d/', ...
              mod(Cat.obsid(I), 10), Cat.obsid(I));
    %e.g., https://cxc.cfa.harvard.edu/cdaftp/science/ao20/cat7/21421/
    
    IsInst = ismember(Cat.instrument(Iobs), Args.Instrument);
    ExpTime = str2double(Cat.exposure_time(Iobs));
    IsArchived  = Cat.status(Iobs) == "archived";
    
    % if numel(char(Cat.start_date(Iobs)))>4    
    %     if str2double(extractBefore(Cat.start_date(Iobs),5))==2024
    %         'a'
    %     end
    % end

    BaseStrOIF = sprintf('%soif.fits',BaseStr);
    if isfolder(BaseStr) && isfile(BaseStrOIF)
        % skip
        fprintf('%s exist\n',BaseStr)
    else
        if IsInst && IsArchived

            fprintf('Downloading to: %s\n',BaseStr);

            [List,IsDir,FileName]=www.r_files_url(URL);
                
            cd('/raid/eran/projects/transients/x/chandra/ObsID');

            PWD = pwd;
            tools.os.cdmkdir(BaseStr);

            BasePath = pwd;

            % if isempty(Args.BasePath)
            %     BasePath = pwd;
            % else
            %     BasePath = Args.BasePath;
            %     cd(BasePath);
            % end
            
            
            
            
            RelFiles = regexprep(List, Args.RemoveBase, '');
            Dirs     = fileparts(RelFiles);
            UniqueDirs = unique(Dirs);
            UniqueDirsCl = tools.os.removeFirstNdirsKeepLast(UniqueDirs, 7);

            Nun = numel(UniqueDirs);
            for Iun=1:1:Nun
                Flag = strcmp(Dirs, UniqueDirs{Iun});
                
                cd(BasePath);
                
                if ~isempty(UniqueDirsCl{Iun})
                    mkdir(UniqueDirsCl{Iun});
                    cd(UniqueDirsCl{Iun});
                end
                www.pwget(List(Flag), '--no-check-certificate -U Mozilla', Args.Npwget);
                
                
                if Args.Unzip
                    pause(30);
                    %gunzip('*.gz');
                    system('gzip -d *.gz');
                end
            end
            cd(PWD);
        end
    end
end

cd(PWD);                        


if 1==0

% check if directory is populated
if (exist('primary','dir')>0 && ~InPar.ReGet)
    % do not get files again
else

    % get file names
    [List,IsDir,FileName]=www.r_files_url(URL);
    switch lower(InPar.Download)
        case 'evt'
            Flag = ~tools.cell.isempty_cell(regexp(List,'_evt2.fits','match'));
            List = List(Flag);
        case 'primary'
            Flag1 = ~tools.cell.isempty_cell(regexp(List,'/primary/','match'));
            Flag2 = ~tools.cell.isempty_cell(regexp(List,'/oif.fits','match'));
            Flag  = Flag1 | Flag2;
            List = List(Flag);
        case 'all'
            % keep List as is
        otherwise
            error('Unknown Download option');
    end

    Nl = numel(List);
    ObsIDstr = sprintf('%d',ObsID);
    
    % download
    switch lower(InPar.Output)
        case 'none'
            % do nothing
            
        case 'flat'
            www.pwget({List.URL},InPar.Extra,InPar.MaxGet);        
            if (InPar.Ungzip)
                system('gzip -d *.gz');
            end
        case {'dir_obsid','dir_full'}
            
            switch lower(InPar.Output)
                case 'dir_obsid'
                    if exist(ObsIDstr,'dir')==0
                        mkdir(ObsIDstr);
                        cd(ObsIDstr);
                        DirExist = false;
                    else
                        DirExist = true;
                    end
                    
                case 'dir_full'
                    ListTmp = regexprep(List,InPar.BaseURL,'');
                    SplitTmp = regexp(ListTmp,'/','split');
                    DirAO = SplitTmp{1}{1};
                    DirCat = SplitTmp{1}{2};
                    
                    mkdir(DirAO);
                    cd(DirAO);
                    mkdir(DirCat);
                    cd(DirCat);
                    if exist(ObsIDstr,'dir')==0
                        mkdir(ObsIDstr);
                        cd(ObsIDstr);
                        DirExist = false;
                    else
                        DirExist = true;
                    end    
                    
                otherwise
                    error('Impossible error');
            end
            
            
            if ~DirExist
                % only if ObsIDstr dir doesn't exist
                MatchP = sprintf('%sao\\d\\d/cat\\d/\\d.{1,5}/primary/*',InPar.BaseURL);
                MatchS = sprintf('%sao\\d\\d/cat\\d/\\d.{1,5}/secondary/*',InPar.BaseURL);
                Fp     = ~tools.cell.isempty_cell(regexp(List,MatchP,'match'));
                Fs     = ~tools.cell.isempty_cell(regexp(List,MatchS,'match'));
                Ff     = ~Fp & ~Fs;
                if (sum(Ff)>0)
                    www.pwget(List(Ff),InPar.Extra,InPar.MaxGet);
                    if InPar.UnGzip
                        system('gzip -d *.gz');
                    end
                end
                if (sum(Fp)>0)
                    mkdir('primary');
                    cd('primary');
                    www.pwget(List(Fp),InPar.Extra,InPar.MaxGet);
                    if InPar.UnGzip
                        system('gzip -d *.gz');
                    end
                    cd ..
                end
                if (sum(Fs)>0)
                    mkdir('secondary');
                    cd('secondary');
                    www.pwget(List(Fs),InPar.Extra,InPar.MaxGet);
                    if InPar.UnGzip
                        system('gzip -d *.gz');
                    end
                    cd ..
                end

            end
            
       otherwise
            error('Unknown Output option');
    end

end  


end


