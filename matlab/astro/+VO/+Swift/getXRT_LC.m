function Table = getXRT_LC(Name, Args)
    % get Swift/XRT light curve of a GRB from the Swift UK database
    % Input  : - Char array of GRB name. E.g., '050509A'.
    %            Default is '050509A'.
    %          * ...,key,val,...
    %            'WorkDir' - Directory in which to downlaod the data.
    %                   Default is pwd.
    %            'SaveDir' - Save downloaded directory.
    %                   Default is false.
    % Output : - A table with the Swift/XRT light curve in flux units
    %            (0.3-10 keV).
    % Author : Eran Ofek (Jun 2022)
    % Example: T = VO.Swift.getXRT_LC
   
    arguments
        Name              = '050509A';
        Args.WorkDir      = pwd;
        Args.SaveDir      = false;
    end
    
    PWD = pwd;
    cd(Args.WorkDir)
    
    % add GRB to name / if not exist
    if contains(Name,'GRB')
        Name = Name(4:end);
        if strcmp(Name(1),' ')
            Name = Name(2:end);
        end
    end
    Name = sprintf('GRB %s',Name);
            
    
    IndexURL = 'https://www.swift.ac.uk/xrt_curves/grb.list';
    
    PageIndex = urlread(IndexURL);
    
    IndexTable = regexp(PageIndex,'(?<Name>GRB \d+.)\s(?<Index>\d+)','names');

    Flag = strcmp({IndexTable.Name}, Name);
    Id   = IndexTable(Flag).Index;
    if isempty(Id)
        error('GRB was not found in Swift index table');
    end
    
    URL = sprintf('https://www.swift.ac.uk/xrt_curves/$targetID/lcfiles_%s.zip',Id);
    www.wget(URL, 'OutputFile',Id);
    unzip(Id);
    cd(Id);
    
	% @Deploy	
    Table = io.files.readtable1('flux_plain.dat','Delimiter','\t','NumHeaderLines',9);
    
    if ~Args.SaveDir
        % delete dir
        cd ..
        system(sprintf('rm -rf %s',Id));
    end
    
    
    
    cd(PWD);
    
end
