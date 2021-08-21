function get_data_dir(Args)
    % get data/ dir for the AstroPack matlab package
    % Author : Eran Ofek (Aug 2021)
    % Example: VO.install.get_data_dir
   
    arguments
        Args.InstellationLocation = '~/matlab/data';
        Args.Dir                  = {'https://euler1.weizmann.ac.il/archive/AstroPack/data/spec/GAIA_SpecTemplate/index.html'};
        Args.SearchFile           = {'.*\.mat'}
        Args.SizeMB               [1805];
        Args.Delete(1,1) logical  = true;
        Args.Npwget               = 10;
        Args.wgetPars             = '-q -o /dev/null -U Mozilla --no-check-certificate';
    end
    
    if ~isunix && ~ismac
        % assume windows - replace / with \
        Args.InstellationLocation = strrep(Args.InstellationLocation,'/',filesep);
    else
        Args.InstellationLocation = strrep(Args.InstellationLocation,'\',filesep);
    end
    
    PWD = pwd;
    cd('~/');
    mkdir(Args.InstellationLocation);
    
    Ndir = numel(Args.Dir);
    for Idir=1:1:Ndir    
        % create dir for instellation
        cd('~/');
        cd(Args.InstellationLocation);
    
        Tmp = regexp(Args.Dir{Idir},'/','split');
        FindData = strcmp(Tmp,'data');
        if sum(FindData)>1
            error('More than one data string in path');
        elseif sum(FindData)==0
            error('No data string in path');
        else
            % 1 match
            Parts  = Tmp(find(FindData)+1:end-1);
            Nparts = numel(Parts);
            SubDir = '';
            for Iparts=1:1:Nparts
                SubDir = sprintf('%s%s%s',SubDir,filesep,Parts{Iparts});
                mkdir(Parts{Iparts});
                cd(Parts{Iparts});
            end
        end
        
        
        [List,IsDir,FileName] = www.find_urls(Args.Dir{Idir},'match',Args.SearchFile{Idir});
        List     = List(~IsDir);
        FileName = FileName(~IsDir);
        
        if numel(List)>0
            % delete content before reload
            if Args.Delete
                delete('*');
            end
            www.pwget(List, Args.wgetPars, Args.Npwget);
        end
    end
    cd(PWD);
end