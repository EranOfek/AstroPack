function startup(AddLast, AddAstroPath)
    % startup for LAST directories - This is a template - the function name is startup

    arguments
        AddLast logical       = true;
        AddAstroPath logical  = true;
    end
    
    PWDbase = pwd;
    
    Home     = char(java.lang.System.getProperty('user.home'));
    Base     = sprintf('%s%s%s%s%s',Home, filesep,'matlab',filesep, 'LAST');
    Computer = char(java.net.InetAddress.getLocalHost.getHostName);
    
    if AddLast
        AllFiles = dir(Base);
        IsDir = [AllFiles.isdir];
        List = {AllFiles(IsDir).name};

        Nlist = numel(List);
        for Ilist=1:1:Nlist
            if ~(strcmp(List(1),'+') || strcmp(List(1),'@'))
                %sprintf('%s%s%s',Base, filesep, List{Ilist})
                addpath(sprintf('%s%s%s',Base, filesep, List{Ilist}));
            end
        end
    end
    
    if AddAstroPath
        PWD = pwd;
        AstroPathStartup = [Home, filesep, 'matlab', filesep, 'AstroPack', filesep, 'matlab', filesep, 'startup'];
        cd(AstroPathStartup);
        AstroPack_CatsHTMPath = [filesep, Computer, filesep, 'data', filesep, 'catsHTM'];
        startup('AstroPack_CatsHTMPath',AstroPack_CatsHTMPath);
    end

    % WebApi stuff
    addpath(sprintf('%s%s%s',Base, filesep, 'LAST_WebApi', filesep, 'Simple3rdParty'));
    addpath(sprintf('%s%s%s',Base, filesep, 'LAST_WebApi', filesep, 'snisWebSite'));
    
    cd(PWDbase);

end
