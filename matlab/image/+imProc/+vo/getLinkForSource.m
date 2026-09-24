function [Result] = getLinkForSource(Obj, Ind, LinkFunction, Args)
    % Get URL link for source in an AstroCatalog/AstroImage/AstroDiff/...
    %     The function search for a source in a catalog by its index,
    %     return its coordinates, and (deg), and also execute a user function
    %     that may return a URL link. Optionally, open the link in a web
    %     browser.
    % Input  : - An AstroCatalog/AstroImage/AstroDiff/AstroZOGY object.
    %          - Index of source in the catalog.
    %            If empty, then will return all sources.
    %            Default is [].
    %          - Function handle of the form LinkCell=F(RA_rad, Dec_rad)
    %          * ...,key,val,... 
    %            'OpenBrowser' - A logical indicating if to open a web
    %                   browser. Default is true.
    % Output : - A structure array with element per element in the input
    %            object.
    %            The following fields are available:
    %            .Link - A cell array of URL links.
    %            .RA [deg]
    %            .Dec [deg].
    % Author : Eran Ofek (2024 Jul) 
    % Example: R=imProc.vo.getLinkForSource(ADc,[], @VO.SDSS.navigator_link)
    %          R=imProc.vo.getLinkForSource(ADc,1, @VO.PS1.navigator_link)
    %          R=imProc.vo.getLinkForSource(ADc,1, @VO.DECaLS.decals_viewer_link)

    arguments
        Obj
        Ind                        = [];
        LinkFunction               = @VO.SDSS.navigator_link;
        Args.OpenBrowser logical   = false;
    end
    Units = 'deg';
    
    RAD = 180./pi;
    
    if nargout==0
        % dorce display
        Args.OpenBrowser = true;
    end
    
    Nobj = numel(Obj);
    for Iobj=Nobj:-1:1
        if isa(Obj, 'AstroCatalog')
            [RA, Dec] = Obj(Iobj).getLonLat(Units);
        elseif isa(Obj, 'AstroImage') || isa(Obj, 'AstroDiff') || isa(Obj, 'AstroZOGY')
            [RA, Dec] = Obj(Iobj).CatData.getLonLat(Units);
        end
            
        Nsrc = numel(RA);
        if isempty(Ind)
            Ind = (1:Nsrc).';
        end
        
        if numel(RA)==0
            Result(Iobj).Link = {};
            Result(Iobj).RA    = NaN;
            Result(Iobj).Dec   = NaN;
        else
            Result(Iobj).Link = LinkFunction(RA./RAD, Dec./RAD);
            Result(Iobj).RA    = RA;
            Result(Iobj).Dec   = Dec;            
        end

        if Args.OpenBrowser && numel(Result(Iobj).Link)==1
            web(Result(Iobj).Link{1});
        end
    end
    
end
