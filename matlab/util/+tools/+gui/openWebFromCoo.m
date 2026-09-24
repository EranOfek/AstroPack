function [URL] = openWebFromCoo(T, Type, Args)
    % Given a table line with RA/Dec, open SIMBAD URL in web.
    %   Aux function for editInspectTable
    % Input  : - A single line table with columns containing RA,Dec.
    %          - Web type option:
    %            'simbad'|'ned'|'sdss'|'decls'|'ps1'.
    %            Default is 'simbad'.
    %          * ...,key,val,... 
    %            'Units' - RA/Dec units. Default is 'deg'.
    %            'ColRA' - Possible RA column names.
    %                   Default is ["ra","RA"]
    %            'ColDec' - Possible Dec column names.
    %                   Default is ["dec","Dec","DEC"]
    %            'OpenWeb' - Open URL in web. Default is true.
    % Output : - URL + open URL in web.
    % Author : Eran Ofek (2025 May) 
    % Example: tools.gui.aux.openWebFromCoo(T(1,:))

    arguments
        T
        Type                       = 'simbad';
        Args.Units                 = 'deg';
        Args.ColRA                 = ["ra","RA"];
        Args.ColDec                = ["dec","Dec","DEC"];
        Args.OpenWeb               = true;
    end

    [~,ColRA]  = tools.table.isColumn(T, Args.ColRA);
    [~,ColDec] = tools.table.isColumn(T, Args.ColDec);

    Factor = convert.angular(Args.Units, 'rad');
    RA     = Factor.*T.(ColRA);
    Dec    = Factor.*T.(ColDec);

    switch lower(Type)
        case 'simbad'
            URL = VO.search.simbad_url(RA, Dec).URL;
        case 'ned'
            URL = VO.NED.ned_link(RA, Dec).URL;
        case 'sdss'
            URL = VO.SDSS.navigator_link(RA, Dec).URL;
        case 'decals'
            URL = VO.DECaLS.decals_viewer_link(RA, Dec).URL;
        case 'ps1'
            URL = VO.PS1.navigator_link(RA,Dec).URL;
        otherwise
            error('Unknown web type option');
    end

    if Args.OpenWeb
        web(URL);
    end

end
