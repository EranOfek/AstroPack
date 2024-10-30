function [Result] = insertCatalog(Obj, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    %            'GeoPos' - Geodetic position. If [], then assume geocentric position
    %                   and return zeros. Otherwise should be [Long, Lat, Height]
    %                   in [rad, rad, m]. Default is [].
    % Output : - 
    % Author : Eran Ofek (2024 Oct) 
    % Example: 

    arguments
        Obj
        Args.ColNameDic      
        Args.GeoPos       = [];
        Args.InTimeScale  = 'UTC';
        Args.ColJD        = 'JD';
        Args.ColRA        = 'RA';   % J2000
        Args.ColDec       = 'Dec';  % J2000
        Args.CooUnits     = 'deg';
        Args.VelOutUnits  = 'cm/s';
        Args.INPOP        = [];

        Args.ColBJD       = 'BJD'; % if [] - do not add
        Args.ColBaryVel   = 'BARYVEL'; % if [] - do not add
        
    end

    Nobj = numel(Obj);
    % read each catalog, selct columns, and convert their names
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            Tmp = Obj(Iobj).CatData.Table;
        else
            Tmp = Obj(Iobj).Table;
        end

        % select tables
        Tmp = Tmp.({Args.ColNameDic.ColName});
        % run functions
        %IndFun = find(~tools.cell.isempty_cell({Args.ColNameDic.ColFun}));
        %for If=1:1:numel(IndFun)

        % change column names
        Tmp.Properties.VariableNames = Args.ColNameDic.ColNameOut;

        % insert additional columns - cat by cat


        % concat all tables
        if Iobj==1
            T = Tmp;
        else
            T = [T;Tmp];
        end
    end

    % insert additional global columns

    % insert BJD
    if ~isempty(Args.ColBJD)
        if ~isempty(Args.ColBaryVel)
            [BJD, BVel] = celestial.time.barycentricJD(T.(Args.ColJD), T.(Args.ColRA), T.(Args.ColDec), 'INPOP',Args.INPOP,...
                                            'GeoPos',Args.GeoPos,...
                                            'InTimeScale',Args.InTimeScale,...
                                            'CooUnits',Args.CooUnits,...
                                            'VelOutUnits',Args.VelOutUnits);
            T.(Args.ColBaryVel) = BVel;
        else
            [BJD] = celestial.time.barycentricJD(T.(Args.ColJD), T.(Args.ColRA), T.(Args.ColDec), 'INPOP',Args.INPOP,...
                                            'GeoPos',Args.GeoPos,...
                                            'InTimeScale',Args.InTimeScale,...
                                            'CooUnits',Args.CooUnits,...
                                            'VelOutUnits',Args.VelOutUnits);
        end
        T.(Args.ColBJD) = BJD;
    end



end
