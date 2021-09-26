function Result = insertAzAlt(Obj, Args)
    % Calculate and insert Az, Alt, AirMass, ParAng columns to AstroCatalog object
    % Input  : - An AstroCatalog or AstroImage object.
    %          * ...,key,val,...
    %            'JD' - A vector of JDs (per image/catalog). If empty, will
    %                   attempt to get the JD from the image header.
    %                   Default is [].
    %            'ObsCoo' - A two column matrix of obs. coordinates
    %                   [Lon Lat]. If empty, will attempt to get
    %                   coordinates from header.
    %            'ObsCooUnits' - Default is 'deg'.
    %            'InsertAzAlt' - Insert Az and Alt. Default is true.
    %            'InsertAM' - Insert AirMass. Default is true.
    %            'InsertPA' - Insert Paralactic angle. Default is true.
    %            'AirMassAlgo' - 'csc'|['hardie].
    %            'ColPos' - Column position at which to adde the new
    %                   columns. Default is Inf.
    %            'ColNameAzAlt' - Names of the new Az/Alt columns.
    %                   Default is {'Az','Alt'}.
    %            'ColUnitsAzAlt' - Units for Az/Alt colums.
    %                   Default is 'deg'.
    %            'ColNameAM' - Name of new AirMass column. Default is 'AM'.
    %            'ColNamePA' - Name of the new paralactic angle column.
    %                   Default is 'ParAng'.
    %            'ColUnitsPA' - Units of PA column. Default is 'deg'.
    %            'juldayArgs' - Cell array of additional arguments to pass
    %                   to the julday function.
    %            'getObsCooArgs' - Add. arguments to pass to getObsCoo.
    %            'getLonLatArgs' - Add. arguments o pass yo getLonLat.
    %            'TypeLST' - LST type. ['m'] - mean; 'a'-apparent.
    %            'CreateNewObj' - [], true, false.
    %                   If true, create new deep copy
    %                   If false, return pointer to object
    %                   If [] and Nargout==0 then do not create new copy.
    %                   Otherwise, create new copy.
    %                   Default is [].
    % Output : - An updated or new copy of the input catalog with the new
    %            columns.
    % Author : Eran Ofek (Sep 2021)
    % Example: AC = AstroCatalog({rand(100,2)},'ColNames',{'RA','Dec'},'ColUnits',{'rad','rad'});
    %          imProc.cat.insertAzAlt(AC, 'JD',2451545, 'ObsCoo',[35 32]);
    
    arguments
        Obj                               % AstroCatalog | AstroImage
        Args.JD                    = [];     % [] - get from Header
        Args.ObsCoo                = [];     % [] - get from header - [Lon, Lat]
        Args.ObsCooUnits           = 'deg';
        
        Args.InsertAzAlt logical   = true;
        Args.InsertAM logical      = true;
        Args.InsertPA logical      = true;
        Args.InsertUnits           = 'deg';
        
        Args.AirMassAlgo           = 'hardie';
        Args.ColPos                = Inf;
        Args.ColNameAzAlt          = {'Az','Alt'};
        Args.ColUnitsAzAlt         = 'deg';
        Args.ColNameAM             = 'AM';
        Args.ColNamePA             = 'ParAng';
        Args.ColUnitsPA            = 'deg';
        
        Args.juldayArgs cell       = {};
        Args.getObsCooArgs cell    = {};
        Args.getLonLatArgs cell    = {};
        Args.TypeLST               = 'm';
        
        Args.CreateNewObj          = [];
    end
    RAD = 180./pi;
    
    [Result] = createNewObj(Obj, Args.CreateNewObj, nargout);
    
    Nobj = numel(Obj);
    
    if isempty(Args.JD)
        if isa(Obj, 'AstroImage')
            Args.JD = julday(Obj, Args.juldayArgs{:});
        else
            error('if input object is not AstroImage, JD must be provided');
        end
    end
    
    if isempty(Args.ObsCoo)
        if isa(Obj, 'AstroImage')
            Args.ObsCoo = nan(Nobj,2);
            for Iobj=1:1:Nobj
                [Args.ObsCoo(Iobj,1), Args.ObsCoo(Iobj,2)] = getObsCoo(Obj(Iobj), Args,getObsCooArgs{:});  % [deg] / assumed
            end
        else
            error('if input object is not AstroImage, JD must be provided');
        end
    end
    
    Args.ObsCoo = convert.angular(Args.ObsCooUnits, 'rad', Args.ObsCoo);   % [rad]
    
    for Iobj=1:1:Nobj
        LST = celestial.time.lst(Args.JD, Args.ObsCoo(Iobj,1)./RAD, Args.TypeLST).*2.*pi;  % [rad]
        
        [RA, Dec] = getLonLat(Obj(Iobj), 'rad', Args.getLonLatArgs{:});
        
        HA  = LST - RA;  % [rad]
        [Az, Alt] = celestial.coo.hadec2azalt(HA, Dec, Args.ObsCoo(Iobj,2));   % [rad]
        
        
        Nrows    = numel(RA);
        Data     = zeros(Nrows,0);
        ColName  = cell(0,1);
        ColUnits = cell(0,1);
        if Args.InsertAzAlt
            ConvFactor = convert.angular('rad', Args.ColUnitsAzAlt);
            Az         = Az.*ConvFactor;
            Alt        = Alt.*ConvFactor;
            
            Data       = [Az, Alt];
            ColName    = Args.ColNameAzAlt;
            ColUnits   = {Args.ColUnitsAzAlt, Args.ColUnitsAzAlt};
        end
        if Args.InsertAM
            AM = celestial.coo.hardie(pi./2 - Alt, Args.AirMassAlgo);
            
            Data       = [Data, AM];
            ColName    = [ColName, {Args.ColNameAM}];
            ColUnits   = [ColUnits, ''];
        end
        if Args.InsertPA
            ParAng     = celestial.coo.parallactic_angle(RA, Dec, LST, Args.ObsCoo(Iobj,2));  % [rad]
            ParAng     = convert.angular('rad', Args.ColUnitsPA, ParAng);
            
            Data       = [Data, ParAng];
            ColName    = [ColName, {Args.ColNamePA}];
            ColUnits   = [ColUnits, {Args.ColUnitsPA}];
        end
        
        if ~isempty(Data)
            if isa(Obj, 'AstroCatalog')
                Result(Iobj)  = insertCol(Result(Iobj), Data, Args.ColPos, ColName, ColUnits);
            else
                % assume an AstroImage
                Result(Iobj).CatData  = insertCol(Result(Iobj).CatData, Data, Args.ColPos, ColName, ColUnits);
            end
        end
        
    end
            
end