function Result = insertAzAlt(Obj, Args)
    % Insert Az, Alt, AirMass, ParAng columns to AstroCatalog object
    % 
    
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
    end
    RAD = 180./pi;
    
    Nobj = numel(Obj);
    
    if isempty(Args.JD)
        if isa(Obj, 'AstroImage')
            Args.JD = julday(Obj, Args.juldayArgs{:});
        else
            error('if input object is not AstroImage, JD must be provided');
        end
    end
    
    if isemprt(Args.ObsCoo)
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
        LST = celestial.time.lst(Args.JD, Args.ObsCoo(Iobj,1)./RAD, Args.TypeLST).*2pi;  % [rad]
        
        [RA, Dec] = getLonLat(Obj(Iobj), 'rad', Args.getLonLatArgs{:});
        
        HA  = LST - RA;  % [rad]
        [Az, Alt] = celestial.coo.hadec2azalt(HA, Dec, Args.ObsCoo(Iobj,2));   % [rad]
        
        
        Nrows    = numel(RA);
        Data     = zeros(Nrows,0);
        ColName  = cell(0,1);
        ColUnits = cell(0,1);
        if Args.insertAzAlt
            ConvFactor = convert.angular('rad', Args.ColUnitsAzAlt);
            Az         = Az.*ConvFactor;
            Alt        = Alt.*ConvFactor;
            
            Data       = [Az, Alt];
            ColName    = Args.ColNameAzAlt;
            ColUnits   = {Args.ColUnitsAzAlt, Args.ColUnitsAzAlt};
        end
        if Args.insertAM
            AM = celestial.coo.hardie(pi./2 - Alt, Args.AirMassAlgo);
            
            Data       = [Data, AM];
            ColName    = [ColName, {Args.ColNameAM}];
            ColUnits   = [ColUnits, ''];
        end
        if Args.insertPA
            ParAng     = celestial.coo.parallactic_angle(RA, Dec, LST, Args.ObsCoo(Iobj,2));  % [rad]
            ParAng     = convert.angular('rad', Args.ColUnitsPA, ParAng);
            
            Data       = [Data, ParAng];
            ColName    = [ColName, {Args.ColNamePA}];
            ColUnits   = [ColUnits, {Args.ColUnitsPA}];
        end
        
        if ~isempty(Data)
            Obj(Iobj)  = insertCol(Obj(Iobj), Data, Args.ColPos, ColName, ColUnits);
        end
        
    end
            
end