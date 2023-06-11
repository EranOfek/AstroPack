function [Result, SelObj, ResInd, CatH] = match_catsHTM_multiInsertFlag(Obj, Args)
    %
    
    
    arguments
        Obj
        Args.CatNames cell       = {'GAIAEDR3','PS1','SDSSDR10','DECaLS','ztfSrcLCDR1','HST','unWISE','GALEX','IPHAS','TMASS','TMASSxsc', 'PGC','GLADE', 'ztfDR1var','SpecSDSS','NED','FIRST','NVSS','ROSAT','XMM'};
        Args.CatBitsInd          = [         1,    2,         3,       4,            5,    6,       7,      8,      9,     10,        11,    12,     13,          14,        15,   16,     17,    18,     19,   20];
        Args.SearchRadii         = [         2,    2,         2,       2,            2,    2,       3,      3,      2,      2,         5,   NaN,    NaN,           1,         5,    5,      5,    15,     20,   10];
        Args.PhysicalSearchRadKpc= [       
        Args.Coo                 = [];
        Args.CooUnits            = 'deg';
        Args.Radius              = 3;
        Args.RadiusUnits         = 'arcsec';
        Args.CatRadius           = [];
        Args.CatRadiusUnits      = 'arcsec';
        Args.Con                 = {};
        Args.catsHTMisRef        = false;
        
        Args.AddColDist logical   = true;
        Args.ColDistPos           = Inf;
        Args.ColDistName          = 'Dist';
        Args.ColDistUnits         = 'arcsec';
        Args.AddColNmatch logical = true;
        Args.ColNmatchPos         = Inf;
        Args.ColNmatchName        = 'Nmatch';
    end

    
    Icat = 0;
    Icat = Icat + 1;
    CatInfo(Icat).Name   = 'GAIAEDR3';
    CatInfo(Icat).BitInd = Icat;
    CatInfo(Icat).RadAS  = 2;
    CatInfo(Icat).RadKPC = NaN;
    Icat = Icat + 1;
    CatInfo(Icat).Name   = 'PS1';
    CatInfo(Icat).BitInd = Icat;
    CatInfo(Icat).RadAS  = 2;
    CatInfo(Icat).RadKPC = NaN;
    Icat = Icat + 1;
    CatInfo(Icat).Name   = 'DECaLS';
    CatInfo(Icat).BitInd = Icat;
    CatInfo(Icat).RadAS  = 2;
    CatInfo(Icat).RadKPC = NaN;
    Icat = Icat + 1;
    CatInfo(Icat).Name   = 'ztfSrcLCDR1';
    CatInfo(Icat).BitInd = Icat;
    CatInfo(Icat).RadAS  = 2;
    CatInfo(Icat).RadKPC = NaN;
    Icat = Icat + 1;
    CatInfo(Icat).Name   = 'HST';
    CatInfo(Icat).BitInd = Icat;
    CatInfo(Icat).RadAS  = 2;
    CatInfo(Icat).RadKPC = NaN;
    Icat = Icat + 1;
    CatInfo(Icat).Name   = 'unWISE';
    CatInfo(Icat).BitInd = Icat;
    CatInfo(Icat).RadAS  = 3;
    CatInfo(Icat).RadKPC = NaN;
    Icat = Icat + 1;
    CatInfo(Icat).Name   = 'GALEX';
    CatInfo(Icat).BitInd = Icat;
    CatInfo(Icat).RadAS  = 3;
    CatInfo(Icat).RadKPC = NaN;
    Icat = Icat + 1;
    CatInfo(Icat).Name   = 'GALEX';
    CatInfo(Icat).BitInd = Icat;
    CatInfo(Icat).RadAS  = 4;
    CatInfo(Icat).RadKPC = NaN;
    Icat = Icat + 1;
    CatInfo(Icat).Name   = 'IPHAS';
    CatInfo(Icat).BitInd = Icat;
    CatInfo(Icat).RadAS  = 2;
    CatInfo(Icat).RadKPC = NaN;
    Icat = Icat + 1;
    CatInfo(Icat).Name   = 'TMASS';
    CatInfo(Icat).BitInd = Icat;
    CatInfo(Icat).RadAS  = 2;
    CatInfo(Icat).RadKPC = NaN;
    Icat = Icat + 1;
    CatInfo(Icat).Name   = 'TMASSxsc';
    CatInfo(Icat).BitInd = Icat;
    CatInfo(Icat).RadAS  = 2;
    CatInfo(Icat).RadKPC = NaN;
    CatInfo(Icat).GalRadColName = '';
    CatInfo(Icat).GalRadFun2AS  = @(x) x;
    Icat = Icat + 1;
    CatInfo(Icat).Name   = 'PGC';
    CatInfo(Icat).BitInd = Icat;
    CatInfo(Icat).RadAS  = 2;
    CatInfo(Icat).RadKPC = NaN;
    CatInfo(Icat).GalRadColName = 'LogD25';
    CatInfo(Icat).GalRadFun2AS  = @(x) 6.*(10.^x);  % [arcsec]
    
    
    
    'GALEX','IPHAS','TMASS','TMASSxsc', 'PGC','GLADE', 'ztfDR1var','SpecSDSS','NED','FIRST','NVSS','ROSAT','XMM'};
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    % convert AstroImage to AstroCatalog
    if isa(Obj,'AstroImage')
        Obj = astroImage2AstroCatalog(Obj,'CreateNewObj',Args.CreateNewObj);
    elseif isa(Obj,'AstroCatalog')
        % do nothing
    elseif isnumeric(Obj)
        error('Input Obj is of unsupported class');
    else
        error('Input Obj is of unsupported class');
    end


    if isempty(Args.Coo) || isempty(Args.Radius)
        UseUserCoo = true;
    else
        UseUserCoo = false;
    end


    Nobj = numel(Obj);
    MatchedObj = AstroCatalog(size(Obj));
    
    Result = Obj.copy();
    if nargout>1
        SelObj = AstroCatalog(size(Obj));
    end
    
    CatH = AstroCatalog(size(Obj));  % output of catsHTM
    for Iobj=1:1:Nobj
        if isempty(Args.Coo) || isempty(Args.CatRadius)
            % get coordinates using boundingCircle
            [CircX, CircY, CircR] = Obj(Iobj).boundingCircle('OutUnits','rad');
            Args.Coo                 = [CircX, CircY];
            Args.CatRadius      = CircR;
            Args.CooUnits       = 'rad';
            Args.CatRadiusUnits = 'rad';
        else
            Args.Coo = convert.angular(Args.CooUnits,'rad',Args.Coo);
        end
        Icoo = 1;
        CatH(Iobj)  = catsHTM.cone_search(CatName, Args.Coo(Icoo,1), Args.Coo(Icoo,2), Args.CatRadius, 'RadiusUnits',Args.CatRadiusUnits, 'Con',Args.Con, 'OutType','astrocatalog');

        if catsHTMisRef
            ResInd = imProc.match.matchReturnIndices(Obj, CatH, 'CooType','sphere',...
                                                            'Radius',Args.Radius,...
                                                            'RadiusUnits',Args.radiusUnits);
        else                                          
            % default!
            ResInd = imProc.match.matchReturnIndices(CatH, Obj, 'CooType','sphere',...
                                                            'Radius',Args.Radius,...
                                                            'RadiusUnits',Args.radiusUnits);
        end
        
        [Result(Iobj), SelObj] = insertCol_matchIndices(Result(Iobj), ResInd, 'AddColDist',Args.AddColDist,...
                                                                              'ColDistPos',Args.ColDistPos,...
                                                                              'ColDistName',Args.ColDistName,...
                                                                              'ColDistUnits',Args.ColDistUnits,...
                                                                              'AddColNmatch',Args.AddColNmatch,...
                                                                              'ColNmatchPos',Args.ColNmatchPos,...
                                                                              'ColNmatchName',Args.ColNmatchName);
        
        

    end
end
