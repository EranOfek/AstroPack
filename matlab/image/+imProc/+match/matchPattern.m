function Result = matchPattern(Obj1, Obj2, Args)
    % Match two catalogs using stars pattern and return approximate transformation
    % Input  : -
    % Output : -
    % Author : Eran Ofek (May 2021)
    % Example:
    
    arguments
        Obj1
        Obj2
        Args.ColNamesX               = {'X','x','XWIN_IMAGE','X_IMAGE'};
        Args.ColNamesY               = {'Y','y','YWIN_IMAGE','Y_IMAGE'};
        
        Args.Scale                   = 1.0; % scale or [min max] range that require to ?
        Args.HistDistEdgesRotScale   = [10 600 300];
        Args.HistDistEdgesRot        = (12:3:300).';
        Args.HistRotEdges            = (-90:0.2:90);  % rotation or [min max] rotation that require to ?
        Args.RangeX                  = [-1000 1000]; 
        Args.RangeY                  = [-1000 1000]; 
        Args.StepX                   = 4;
        Args.StepY                   = 4;
        Args.Flip                    = [1 1; 1 -1;-1 1;-1 -1];
        Args.SearchRadius            = 4;

        % maxima finding
        Args.MaxMethod               = 'thresh_fracmax';
        Args.Threshold               = 5;
        Args.FracOfMax               = 0.8;
        Args.Conn                    = 8;
        
        % background pars for find_shift_pairs
        Args.BackFun                 = @nanmedian; % @median;
        Args.BackFunPar cell         = {'all'};      % {[1 2],'omitnan'});
        Args.VarFun                  = @imUtil.background.rvar;    % if empty, then will try to read var from second output of BackFun...
        Args.VarFunPar cell          = {}; % {[1 2]});
        Args.SubSizeXY               = [128 128];  % or 'full'
        Args.Overlap                 = [16]; 
        Args.MinVariance             = 1;
    end
    
    % Obj1
    if isa(Obj1,'AstroImage')
        % transform AstroImage to AstroCatalog
        Obj1 = astroImage2AstroCatalog(Obj1, 'CreateNewObj',false);
        
    elseif isa(Obj1,'AstroCatalog')
        % do nothing
    elseif isnumeric(Obj1)
        % assume Obj1 is a matrix in which the X/Y are in columns 1 and 2
        Obj1 = AstroCatalog({Obj1},'ColNames',{'X','Y'});
        Obj1.ColX = 1;
        Obj1.ColY = 2;
        Obj1.CooType = 'pix';
    else
        error('Unsupported Obj1 type - must be AstroImage, AstroCatalog or numeric');
    end
    
    % Obj2
    if isa(Obj2,'AstroImage')
        % transform AstroImage to AstroCatalog
        Obj2 = astroImage2AstroCatalog(Obj2, 'CreateNewObj',false);
        
    elseif isa(Obj2,'AstroCatalog')
        % do nothing
    elseif isnumeric(Obj2)
        % assume Obj1 is a matrix in which the X/Y are in columns 1 and 2
        Obj2 = AstroCatalog({Obj2},'ColNames',{'X','Y'});
        Obj2.ColX = 1;
        Obj2.ColY = 2;
        Obj2.CooType = 'pix';
    else
        error('Unsupported Obj1 type - must be AstroImage, AstroCatalog or numeric');
    end   
    
    % 
    Nobj1 = numel(Obj1);
    Nobj2 = numel(Obj2);
    Nmax  = max(Nobj1, Nobj2);
    for Imax=1:1:Nmax
        Iobj1 = min(Imax, Nobj1);
        Iobj2 = min(Imax, Nobj2);
        % get X/Y coordinates
        CatColX = colnameDict2ind(Obj1(Iobj1), Args.ColNamesX);
        CatColY = colnameDict2ind(Obj1(Iobj1), Args.ColNamesY);
        RefColX = colnameDict2ind(Obj2(Iobj2), Args.ColNamesX);
        RefColY = colnameDict2ind(Obj2(Iobj2), Args.ColNamesY);

        Cat = getCol(Obj1(Iobj1), [CatColX, CatColY]);
        Ref = getCol(Obj2(Iobj2), [RefColX, RefColY]);
        
        % match catalogs
        [Sol, PrevStep, Matched] = imUtil.patternMatch.match_scale_rot_shift(Cat, Ref, 'CatColX',CatColX, 'CatColY',CatColY,...
                                                                                       'RefColX',RefColX, 'RefColY',RefColY,...
                                                                                       'Scale',Args.Scale,...
                                                                                       'HistDistEdgesRotScale',Args.HistDistEdgesRotScale,...
                                                                                       'HistDistEdgesRot',Args.HistDistEdgesRot,...
                                                                                       'HistRotEdges',Args.HistRotEdges,...
                                                                                       'RangeX',Args.RangeX,...
                                                                                       'RangeY',Args.RangeY,...
                                                                                       'StepX',Args.StepX,...
                                                                                       'StepY',Args.StepY,...
                                                                                       'Flip',Args.Flip,...
                                                                                       'SearchRadius',Args.SearchRadius,...
                                                                                       'MaxMethod',Args.MaxMethod,...
                                                                                       'Threshold',Args.Threshold,...
                                                                                       'FracOfMax',Args.FracOfMax,...
                                                                                       'Conn',Args.Conn,...
                                                                                       'BackFun',Args.BackFun,...
                                                                                       'BackFunPar',Args.BackFunPar,...
                                                                                       'VarFun',Args.VarFun,...
                                                                                       'VarFunPar',Args.VarFunPar,...
                                                                                       'SubSizeXY',Args.SubSizeXY,...
                                                                                       'Overlap',Args.Overlap,...
                                                                                       'MinVariance',Args.MinVariance);
                                                                                       
                                                                                       
                                                                                       
        % populate the Result
        Result(Imax).Sol     = Sol;
        Result(Imax).Matched = Matched;
        
        
         
    end
    
end