function [Result, Matched] = fitPattern(Obj1, Obj2, Args)
    % Match two catalogs using stars pattern and return approximate transformation
    % Description: Given two catalogs that coordinadte systems are related
    %              by flip, scale, rotation and shift, search the the approximate
    %              affine transformation
    %              that is required in order to align the coordinate systems of
    %              the two catalogs. The search is done by matching patterns in
    %              one catalog to the other.
    % Input  : - An AstroImage containing AstroCatalog or an AstroCatalog,
    %            object, or a two column matrix with [X,Y] positions.
    %          - A reference catalog (like the first input argument).
    %            Both first and second arguments may be a single or
    %            multiple element objects.
    %          * Pairs of ...,key,val,... Possible keywords include:
    %            'ColNamesX' - Cell array of dictioray for X column names
    %                   for both catalogs.
    %                   Default is AstroCatalog.DefNamesX
    %            'ColNamesY' - Cell array of dictioray for Y column names
    %                   for both catalogs.
    %                   Default is AstroCatalog.DefNamesY
    %            'Scale' - The scale to apply to the referepnce catalog in order
    %                   to convert it to the input catalog. If this is a
    %                   scalar, then will not attempt to search for the best
    %                   scaleing. If a two element vector, then these are the
    %                   [min, max] scale range to search.
    %            'HistDistEdgesRotScale' - [MinDist, MaxDist, NumberOf points] for
    %                     distance histogram.
    %                     Default is [10 600 300].
    %            'HistRotEdges' - Edges for angle axis of the histogram.
    %                     If a scalar is provided then this rotation will be
    %                     assumed.
    %                     Default is (-90:0.2:90).'.
    %            'RangeX' - [Min Max] range of X shifts to test.
    %                     If empty, then will select automaticall the maximal
    %                     possible range.
    %                     Alternatively, this can be a vector of X-axis
    %                     histogram edges.
    %                     Default is [-1000 1000].
    %            'RangeY' - [Min Max] range of Y shifts to test.
    %                     If empty, then will select automaticall the maximal
    %                     possible range.
    %                     Alternatively, this can be a vector of Y-axis
    %                     histogram edges.
    %                     Default is [-1000 1000].
    %            'StepX' - X-axis step size for the histogram calculation.
    %                     If empty, then will use RangeX as a vector of edges.
    %                     Default is 3.
    %            'StepY' - Y-axis step size for the histogram calculation.
    %                     If empty, then will use RangeY as a vector of edges.
    %                     Default is 3.
    %            'Flip' - A two column matrix of all possible flips to test.
    %                     Use 'all' to check all flips - i.e., [1 1; 1 -1;-1 -1; -1 1].
    %                     Default is [1 1].
    %            'SearchRadius' - Searchj radius for final source matching
    %                     [pix]. Default is 4.
    %            'MaxMethod' - he method by which the 2D histogram peaks will
    %                     be selected. The following options are available:
    %                     'thresh' - Select maxima larger than some threshold.
    %                     'max1' - Select the highest maxima.
    %                     'maxall' - Select all the maxima.
    %                     'max_fracmax' - Select all the maxima above a
    %                               fraction (given by FracOfMax) of the
    %                               highest maximum.
    %                     'thresh_fracmax' - Select all the maxima above the
    %                               threshold and above a
    %                               fraction (given by FracOfMax) of the
    %                               highest maximum.
    %                     Alternatively this can be a positive integer (N).
    %                     In this case will return the N highest maxima.
    %                     Default is 'thresh_fracmax'
    %            'Threshold' - Detection threshold.
    %                     If PeakMethod is 'sn' then this has units of S/N.
    %                     Otherwise, this is the number of matches.
    %                     Default is 8.
    %            'Conn' - local maxima finding connectivity parameter.
    %                     For details see imUtil.image.local_maxima.
    %                     Default is 8.
    %            'FracOfMax' - The parameter that that used in 'max1frac' and
    %                     'sn' PeakMethod, for selecting peaks.
    %                     Only peaks that above the maximal peak multiplied by
    %                     this parameter will be returned.
    %            'BackFun' - Used for 'sn' PeakMethod.
    %                     For details see imUtil.background.background.
    %                     Default is @median.
    %            'BackFunPar' - Used for 'sn' PeakMethod.
    %                     For details see imUtil.background.background.
    %                     Default is {'all','omitnan'}.
    %            'VarFun' - Used for 'sn' PeakMethod.
    %                     For details see imUtil.background.background.
    %                     Default is @imUtil.background.rvar.
    %            'VarFunPar' - Used for 'sn' PeakMethod.
    %                     For details see imUtil.background.background.
    %                     Default is {}.
    %            'SubSizeXY' - Used for 'sn' PeakMethod.
    %                     For details see imUtil.background.background.
    %                     Default is [128 128].
    %            'OverlapXY' - Used for 'sn' PeakMethod.
    %                     For details see imUtil.background.background.
    %                     Default is [16 16].
    %            'MinVariance' - The minimum variance in in the 2D histogram,
    %                     That is used to calculate the S/N.
    %                     Default is 1.
    %            'FilterSigma' - Width [sigma units] of Gaussian filter with
    %                   which to cross-correlate the H2 (hits for shifts) matrix.
    %                   If empty, no filtering is applied. Default is 3.
    % Output : - A structure of possible solutions for matching between the two
    %            catalogs. Follwoing fields are available:
    %            .SN
    %            .MaxVal
    %            .Flip
    %            .Rot
    %            .Scale
    %            .ShiftX - The shift in X one need to add to Ref in order to
    %                   get Cat.
    %            .ShiftY - The shift in Y one need to add to Ref in order to
    %                   get Cat.
    %            .AffineTran - A cell array of affine matrix transformations
    %               Apply this transformation to Ref using imUtil.cat.affine2d_transformation
    %               In order to get Cat.
    %               This is the rotation transformation for the reference frame
    %               and not the reference coordinates.
    %          - A structure of matching steps, including ResShift and ResRot.
    %            ResShift is documented in
    %            imUtil.patternMatch.find_shift_pairs, while ResRot in imUtil.patternMatch.find_rot_pairs
    %            or imUtil.patternMatch.find_scalerot_pairs.
    %          - A structure containing the matched sources for each solution.
    %            The following fields are available:
    %            'MatchedCat - [X,Y] of the sources in Cat matched to 
    % Author : Eran Ofek (May 2021)
    % Example: Result = imProc.trans.fitPattern(Obj1, Obj2, Args)
    
    arguments
        Obj1
        Obj2
        Args.ColNamesX               = AstroCatalog.DefNamesX;
        Args.ColNamesY               = AstroCatalog.DefNamesY;
        
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
        Args.BackFun                 = @median; % @median;
        Args.BackFunPar cell         = {'all','omitnan'};      % {[1 2],'omitnan'});
        Args.VarFun                  = @imUtil.background.rvar;    % if empty, then will try to read var from second output of BackFun...
        Args.VarFunPar cell          = {}; % {[1 2]});
        Args.SubSizeXY               = [128 128];  % or 'full'
        Args.Overlap                 = [16]; 
        Args.MinVariance             = 1;
        Args.FilterSigma             = 3;
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

        %Cat = getCol(Obj1(Iobj1)); %, [CatColX, CatColY]);  if using this modify CatCol to 1,2...
        %Ref = getCol(Obj2(Iobj2)); %, [RefColX, RefColY]);
        
        
        % match catalogs
        if nargout>1
            OutputArgs = cell(1,3);
            Nargs = 3;
        else
            OutputArgs = cell(1,1);
            Nargs = 1;
        end
        % Sol, PrevStep, Matched                                                                              
        [OutputArgs{1:Nargs}] = imUtil.patternMatch.match_scale_rot_shift(Obj1(Iobj1).Catalog, Obj2(Iobj2).Catalog,...
                                                                                       'CatColX',CatColX, 'CatColY',CatColY,...
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
                                                                                       'MinVariance',Args.MinVariance,...
                                                                                       'FilterSigma',Args.FilterSigma);
                                                                                       
        if nargout>1                                                                           
            Result(Imax).Sol  = OutputArgs{1};
            %Matched(Imax).Matched = OutputArgs{3};
            MatchedCat = AstroCatalog;
            MatchedCat.Catalog  = OutputArgs{3}.MatchedCat;
            MatchedCat.ColNames = Obj1(Iobj1).ColNames;
            MatchedCat.ColUnits = Obj1(Iobj1).ColUnits;
            MatchedRef          = Obj2(Iobj2);
            % update the coordinate in Ref (by the affine transformation)
            MatchedRef.replaceCol(OutputArgs{3}.Ref, [RefColX, RefColY]);
            Matched(Imax).MatchedCat = MatchedCat;
            Matched(Imax).MatchedRef = MatchedRef;
            
            Matched(Imax).ResM       = OutputArgs{3}.ResM;
        else
            Result(Imax).Sol  = OutputArgs{1};
        end
         
        % populate the Result
        %Result(Imax).Sol     = Sol;
        %Result(Imax).Matched = Matched;
        
        
         
    end
    
end