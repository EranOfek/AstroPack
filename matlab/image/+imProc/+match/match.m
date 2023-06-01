function [Result1, ResInd, UnMatched1, UnMatched2]=match(Obj1, Obj2, Args)
    % Match the content of two AstroCatalog objects.
    %   
    % Output : - (Obj1) The first input object. An AstroCatalog or an AstroImage 
    %            containing AstroCatalog in the CatData property.
    %          - (Obj2) The second input object. An AstroCatalog or an AstroImage 
    %            containing AstroCatalog in the CatData property.
    %            The number of elemts in Obj2 is or equal to the number of
    %            elemsnts in Obj1. Each elemnt in Obj1 will be matched
    %            against the corresponding element in Obj2.
    %          * ...,key,val,...
    %            'Radius' - Search radius for matching. Default is 5.
    %            'RadiusUnits' - Default is 'arcsec'.
    %                   If 'CooType'=pix then the search units are pixels.
    %            'CooType' - 'sphere'|'pix'. Default is 'sphere'.
    %            'CreateNewObj' - Create a new output object. If false,
    %                   then will update Obj1.
    %                   Default is true.
    %
    %            'ColCatX' - If CooType is not empty, this is the column
    %                   names/index from which to select the catalog X
    %                   coordinate. Default is [].
    %            'ColCatY' - Like 'ColCatX', but for the Y coordinate.
    %            'ColRefX' - Like 'ColCatX', but for te ref catalog.
    %            'ColRefY' - Like 'ColRefX', but for the Y coordinate.
    % Output : - An AstroImage or AstroCatalog object with the sources
    %            in Obj1 matched to Obj2
    %            In each catalog the number of lines is equal to the number
    %            of lines in the Obj2 catalog.
    %          - A structure array containing the main output of the
    %            imProc.match.matchReturnIndices function.
    %          - An AstroCatalog object of sources in Obj1 that are not in
    %            Obj2.
    %          - An AstroCatalog object of sources in Obj2 that are not in
    %            Obj1.
    %
    % Author : Eran Ofek (May 2023)
    % Example: R=imProc.match.match(AI1, AI2)

    arguments
        Obj1
        Obj2

        Args.Radius                      = 5;
        Args.RadiusUnits                 = 'arcsec';
        % if given will override ColX/ColY
        Args.CooType                     = 'sphere';   % MUST BE SPECIFIED: 'pix' | 'sphere'
        Args.CreateNewObj(1,1) logical   = true; % for the sorted version of Obj1

        Args.ColCatX                     = [];
        Args.ColCatY                     = [];
        Args.ColRefX                     = [];
        Args.ColRefY                     = [];
        
    end

    if Args.CreateNewObj
        Result1 = Obj1.copy;
    else
        Result1 = Obj1;
    end


    Nobj1 = numel(Obj1);
    Nobj2 = numel(Obj2);
    Nmax  = max(Nobj1, Nobj2);

    if ~(Nobj1==1 || Nobj1==Nmax)
        error('Number of elements in Obj1 must be 1 or equal to the number of elements in Obj2');
    end
    if ~(Nobj2==1 || Nobj2==Nmax)
        error('Number of elements in Obj2 must be 1 or equal to the number of elements in Obj1');
    end

    for Imax=1:1:Nmax
        Iobj1 = min(Imax, Nobj1);
        Iobj2 = min(Imax, Nobj2);

        if isa(Obj1, 'AstroImage')
            Cat1 = Result1(Iobj1).CatData;
        elseif isa(Obj1, 'AstroCatalog')
            Cat1 = Result1(Iobj1);
        else
            error('Obj1 must be of AstroImage or AstroCatalog class');
        end
        if isa(Obj2, 'AstroImage')
            Cat2 = Obj2(Iobj2).CatData;
        elseif isa(Obj1, 'AstroCatalog')
            Cat2 = Obj2(Iobj2);
        else
            error('Obj2 must be of AstroImage or AstroCatalog class');
        end

        %check that Cat1 is sorted
        switch Args.CooType
            case 'pix'
                [ColInd, ~, ~] = Cat1.colnameDict2ind(AstroCatalog.DefNamesY);
                Cat1.sortrows(ColInd);
            case 'sphere'
                [ColInd, ~, ~] = Cat1.colnameDict2ind(AstroCatalog.DefNamesDec);
                Cat1.sortrows(ColInd);
            otherwise
                error('Unknown CooType option');
        end

        % Match Cat1 and Cat2
        ResInd(Imax) = imProc.match.matchReturnIndices(Cat1,Cat2, 'Radius',Args.Radius,...
                                                            'RadiusUnits',Args.RadiusUnits,...
                                                            'CooType',Args.CooType,...
                                                            'ColCatX',Args.ColCatX,...
                                                            'ColCatY',Args.ColCatY,...
                                                            'ColRefX',Args.ColRefX,...
                                                            'ColRefY',Args.ColRefY,...
                                                            'CreateNewObj',Args.CreateNewObj);
        %
        % Unmatched catalogs
        % need to be done before the matching, otherwise Result1 may
        % changed
        if nargout>2
            UnMatched1(Imax) = AstroCatalog;
            FlagNaN = isnan(ResInd(Imax).Obj1_IndInObj2);
            if isa(Obj1, 'AstroImage')
                UnMatched1(Imax) = Obj1.CatData.selectRows(FlagNaN, 'CreateNewObj',true);
            else
                UnMatched1(Imax) = Obj1.selectRows(FlagNaN, 'CreateNewObj',true);
            end

            if nargout>3
                
                UnMatched2(Imax) = AstroCatalog;
                FlagNaN = isnan(ResInd(Imax).Obj2_IndInObj1);
                if isa(Obj2, 'AstroImage')
                    UnMatched2(Imax) = Obj2.CatData.selectRows(FlagNaN, 'CreateNewObj',true);
                else
                    UnMatched2(Imax) = Obj2.selectRows(FlagNaN, 'CreateNewObj',true);
                end
            end
        end


        if isa(Result1, 'AstroImage')
            Result1(Imax).CatData= Result1(Imax).CatData.selectRows(ResInd(Imax).Obj2_IndInObj1, 'IgnoreNaN',false, 'CreateNewObj',Args.CreateNewObj);
        else
            % Result1 is an AstroCatalog object
            Result1(Imax) = Result1(Imax).selectRows(ResInd(Imax).Obj2_IndInObj1, 'IgnoreNaN',false, 'CreateNewObj',Args.CreateNewObj);
        end

        
    end

end