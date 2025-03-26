function [Result] = applyZP_AperCorr(Obj, Args)
    % Apply ZP and aperture corrections to magnitude columns.
    %   Given an AstroImage or AstroCatalog, search for all columns
    %   starting with 'MAG_'.
    %   1. Apply a ZP scalar or vector to ref mag (default is MAG_APER_3).
    %   2. For each mag column, calculate the aperture correction:
    %      median(Ref-Mag).
    %   3. Apply the aperture correction.
    %   4. Update the columns.
    %   5. Optionally add columns with the aperture corrections.
    % Input  : - An AstroImage or AstroCatalog object.
    %          * ...,key,val,... 
    %            'MagColName2update' - The prefix of the column names to
    %                   select as magnitude columns, or a cell
    %                   array/strings array of magnitude columns.
    %                   Default is 'MAG_'.
    %            'ColRefMag' - Column name to which to add the user
    %                   provided ZP. Default is 'MAG_APER_3'.
    %            'ZP' - A scalar or vector of ZP to add to the ColRefMag
    %                   column. Default is 0.
    %            'ApplyAperCorr' - A logical indicating if to apply the
    %                   aperture correction to all the other magnitude columns.
    %                   Default is true.
    %            'FunAperCorr' - Function handle to calculate the aperture
    %                   correction. Default is @median.
    %            'FunAperCorrArgs' - A cell array of additional arguments to
    %                   pass to the FunAperCorr function.
    %                   Default is {1, 'omitnan'}.
    %            'ColSN' - S/N column name. Default is 'SN'.
    %            'MinSN' - Min S/N of stars on which to calculate the
    %                   aperture correction. Default is 50.
    %            'MaxSN' - Max S/N of stars on which to calculate the
    %                   aperture correction. Default is 1000.
    %            'AddAperCorrCol' - A logical indicating if to add an
    %                   aperture correction column for each magnitude column.
    %            'AperCorrColPrefix' - The prefix to add to the magnitude
    %                   column name, that will contain the aperture correction.
    %                   Default is 'APC_'.
    %            'AperCorrColUnits' - Units of aperture correction.
    %                   Default is 'mag'.
    %            'CreateNewObj' - Create new copy of the object.
    %                   Defaults false.
    %
    % Output : - An update input object.
    % Author : Eran Ofek (2025 Mar) 
    % Example: AI=imProc.calib.applyZP_AperCorr(AI, 'ZP',ZP);

    arguments
        Obj
        Args.MagColName2update         = 'MAG_';  % or e.g., {'MAG_APER_1','MAG_APER_2'}
        Args.ColRefMag                 = 'MAG_APER_3';
        
        Args.ZP                        = 0;  % ZP to add to ColRefMag
        Args.ApplyAperCorr             = true;  % apply aperture corrections to all other mags.

        Args.FunAperCorr               = @median;
        Args.FunAperCorrArgs           = {1,'omitnan'};

        Args.ColSN                     = 'SN';
        Args.MinSN                     = 50;
        Args.MaxSN                     = 1000;
        Args.AddAperCorrCol            = true;
        Args.AperCorrColPrefix         = 'APC_';
        Args.AperCorrColUnits          = 'mag';

        Args.CreateNewObj              = false;
    end

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end


    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            Cat = Result(Iobj).CatData;
        else
            Cat = Result(Iobj);
        end
        
        if ischar(Args.MagColName2update) || (isstring(Args.MagColName2update) && numel(Args.MagColName2update)==1)
            MagColFlag = ~tools.cell.isempty_cell(regexp(Cat.ColNames, Args.MagColName2update, 'match'));
        else
            % Args.MagColName2update is a cell array or string array:
            MagColFlag = ismember(Cat.ColNames, Args.MagColName2update);
        end

        % SN is used for select good stars
        SN      = Cat.getCol(Args.ColSN);
        IgoodSN = find(SN>Args.MinSN & SN<Args.MaxSN);

        [RefMag,~,RefColInd] = Cat.getCol(Args.ColRefMag);

        % update ColMag in Catalog
        Cat.Catalog(:,RefColInd) = RefMag + Args.ZP;

        MagColInd = find(MagColFlag);
        Ncol      = numel(MagColInd);
        for Icol=1:1:Ncol
            if MagColInd(Icol)~=RefColInd
                ColMag = Cat.getCol(MagColInd(Icol));

                % calculate the aperture correction using the median of mag
                % diffs:
                AperCorr = Args.FunAperCorr(RefMag(IgoodSN) - ColMag(IgoodSN), Args.FunAperCorrArgs{:});

                % Apply aperture correction to ColMag:
                %ColMag = ColMag + AperCorr + Args.ZP;
            
                % update ColMag in Catalog
                Cat.Catalog(:,MagColInd(Icol)) = ColMag + AperCorr + Args.ZP;
                
                % add: AperCorr to catalog:
                if Args.AddAperCorrCol
                    Nsrc = numel(ColMag);
                    AperCorrColName = sprintf('%s%s',Args.AperCorrColPrefix, Cat.ColNames{MagColInd(Icol)});
                    Cat = replaceCol(Cat, AperCorr.*ones(Nsrc,1), AperCorrColName, Inf, Args.AperCorrColUnits);
                end
            end
        end

        % This should happen automatically, but we are doing this for
        % readability and order
        if isa(Obj, 'AstroImage')
            Result(Iobj).CatData = Cat;
        else
            Result(Iobj) = Cat;
        end
    end



end
