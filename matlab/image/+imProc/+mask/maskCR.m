function [Result,FlagGood] = maskCR(Obj, Args)
    % Find cosmic rays (CR) flag them in the mask image, and optionally remove from a catalog.
    %   DeltaHT algorithm: using the SN_1 (S/N for a delta function and
    %   SN_2 (S/N for a finite size PSF), declare a CR if
    %   SN1>(SN2+Args.DeltaSN).
    %
    %   There is an alternaitive implementation in imProc.sources.cleanSources
    % Input  : - An AstroImage object with populated Mask and Catalog.
    %          * ...,key,val,...
    %            'ColX' - Column name for X position in which to mask the
    %                   CR bit mask. Default is 'XPEAK'.
    %            'ColY' - Column name for X position in which to mask the
    %                   CR bit mask. Default is 'YPEAK'.
    %            'AlgoCR' - Algorithm.
    %                   'DeltaHT' - using the SN_1 (S/N for a delta function and
    %                           SN_2 (S/N for a finite size PSF), declare a CR if
    %                           SN1>(SN2+Args.DeltaSN).
    %                   Default is 'DeltaHT'.
    %            'SN_1' - Column name for S/N of delta function.
    %                   Default is SN_1.          
    %            'SN_2' - Column name for S/N of PSF function.
    %                   Default is SN_2.
    %            'DeltaSN' - min difference between S/N. Default is 0.
    %            'BitNameCR' - CR bit name to flag.
    %                   Default is 'CR_DeltaHT'.
    %            'SetMask' - If truem then will set bit mask.
    %                   Default is true.
    %            'RemoveFromCat' - If true, then remove CR sources from
    %                   catalog. Default is true.
    %            'CreateNewObj' - If true, then will create a new copy of
    %                   the AstroImage object. Default is false.
    %
    % Output : - An updated AstroImage object.
    % Author : Eran Ofek (Jan 2024)
    % Example: AI = imProc.mask.maskCR(AI)
    
    arguments
        Obj AstroImage
                
        Args.ColX                    = 'XPEAK';
        Args.ColY                    = 'YPEAK';

        Args.AlgoCR                  = 'DeltaHT';
        Args.SN_1                    = 'SN_1';
        Args.SN_2                    = 'SN_2';
        Args.DeltaSN                 = 0;

        Args.BitNameCR               = 'CR_DeltaHT';

        Args.SetMask logical         = true;
        Args.RemoveFromCat logical   = true;

        Args.CreateNewObj logical    = false;
       
    end
    
      
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % for each image
    
        switch lower(Args.AlgoCR)
            case 'deltaht'
                % apply hypothesis testing using existing catalog
                % with S/N for dekta function and "point sources"
                
                Cols = getCol(Obj(Iobj).CatData, {Args.ColX, Args.ColY, Args.SN_1, Args.SN_2});
                X    = Cols(:,1);
                Y    = Cols(:,2);
                SN1  = Cols(:,3);
                SN2  = Cols(:,4);

                % use find because this is needed twice below, so faster...
                FlagGood = SN1<(SN2+Args.DeltaSN);
                if Args.SetMask
                    IndFlag = find(~FlagGood);
                    %FlagImage = false(SizeImage);
                    Find = imUtil.image.sub2ind_fast(Obj(Iobj).sizeImage, Y(IndFlag), X(IndFlag));
                    %FlagImage(Find) = true;
                    %Result(Iobj).MaskData = maskSet(Obj(Iobj).MaskData, FlagImage, Args.BitNameCR, 1);
                    Result(Iobj).MaskData = maskSet(Obj(Iobj).MaskData, Find, Args.BitNameCR, 1);
                end
            otherwise
                error('Unknown AlgoCR option');
        end

        if Args.RemoveFromCat
            % remove possible CR from catalog
            Result(Iobj).CatData = Result(Iobj).CatData.selectRows(FlagGood);
            
        end
    end
    
end