function [Result,FlagGood] = xpeak_x1_diff(Obj, Args)
    % Set bit mask and optionally remove from catalog sources with X1 position very different than XPEAK
    % Input  : - An AstroImage object with populated Mask and Catalog.
    %          * ...,key,val,...
    %            'ColXPEAK' - Column name for XPEAK position. Default is 'XPEAK'.
    %            'ColYPEAK' - Column name for XPEAK position. Default is 'YPEAK'.
    %            'ColX1' - Column for X1 position. Default is 'X1'.
    %            'ColY1' - Column for X1 position. Default is 'Y1'.
    %            'XYDiffBitName' - Bit name in the mask image to set the
    %                   X1,Y1 positions of sources with large shifts from
    %                   XPEAK,YPEAK.
    %                   Default is ''.
    %            'MaxDist' - Distance above to declare a bad source.
    %                   Default is 1.5 pix.
    %            'SetMask' - A logical indicating if to set the bit mask.
    %                   Default is false.
    %            'RemoveFromCat' - If true, then remove bad sources from
    %                   catalog. Default is true.
    %            'CreateNewObj' - If true, then will create a new copy of
    %                   the AstroImage object. Default is false.
    %
    % Output : - An updated AstroImage object.
    % Author : Eran Ofek (Jan 2024)
    % Example: AI = imProc.mask.xpeak_x1_diff(AI)
    
    arguments
        Obj AstroImage
                
        Args.ColXPEAK                    = 'XPEAK';
        Args.ColYPEAK                    = 'YPEAK';
        Args.ColX1                       = 'X1';
        Args.ColY1                       = 'Y1';
        
        Args.BitNameDiffXY               = '';
        Args.MaxDist                     = 1.5;

        Args.SetMask logical             = false;
        Args.RemoveFromCat logical       = true;

        Args.CreateNewObj logical        = false;
       
    end
    
    MaxDist2 = Args.MaxDist.^2;

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % for each image
    
        Cols = getCol(Obj(Iobj).CatData, {Args.ColXPEAK, Args.ColYPEAK, Args.ColX1, Args.ColY1});
        XP    = Cols(:,1);
        YP    = Cols(:,2);
        X1    = Cols(:,3);
        Y1    = Cols(:,4);

        Dist2 = (XP-X1).^2 + (YP-Y1).^2;
        FlagGood = Dist2<MaxDist2;
        
        if Args.SetMask && ~isempty(Args.BitNameDiffXY)
            IndFlag = find(~FlagGood);
            %FlagImage = false(SizeImage);
            Find = imUtil.image.sub2ind_fast(Obj(Iobj).sizeImage, round(Y1(IndFlag)), round(X1(IndFlag)));
            %FlagImage(Find) = true;
            %Result(Iobj).MaskData = maskSet(Obj(Iobj).MaskData, FlagImage, Args.BitNameCR, 1);
            Result(Iobj).MaskData = maskSet(Obj(Iobj).MaskData, Find, Args.BitNameDiffXY, 1);
        end
         
        if Args.RemoveFromCat
            % remove possible CR from catalog
            Result(Iobj).CatData = Result(Iobj).CatData.selectRows(FlagGood);
            
        end
    end

end
