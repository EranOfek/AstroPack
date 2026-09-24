function [GoodMask] = maskByPos(Array, Dim, Args)
    % Generate a logicals mask of good pixels based on ranges of positions to ignore.
    % Input  : - An array.
    %          - Dimension in which will be masked. I.e., Complementry
    %            dimenson to which the coordinates in IgnoreRanges are
    %            specified. Default is 1.
    %          * ...,key,val,... 
    %            'IgnoreRanges' - Two column matrix of [Min Max] of
    %                   positons to set to false in the imUt    GoodMask.
    %                   If empty, return a matrix of true.
    %                   Default is [].
    %            'GoodMask' - An optional good mask to combine with the generated
    %                   mask. Default is [].
    %            'Operator' - Operator to use for combining the makss.
    %                   Default is @and.
    % Output : - A matrix of logical indicating good pixels to use.
    %            (i.e., not in the specified ranges).
    % Author : Eran Ofek (2024 Dec) 
    % Example: imUtil.mask.maskByPos(Array)

    arguments
        Array
        Dim                    = 1;
        Args.IgnoreRanges      = [];
        Args.GoodMask          = [];
        Args.Operator          = @and;
    end

    if Dim==2
        Array = Array.';
    end
    
    [SizeI, SizeJ] = size(Array);
    GoodMask = true(SizeI, SizeJ);
    if ~isempty(Args.IgnoreRanges)
        [MatX, ~]   = meshgrid((1:1:SizeJ),(1:1:SizeI));
        
        Nreg = size(Args.IgnoreRanges,1);
        for Ireg=1:1:Nreg
            GoodMask(MatX>Args.IgnoreRanges(Ireg,1) & MatX<Args.IgnoreRanges(Ireg,2)) = false;
        end
    end
    
    if Dim==2
        GoodMask = GoodMask.';
    end
    
    if ~isempty(Args.GoodMask)
        GoodMask = Args.Operator(GoodMask, Args.GoodMask);
    end
end
