function [SrcStruct, FlagGood] = cleanSources(SrcStruct, Args)
    % Basic source cleaning - removing sharp sources / near edges
    % Input  : - A structure. This must be the structure returned by
    %            imUtil.sources.findSources
    %          * ...,key,val,...
    %            'ColSN_sharp' - The column index in the input structure
    %                   .SN field that corresponds to the S/N for delta
    %                   function. Default is 1.
    %            'ColSN_psf' - The column index in the input structure
    %                   .SN field that corresponds to the S/N for the
    %                   nominal PSF. Default is 2.
    %            'SNdiff' - Threshold for good sources selected by the S/N
    %                   difference. (SN_pdf - SN_sharp) > Args.SNdiff.
    %            'MinEdgeDist' - Minimal edge distance to declare a good
    %                   source. Default is 2.
    %            'ImageSizeXY' - [X Y] of image size. If empty, don't apply
    %                   MinEdgeDist. Default is [].
    %            'RemoveBadSources' - A logical indicating if to remove the
    %                   bad sources. Default is true.
    % Output : - The input structure, optionally without the bad sources.
    %          - The flags of the good sources.
    % Author : Eran Ofek (Dec 2020)
    % Example: [SrcStruct, FlagGood] = imUtil.sources.cleanSources(SrcStruct)
    
    arguments
        SrcStruct
        Args.ColSN_sharp   = 1;
        Args.ColSN_psf     = 2;
        Args.SNdiff        = 0;
        Args.MinEdgeDist   = 2;
        Args.ImageSizeXY   = [];  % if empty, ignore MinEdgeDist
        Args.RemoveBadSources logical   = true;
    end
   
    FlagGood.SN = SrcStruct.SN(:,Args.ColSN_psf) - SrcStruct.SN(:,Args.ColSN_sharp) > Args.SNdiff;
   
    if ~isempty(Args.ImageSizeXY)
        FlagGood.Edge = SrcStruct.XPEAK > Args.MinEdgeDist & ...
                        SrcStruct.YPEAK > Args.MinEdgeDist & ...
                        (Args.ImageSizeXY(1) - SrcStruct.XPEAK) > Args.MinEdgeDist & ...
                        (Args.ImageSizeXY(2) - SrcStruct.YPEAK) > Args.MinEdgeDist;
    else
        FlagGood.Edge = true(size(FlagGood.SN));
    end
    FlagGood.All = FlagGood.SN  & FlagGood.Edge;
   
    if Args.RemoveBadSources
        tools.struct.structcut(SrcStruct, FlagGood.All);
    end
    
end