function [NewBack, BackSubImage] = backgroundMeanColRow(Image, Back, Args)
    %
    
    arguments
        Image
        Back                          = [];
        Args.SubMedRow logical        = true;
        Args.SubMedCol logical        = true;
        Args.MeanFun function_handle  = @median;
        Args.MeanFunArgs cell         = {'omitnan'};
    end
    
    BackSubImage = Image - Back;
    
    if Args.SubMedRow
        MedianRow    = Args.MeanFun(BackSubImage, 1, Args.MeanFunArgs{:});
        BackSubImage = BackSubImage - MedianRow;
    end
    if Args.SubMedCol
        MedianCol    = Args.MeanFun(BackSubImage, 2, Args.MeanFunArgs{:});
        BackSubImage = BackSubImage - MedianCol;
    end
    
    %Diff = BackSubImage - (Image - Back);
    %NewBack = Back - Diff;
    NewBack = Image - BackSubImage;
    
end