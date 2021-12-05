function [NewBack, BackSubImage] = backgroundMeanColRow(Image, Back, Args)
    % Given an image and a background image, refine the background image
    %   by subtracting the lines/rows correlated noise.
    %   The following operations are performed:
    %   1. BackSubImage = Image - Back
    %   2. Subtract collapsed median rows from BackSubImage
    %   3. Subtract collapsed median columns from BackSubImage
    %   4. NewBack = Image - BackSubImage
    % Input  : - An image.
    %          - A background image with the same size as the input image.
    %          * ...,key,val,...
    %            'SubMedRow' - Subtract median of rows. Default is true.
    %            'SubMedCol' - Subtract median of columns. Default is true.
    %            'MeanFun' - Function handle for mean. Second argument of
    %                   function must be the dimension index.
    %                   Default is @median.
    %            'MeanFunArgs' - A cell array of additional arguments to
    %                   pass to the mean function, after the dimension
    %                   argument. Default is {'omitnan'}.
    % Output : - The new background image.
    %          - The background subtracted image.
    % Author : Eran Ofek (Dec 2021)
    % Example: R=randn(1600,1600);
    %          [NewBack, BackSubImage] = imUtil.background.backgroundMeanColRow(R, 0);
    
    arguments
        Image
        Back
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