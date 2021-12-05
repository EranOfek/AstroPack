function Image = subtractMeanColRow(Image, Args)
    % Subtract the collapsed median of rows and columns from an image.
    %       This function may be useful in order to remove lines correlated
    %       noise from images.
    % Input  : - A 2D matrix.
    %          * ...,key,val,...
    %            'SubMedRow' - Subtract median of rows. Default is true.
    %            'SubMedCol' - Subtract median of columns. Default is true.
    %            'MeanFun' - Function handle for mean. Second argument of
    %                   function must be the dimension index.
    %                   Default is @median.
    %            'MeanFunArgs' - A cell array of additional arguments to
    %                   pass to the mean function, after the dimension
    %                   argument. Default is {'omitnan'}.
    % Output : - Row/lines background subtracted image.
    % Author : Eran Ofek (Dec 2021)
    % Example: R = rand(1600,1600);
    %          R = imUtil.background.subtractMeanColRow(R)
    
    arguments
        Image
        Args.SubMedRow logical        = true;
        Args.SubMedCol logical        = true;
        Args.MeanFun function_handle  = @median;
        Args.MeanFunArgs cell         = {'omitnan'};
    end
    
    if Args.SubMedRow
        MedianRow = Args.MeanFun(Image, 1, Args.MeanFunArgs{:});
        Image     = Image - MedianRow;
    end
    if Args.SubMedCol
        MedianCol = Args.MeanFun(Image, 2, Args.MeanFunArgs{:});
        Image     = Image - MedianCol;
    end
    
    
end