function Image = subtractMeanColRow(Image, Args)
    % Subtract the collapsed median of rows and columns from an image.
    %       This function may be useful in order to remove lines correlated
    %       noise from images.
    %       However, it is not taking into account the background of the
    %       image. See imUtil.background.backgroundMeanColRow for full
    %       treatment.
    % Input  : - A 2D matrix.
    %          * ...,key,val,...
    %            'Back'      - A background image. If not empty, then, will
    %                   remove the background image, then subtract the
    %                   row/columns collapse median, and then return the
    %                   background image (if RetBack=true).
    %                   I.e., remove only the correlated
    %                   noise, but keep the background as it.
    %                   Default is [].
    %            'RetBack' - Return the background to the image.
    %                   Default is true.
    %            'SubMedRow' - Subtract median of rows. Default is true.
    %            'SubMedCol' - Subtract median of columns. Default is true.
    %            'MeanFun' - Function handle for mean. Second argument of
    %                   function must be the dimension index.
    %                   Default is @imUtil.background.rmeanCol.
    %            'MeanFunArgs' - A cell array of additional arguments to
    %                   pass to the mean function, after the dimension
    %                   argument. Default is {'omitnan'}.
    % Output : - Row/lines background subtracted image.
    % Author : Eran Ofek (Dec 2021)
    % Example: R = rand(1600,1600);
    %          R = imUtil.background.subtractMeanColRow(R)
    
    arguments
        Image
        Args.Back                     = [];
        Args.RetBack logical          = true;
        Args.SubMedRow logical        = true;
        Args.SubMedCol logical        = true;
        Args.MeanFun function_handle  = @median; %@imUtil.background.rmeanCol; %@median;
        Args.MeanFunArgs cell         = {}; %{'omitnan'};
    end
    
    if ~isempty(Args.Back)
        Image = Image - Args.Back;
    end
    
    if Args.SubMedRow
        MedianRow = Args.MeanFun(Image, 1, Args.MeanFunArgs{:});
        Image     = Image - MedianRow;
    end
    if Args.SubMedCol
        MedianCol = Args.MeanFun(Image, 2, Args.MeanFunArgs{:});
        Image     = Image - MedianCol;
    end
    
    if Args.RetBack
        Image = Image + Args.Back;
    end
    
    
end