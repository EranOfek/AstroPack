function [Result, Mean, Std, Max] = identifyFlaringPixels(Cube, Args)
    % Identify flaring pixels in a cube of images
    %       Searched by looking at (Cube-Mean)/Std>Threshold
    % Input  : - A cube of images. Usulally the image index is in
    %            the 3rd dimension.
    %          * ...,key,val,...
    %            'MeanFun' - Either a number, a matrix or a
    %                   function handle by which to calculate the
    %                   mean function. Default is @median.
    %            'MeanFunArgs' - A cell array of arguments to pass
    %                   to the mean function.
    %                   Default is {3,'omitnan'}.
    %            'MaxFun' - A function handle by which to calculate
    %                   the max of the data. Default is @max.
    %            'MaxFunArgs' - A cell array of arguments to pass
    %                   to the max function.
    %                   Default is {[],3}.
    %            'StdFun' - Either a number, a matrix or a
    %                   function handle by which to calculate the
    %                   mean function. Default is @imUtil.background.rstd
    %            'StdFunArgs' - A cell array of arguments to pass
    %                   to the std function.
    %                   Default is {3}.
    %            'Threshold' - Threshold above to flag the pixel.
    %                   Default is 10.
    % Output : - A matrix of logicals indicating pixels that are
    %            above the flaring threshold.
    %          - A matrix of the mean values.
    %          - A matrix of the std values.
    %          - A matrix of the max values.
    % Author : Eran Ofek (May 2021)
    % Example: Cube = randn(100,100,10); Cube(1,1,1)=30;
    %          [Result,Mean,Std,Max] = imProc.dark.identifyFlaringPixels(Cube);
    %          [Result,Mean,Std,Max] = imProc.dark.identifyFlaringPixels(Cube,'MeanFunArgs',{'all'});

    arguments
        Cube
        Args.MeanFun                            = @median;  % or number or imagew
        Args.MeanFunArgs cell                   = {3,'omitnan'};
        Args.MaxFun function_handle             = @max;
        Args.MaxFunArgs cell                    = {[],3};
        Args.StdFun                             = @imUtil.background.rstd;   % or number or image
        Args.StdFunArgs cell                    = {3};
        Args.Threshold                          = 20;   % number of sigmas
    end

    if isa(Args.MeanFun,'function_handle')
        Mean = Args.MeanFun(Cube, Args.MeanFunArgs{:});
    else
        Mean = Args.MeanFun;
    end
    Max  = Args.MaxFun(Cube,  Args.MaxFunArgs{:});
    if isa(Args.StdFun,'function_handle')
        Std = Args.StdFun(Cube, Args.StdFunArgs{:});
    else
        Std = Args.StdFun;
    end

    Result = (Max - Mean)./Std > Args.Threshold;

end
