function [Back, Std] = backgroundCube(Cube, Args)
    % For a cube of stamps, calculate the background and std in an annulus
    % in each stamp.
    % Input  : - A cube of stamps. The 3rd dimension contains the stamp
    %            index. The code was tested on odd size stamps.
    %          * ...,key,val,...
    %            'AnnulusRad' - Either [inner, outer] or [width] of
    %                   annulus. If width, then the outer radius is given
    %                   by the stamp (size - 1)/2.
    %                   Default is 2.
    %            'StdFun' - A function handle for calculating the std in
    %                   the cube (where the non annulus pixels are set to
    %                   NaN). Default is @std.
    %            'StdFunArgs' - A cell array of arguments to pass to the
    %                   StdFun. Default is {[],[1 2],'omitnan'}.
    %            'MeanFun' - A function handle for calculating the mean in
    %                   the cube (where the non annulus pixels are set to
    %                   NaN). Default is @median.
    %            'MeanFunArgs' - A cell array of arguments to pass to the
    %                   MeanFun. Default is {[1 2],'omitnan'}.
    %            'Squeeze' - Squeeze the output. Default is true.
    % Output : - A vector of background per stamp.
    %          - A vector of std per stamp.
    % Author : Eran Ofek (Dec 2021)
    % Example: Cube = randn(15,15,4000);
    %          [Back, Std] = imUtil.sources.backgroundCube(Cube)
    
    arguments
        Cube
        Args.AnnulusRad                = 2;  % if scalar than width of annulus
        Args.StdFun function_handle    = @std
        Args.StdFunArgs cell           = {[],[1 2],'omitnan'};
        Args.MeanFun function_handle   = @median;
        Args.MeanFunArgs cell          = {[1 2],'omitnan'};
        Args.Squeeze logical           = true;
    end
        
  
    [Ny, Nx, Nim] = size(Cube);

    if numel(Args.AnnulusRad)==1
        % calc [inner, outer] annulus radius
        Args.AnnulusRad = (Nx-1).*0.5 - [Args.AnnulusRad, 0];
    end
    AnnulusRad2 = Args.AnnulusRad.^2;

    Xcenter = Nx.*0.5 + 0.5;
    Ycenter = Ny.*0.5 + 0.5;

    VecXrel = (1:1:Nx) - Xcenter;
    VecYrel = (1:1:Ny) - Ycenter;
    MatR2   = VecXrel.^2 + VecYrel(:).^2;

    FlagAnnulus = cast(MatR2>AnnulusRad2(1) & MatR2<AnnulusRad2(2), 'like',Cube);
    FlagAnnulus(~FlagAnnulus) = NaN;


    Back = Args.MeanFun(Cube.*FlagAnnulus, Args.MeanFunArgs{:});

    if nargout>1
        Std  = Args.StdFun(Cube.*FlagAnnulus,  Args.StdFunArgs{:});
    end

    if Args.Squeeze
        Back = squeeze(Back);
        if nargout>1
            Std  = squeeze(Std);
        end
    end
end