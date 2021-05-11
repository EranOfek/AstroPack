function [M1,M2,Aper]=moment2(Image,X,Y,Args)
% Calculate 1st, 2nd moments and (weighted) aperture photometry 
% Package: @imUtil.image
% Description: Given a 2D image, or a 3D cube of image stamps, and X, Y
 %             coordinates of sources (or the center of the stamps),
%              calculate for each stamp the 1st and 2nd moments, and
%              aperture photometry.
%              By default, first moment is calculated iteratively around
%              the guess position. It is calculated using windowing (i.e.,
%              multiplying the stamp by a weight function). The user can
%              supply the weight function, but by default it is a Gaussian
%              with a radius specified by the user. By default, the weight
%              function width is adapeted iteratively, where in the first
%              iteration a flat window is used, and then a Gaussian with
%              smaller and smaller sigma is used (see code for details).
%              The central second moment is calculated around the 1st
%              moment position with the weight function.
%              Aperture photometry is calculated in a list of apertures.
%              Also calculated is the total flux in the stamps, and the
%              weighted aperture photometry (weighted by the weight
%              function and properly normalized). This is roughly
%              equivalent to PSF photometry.
%              Note that the measurment of the second moment may be highly
%              biased and it can be usually used only as a relative
%              quantity.
% Input  : - a 2D image (background subtracted), or a 3D cube of cutouts
%            (3rd dim is ciutout index).
%          - A vector of X coordinates around to calculate moments.
%          - A vector of Y coordinates around to calculate moments.
%          * Pairs of ...,key,val,... The following keywords are available:
%            'AperRadius' - Vector of aperture radii, in which to calculate
%                       aperture photometry.
%                       Default is [2 4 6].
%            'Annulus' - Vector of inner and outer radius of background
%                       annulus. Default is [8 12].
%            'BackFun' - Function handle to use for background estimation.
%                       In order to meaningful this function must ignore
%                       NaNs.
%                       Default is @nanmedian.
%            'MomRadius' - Radius around position in which to calculate the
%                       moments. Recomended ~1.7 FWHM. Default is 8.
%            'WeightFun' - The weight function to use for weighting the
%                       moments and the weighted photometry.
%                       This can be a scalar indicating the sigma of a
%                       circularly symmetric Gaussian,
%                       or a function handle that matrix of radii, and
%                       return a matrix of weights (e.g., @(r)
%                       exp(-r.^2./(2.*4))./(2.*pi.*4.^2); ).
%                       Default is 2.
%            'Circle' - A flag indicating if to extract the stamps with
%                       circular shape. Default is false.
%            'MaxIter' - Maximum number of 1st moment position iterations.
%                       0 will perform aingle 1st moment calculation.
%                       Default is 10.
%            'NoWeightFirstIter' - A flag indicating if not to apply weight
%                       on the first itearation. Default is true.
%            'PosConvergence' - Position convergence. Default is 1e-4.
%            'DynamicWindow' - Apply dynamic windowing. Default is true.
%            'WindowOnlyOnLastIter' -Default is false.
%            'FinalIterWithCorrectWin' - Apply an additional final
%                       iteration with the correct window.
%            'mexCutout' - use imUtil.image.mexCutout.m (true) or
%                       imUtil.image.find_within_radius_mat (false).
%                       Default is true.
% Output  : - First moment information. 
%             A structure with the following fields.
%             .RoundX - Vector of roundex X position
%             .RoundY - Vector of roundex Y position
%             .DeltaLastX - Vector of the X shifts in the last position
%                           iteration.
%             .DeltaLastY - Vector of the Y shifts in the last position
%                           iteration.
%             .Iter - Number of position iterations.
%             .X    - 1st moment X position
%             .Y    - 1st moment Y position.
%             .Xstart - Starting X position,
%             .Ystart - Starting Y position.
%           - A second momement information.
%             A structure with the following fields.
%             .X2 - X^2 2nd moment.
%             .Y2 - Y.^2 2nd moment.
%             .XY - X*Y 2nd moment.
%           - Photometry information. A structure with the following fields.
%             .AperRadius - Vector of apertures radius.
%             .AperPhot - Matrix of aperture photometry. Column per
%                         aperture.
%             .AperArea - Matrix of apertures area. Column per aperture.
%             .BoxPhot  - Vector of the full box photometry
%             .AnnulusBack - Annulus background.
%             .AnnulusStd - Annulus StD.
%             .WeightedAper - Weighted photometry. Weighted by the user
%                           specified weight function.
%      By: Eran O. Ofek                       Apr 2020             
% Example: Image = rand(1024,1024); X=rand(1e4,1).*1023+1; Y=rand(1e4,1).*1023+1;
%          [M1,M2,Aper]=imUtil.image.moment2(Image,X,Y);
%          Matrix = imUtil.kernel2.gauss(2, [16 16]);
%          [M1,M2,Aper]=imUtil.image.moment2(Matrix,16,16)
%          [M1,M2,Aper]=imUtil.image.moment2(Matrix,16,16,'WeightFun',@(r) 1)
%          Cube = imUtil.kernel2.gauss([2;2.1;2.2], [31 31]);
%          [M1,M2,Aper]=imUtil.image.moment2(Cube,16,16)

arguments
    Image             {mustBeNumeric(Image)}
    X                 {mustBeNumeric(X)}
    Y                 {mustBeNumeric(Y)}
    Args.AperRadius   {mustBeNumeric(Args.AperRadius)} = [2 4 6];
    Args.Annulus      {mustBeNumeric(Args.Annulus)}    = [8 12];
    Args.BackFun                                       = @nanmedian
    Args.MomRadius    {mustBeNumeric(Args.MomRadius)}  = 8;    % recomended ~1.7 FWHM
    Args.WeightFun                                     = 2;    % sigma or function: @(r) exp(-r.^2./(2.*4))./(2.*pi.*4.^2);
    Args.Circle(1,1) logical                           = false;
    Args.MaxIter      {mustBeNumeric(Args.MaxIter)}    = 10;
    Args.NoWeightFirstIter(1,1) logical                = true; 
    Args.PosConvergence                                = 1e-4;
    Args.DynamicWindow(1,1) logical                    = true;
    Args.WindowOnlyOnLastIter(1,1) logical             = false;
    Args.FinalIterWithCorrectWin(1,1) logical          = true;
    Args.mexCutout(1,1) logical                        = true;
end


MaxRadius  = max(Args.MomRadius, Args.Annulus(2));   % need to be larger than all the rest
Naper      = numel(Args.AperRadius);


NdimImage = ndims(Image);
if NdimImage==2
    % Image is 2D - build a cube of 2D stamps
    if Args.mexCutout
        [Cube]=imUtil.image.mexCutout(Image,[X,Y],MaxRadius.*2+1);
        Cube  = squeeze(Cube);
        RoundX = round(X);
        RoundY = round(Y);
    else
        [Cube,RoundX,RoundY]=imUtil.image.find_within_radius_mat(Image,X,Y,MaxRadius,Args.Circle);
    end
    
    
elseif NdimImage==3
    % Image is already a cube of stamps
    Cube   = Image;
    RoundX = round(X);
    RoundY = round(Y);
    [~,~,Nc] = size(Cube);
    if numel(X)==1
        RoundX = RoundX.*ones(Nc,1);
        X      = X.*ones(Nc,1);
    end
    if numel(Y)==1
        RoundY = RoundY.*ones(Nc,1);
        Y      = Y.*ones(Nc,1);
    end
    
else
    error('Image number of dimensions must be 2 or 3');
end

SizeCube = size(Cube);
if (SizeCube(1)~=SizeCube(2))
    error('First two dimensions of cube must be equal');
end
Vec = (1:1:SizeCube(1)) - SizeCube(1).*0.5 - 0.5;
[MatX,MatY] = meshgrid(Vec,Vec);
MatR        = sqrt(MatX.^2 + MatY.^2);

% apply Gaussian weight
if isa(Args.WeightFun,'function_handle')
    % WeightFun is a function handle
    W = Args.WeightFun(MatR.^2);
elseif isnumeric(Args.WeightFun)
    % WeightFun is assumed to be the sigma of a Gaussian
    W   = exp(-0.5.*(MatR./Args.WeightFun).^2)./(2.*pi.*Args.WeightFun.^2);
    %W         = GaussFun(MatR,WeightFun);   % exp(-MatR.^2./(2.*WeightFun));
else
    error('WeightFun must be a function handle or a numeric scalar');
end
% construct a window with maximal radiu
W_Max = ones(size(MatR));
W_Max(MatR>Args.MomRadius) = 0;

Nsrc = numel(X);


%M1.X = RoundX + squeeze(nansum(W.*Cube.*MatX,[1 2]))./squeeze(nansum(W.*Cube,[1 2]));
%M1.Y = RoundY + squeeze(nansum(W.*Cube.*MatY,[1 2]))./squeeze(nansum(W.*Cube,[1 2]));

% 1st moment
if Args.NoWeightFirstIter
    % no weight function on 1st iteration
    % this is good for much faster convergene
    WInt = W_Max.*Cube;
else
    WInt = W.*W_Max.*Cube; % Weighted intensity
end
Norm = 1./squeeze(sum(WInt,[1 2]));  % normalization
% 1st moment relative to the stamp center
CumRelX1 = squeeze(sum(WInt.*MatX,[1 2])).*Norm;
CumRelY1 = squeeze(sum(WInt.*MatY,[1 2])).*Norm;
RelX1    = CumRelX1;
RelY1    = CumRelY1;

M1.RoundX = RoundX;
M1.RoundY = RoundY;

M1.DeltaLastX = RelX1;
M1.DeltaLastY = RelY1;
Iter = 0;
while Iter<Args.MaxIter && any(abs(M1.DeltaLastX)>Args.PosConvergence) && any(abs(M1.DeltaLastY)>Args.PosConvergence)
    Iter = Iter + 1;
    % the MatX/MatY cube - shifted to the first moment position
    MatXcen = MatX - reshape(CumRelX1,1,1,Nsrc);
    MatYcen = MatY - reshape(CumRelY1,1,1,Nsrc);

    MatR        = sqrt(MatXcen.^2 + MatYcen.^2);

    % apply Gaussian weight to the new centeral matrix
    if ~Args.WindowOnlyOnLastIter
        if isa(Args.WeightFun,'function_handle')
            % WeightFun is a function handle
            W = Args.WeightFun(MatR.^2);
        elseif isnumeric(Args.WeightFun)
            % WeightFun is assumed to be the sigma of a Gaussian
            if Args.DynamicWindow
                % tested a few options of dynamic windowing:
                %Factor = sqrt(sqrt(abs(M1.DeltaLastX)./PosConvergence).*sqrt(abs(M1.DeltaLastY)./PosConvergence));
                %Factor = log10( abs(M1.DeltaLastX)./PosConvergence.* abs(M1.DeltaLastY)./PosConvergence ) + 1;
                Factor = max((30./Iter).^0.5,1);
            else
                Factor = 1;
            end
            W   = exp(-0.5.*(MatR./(Args.WeightFun.*Factor)).^2)./(2.*pi.*(Args.WeightFun.*Factor).^2);
            %W         = GaussFun(MatR,WeightFun.*Factor);
            %W         = exp(-MatR.^2./(2.*(WeightFun.*Factor).^2));
        else
            error('WeightFun must be a function handle or a numeric scalar');
        end
    end

    % construct a window with maximal radius
    W_Max = ones(size(MatR));
    W_Max(MatR>Args.MomRadius) = 0;


    WInt = W.*W_Max.*Cube; % Weighted intensity
    Norm = 1./squeeze(sum(WInt,[1 2]));  % normalization
    
    CumRelX1 = CumRelX1 + squeeze(sum(WInt.*MatXcen,[1 2])).*Norm;
    CumRelY1 = CumRelY1 + squeeze(sum(WInt.*MatYcen,[1 2])).*Norm;

    M1.DeltaLastX = CumRelX1 - RelX1;
    M1.DeltaLastY = CumRelY1 - RelY1;
    RelX1         = CumRelX1;
    RelY1         = CumRelY1; 
end

% final iteration with the correct window
if Args.FinalIterWithCorrectWin
    Iter = Iter + 1;
    % the MatX/MatY cube - shifted to the first moment position
    MatXcen = MatX - reshape(CumRelX1,1,1,Nsrc);
    MatYcen = MatY - reshape(CumRelY1,1,1,Nsrc);

    MatR        = sqrt(MatXcen.^2 + MatYcen.^2);

    % apply Gaussian weight to the new centeral matrix
    if isa(Args.WeightFun,'function_handle')
        % WeightFun is a function handle
        W = Args.WeightFun(MatR.^2);
    elseif isnumeric(Args.WeightFun)
        %Factor = 1;
        W   = exp(-0.5.*(MatR./Args.WeightFun).^2)./(2.*pi.*Args.WeightFun.^2);
        %W         = GaussFun(MatR,WeightFun);
        %W         = exp(-MatR.^2./(2.*(WeightFun.*Factor).^2));
    else
        error('WeightFun must be a function handle or a numeric scalar');
    end
    % construct a window with maximal radiu
    W_Max = ones(size(MatR));
    W_Max(MatR>Args.MomRadius) = 0;


    WInt = W.*W_Max.*Cube; % Weighted intensity
    Norm = 1./squeeze(sum(WInt,[1 2]));  % normalization

    CumRelX1 = CumRelX1 + squeeze(sum(WInt.*MatXcen,[1 2])).*Norm;
    CumRelY1 = CumRelY1 + squeeze(sum(WInt.*MatYcen,[1 2])).*Norm;

    M1.DeltaLastX = CumRelX1 - RelX1;
    M1.DeltaLastY = CumRelY1 - RelY1;
end

M1.Iter = Iter;


M1.X = RoundX + CumRelX1;
M1.Y = RoundY + CumRelY1;
M1.Xstart = X;
M1.Ystart = Y;

if nargout>1
    % 2nd moment
    % the MatX/MatY cube - shifted to the first moment position
    MatXcen = MatX - reshape(RelX1,1,1,Nsrc);
    MatYcen = MatY - reshape(RelY1,1,1,Nsrc);
    
    M2.X2 = squeeze(sum(WInt.*MatXcen.^2,[1 2])).*Norm;
    M2.Y2 = squeeze(sum(WInt.*MatYcen.^2,[1 2])).*Norm;
    M2.XY = squeeze(sum(WInt.*MatXcen.*MatYcen,[1 2])).*Norm;
    
    if nargout>2
        % aperture photometry
        Aper.AperRadius = Args.AperRadius;

        % simple aperture photometry in centered pixeleted aperure
        Aper.AperPhot = zeros(Nsrc,Naper);
        Aper.AperArea = zeros(Nsrc,Naper);
        for Iaper=1:1:Naper
            AperFilter = ones(size(MatR));
            AperFilter(MatR>Args.AperRadius(Iaper)) = 0;
            Aper.AperPhot(:,Iaper) = sum(Cube.*AperFilter,[1 2]);
            Aper.AperArea(:,Iaper)   = squeeze(sum(AperFilter,[1 2]));
        end


        % total box photometry - on non centred position
        Aper.BoxPhot = squeeze(nansum(Cube,[1 2]));

        % Annulus background
        BackFilter = nan(size(MatR));
        BackFilter(MatR>Args.Annulus(1) & MatR<Args.Annulus(2)) = 1;
        BackCube = BackFilter.*Cube;
        % note - use NaN ignoring functions!
        Aper.AnnulusBack = squeeze(Args.BackFun(BackCube,[1 2]));
        Aper.AnnulusStd  = squeeze(nanstd(BackCube,0,[1 2]));

        % weighted aperture photometry
        Aper.WeightedAper = squeeze(sum(WInt,[1 2])./sum((W.*W_Max).^2,[1 2])); 

        % Aperure photometry on shifted kernel

    end
end





    
