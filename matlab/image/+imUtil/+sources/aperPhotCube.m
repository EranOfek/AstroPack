function Result = aperPhotCube(Cube, X, Y, Args)
    % Aperture and simple PSF (no-fit) photometry on a cube of stamps
    % including sub-pix shifts.
    %       This function may optional center the sources (given their
    %       position) using fft or Lanczos shifts.
    %       The PSF photometry is calculated without fitting (just PSF
    %       weighting after optional sub pixel shift), according to the
    %       scheme of Zackay & Ofek (2017; ApJ 836, 187).
    % Input  : - A cube of stamps, in which the stamp index is in the 3rd
    %            dimension.
    %          - Vector of X position (one per stamp) of the sources in the
    %            stamps.
    %          - Vector of Y position (one per stamp) of the sources in the
    %            stamps.
    %          * ...,key,val,...
    %            'AperRad' - A vector of aperture radii [pix].
    %                   Default is [2,4,5].
    %            'AnnulusRad' - [Inner, Outer] sky background annulus
    %                   radius. If scalar, then this is the annulus width,
    %                   where the outer radius is given by the stamp half
    %                   size. Default is 3.
    %            'SubBack' - A logical indicating if to subtract the
    %                   measured background before measurments.
    %                   Default is true.
    %            'BackFun' - A function handle for mean background
    %                   calculation. The function is of the forms
    %                   Fun(Data,Dim,...)
    %                   Default is @medain
    %            'BackFunArgs' - A cell array of additional arguments to
    %                   pass to 'BackFun' (after the dimension arguments.
    %                   Defauly is {'omitnan'}.
    %            'StdFun' - A function handle for std background
    %                   calculation. The function is of the forms
    %                   Fun(Data,Dim,...)
    %                   Default is @std
    %            'StdFunArgs' - A cell array of additional arguments to
    %                   pass to 'StdFun' (after the dimension arguments.
    %                   Defauly is {'omitnan'}.
    %            'SubPixShift' - The sub pixel shift method:
    %                   'lanczos' - use imUtil.trans.shift_lanczos
    %                   'fft' - use imUtil.trans.shift_fft
    %                   'none' - no interpolation.
    %            'A' - Lanczos order parameter. Default is 3.
    %            'IsCircFilt' - Logical for Lanczos circular filtering.
    %                   Default is true.
    %            'PadVal' - Laczos padding value. Default is 0.
    %            'BoxPhot' - A logical indicating if to return the Box
    %                   (stamp integral) photometry.
    %                   Default is true.
    %            'PSF' - A cube of PSFs with which to calculate PSF photometry.
    %                   The PSF index is in the 3rd dimension.
    %                   If PSF is given, then SubBack should be true.
    %                   If empty, then do not calculate PSF photometry.
    %                   Default is [].
    % Output : - A structure containing the following fields:
    %            .AnnulusBack - Vector of annulus background mean.
    %            .AnnulusStd - Vectof of annulus background std.
    %            .AnnulusBackArea - Back area.
    %            .AperPhot - A matrix with photometry, column per aperture.
    %            .AperArea - A vector of aperture area (element per
    %                   aperture).
    %            .AperRadius - A vector of aperture radii (element per
    %                   aperture).
    %            .PsfPhot - A matrix with photometry, column per PSF.
    %            .BoxPhot (optional)
    % Example: Cube = randn(17,17,4000); X=ones(4000,1).*9; Y=X;
    %          imUtil.sources.aperPhotCube(Cube, X, Y);
    %          PSF  = imUtil.kernel2.gauss([1.5;2],[17 17]);
    %          imUtil.sources.aperPhotCube(Cube, X, Y, 'PSF',PSF)
    %          imUtil.sources.aperPhotCube(Cube, X, Y, 'PSF',PSF,'SubPixShift','fft')
    %          
    
    arguments
        Cube    % image index is in thrid dimension
        X
        Y
        Args.AperRad                  = [2,4,5];
        Args.AnnulusRad               = 3;     % either [Rin, Rout] or width, where Rout is half size
        Args.SubBack logical          = true;
        Args.BackFun function_handle  = @median
        Args.BackFunArgs cell         = {'omitnan'};
        Args.StdFun function_handle   = @std
        Args.StdFunArgs cell          = {'omitnan'};
        
        Args.SubPixShift              = 'fft';    % 'lanczos' | 'fft' | 'none'
        Args.A                        = 3;
        Args.IsCircFilt logical       = true;
        Args.PadVal                   = 0;
        
        Args.BoxPhot logical          = false;
        Args.PSF                      = [];   % PSF index is in 3rd dim
    end
        
    [SizeY, SizeX, Nim] = size(Cube);
    
    AperRad2 = Args.AperRad.^2;
    Naper    = numel(AperRad2);
    
    if numel(Args.AnnulusRad)==1
        % width, where Rout is half size
        Rout = 0.5.*min(SizeX, SizeY);
        Args.AnnulusRad = [Rout-Args.AnnulusRad, Rout];  % [Rin, Rout]
    end
    Rin2  = Args.AnnulusRad(1).^2;
    Rout2 = Args.AnnulusRad(2).^2;
    
    
    VecX  = 0.5.*SizeX + 0.5 - (1:1:SizeX);
    VecY  = 0.5.*SizeY + 0.5 - (1:1:SizeY).';
    MatR2 = VecX.^2 + VecY.^2;
    
    Flag  = cast(MatR2<Rin2 | MatR2>Rout2, 'like',Cube);
    Flag(Flag==1) = NaN;
    CubeAnnulus = Cube+Flag;
    Result.AnnulusBack  = Args.BackFun(CubeAnnulus, [1 2], Args.BackFunArgs{:});
    Result.AnnulusStd   = Args.StdFun(CubeAnnulus, 0, [1 2], Args.StdFunArgs{:});
    Result.AnnulusBackArea = pi.*(Rout2 - Rin2);
    
    % subtract background
    if Args.SubBack
        Cube = Cube - Result.AnnulusBack;
    end
    Result.AnnulusBack = squeeze(Result.AnnulusBack);
    Result.AnnulusStd  = squeeze(Result.AnnulusStd);
     
    ShiftXY = [ceil(SizeX.*0.5) - X, ceil(SizeY.*0.5) - Y];  % is this correct?
    
    % sub pixel shift
    switch Args.SubPixShift   
        case 'lanczos'
            [Cube] = imUtil.trans.shift_lanczos(Cube, ShiftXY, Args.A, Args.IsCircFilt, Args.PadVal);
        case 'fft'
            [Cube] = imUtil.trans.shift_fft(Cube, ShiftXY(:,1), ShiftXY(:,2));            
        case 'none'
            % no sub pixel shift
            % need to prepare a new version of MatR2 with the correct
            % positions
            
            VecX  = X(:) - (1:1:SizeX);
            VecY  = Y(:) - (1:1:SizeY);
            
            MatR2 = zeros(SizeY, SizeX, Nim);
            for Iim=1:1:Nim
                MatR2(:,:,Iim) = VecX(Iim,:).^2 + (VecY(Iim,:).').^2;
            end
                
            
        otherwise
            error('Unknown SubPixShift option');
    end
    
    % aperture photometry
    Result.AperArea   = pi.*AperRad2;
    Result.AperRadius = Args.AperRad;
    Result.AperPhot   = zeros(Nim, Naper);
    for Iaper=1:1:Naper        
        Result.AperPhot(:,Iaper)   = squeeze(sum(Cube.*(MatR2 < AperRad2(Iaper)),[1 2],'omitnan'));
    end
    
    if Args.BoxPhot
        Result.BoxPhot = squeeze(sum(Cube,[1 2],'omitnan'));
    end
    
    if ~isempty(Args.PSF)
        [SizePY, SizePX, Npsf] = size(Args.PSF);
        if (SizePX~=SizeX) || (SizePY~=SizeY)
            error('PSF X/Y size must be equalt to image stamps X/Y size');
        end
        
        NormPSF = squeeze(sum(Args.PSF.^2,[1 2]));
        Result.PsfPhot = zeros(Nim,Npsf);
        for Ipsf=1:1:Npsf
            Result.PsfPhot(:,Ipsf) = squeeze(sum(Cube.*Args.PSF(:,:,Ipsf),[1 2],'omitnan')./NormPSF(Ipsf));
        end
        
    end
    
end