function [TotMu,Res]=self_microlensing(D, Args)
    % Estimate the self microlensing for binary stars
    % Input  : - The impact parameter at which to calculate the total
    %            magnification, in units of the SrcRad.
    %          * ...,key,val,...
    %            'Dl' - Dist. to lens. Default is 1000.
    %            'Dls' - Dist from lens to source. Default is 0.01./206000
    %            'DistUnits' - Dist. units. Default is 'pc'.
    %            'SrcRad' - Source radius. Default is 6400.
    %            'SrcRadUnits' - Source radius units. Default is 'km'.
    %            'Mass' - Lens mass. Default is 1.4
    %            'MassUnits' - Lens mass units. Default is 'SunM'.
    %            'TotL' - Unlensed source luminosity. Default is 1.
    %            'Nstep' - Integration steps. Recomended to use step that
    %                   will bring you to the lens size.
    %                   If empty, then will choose Nstep to be:
    %                   ceil(AngSrcRad./AngLensRad)
    %                   In the magnification calculation, the infinte
    %                   magnification point will be removed such that the
    %                   magnification takes into account the obstruction by
    %                   the lens.
    %                   Default is [].
    %            'Oversampling' - An oversampling factor for the automatic
    %                   selection of Nstep. Default is 3.
    %            'LimbFun' - Limb darkning function.
    %                   Default is @astro.binary.limb_darkening
    %            'LimbFunPars' - Default is {'constant'}
    % Output : - Total magnification.
    %          - A structure with additional information.
    % Reference: See also Agol 2003
    % Author : Eran Ofek (Sep 2023)
    % Example: K=celestial.Kepler.kepler3law(1.4.*2e33, 'p',3600);
    %          Dls = K.a./constant.pc;
    %          astro.binary.self_microlensing(1, 'Dls',Dls);
    %
    %          PerVec = logspace(log10(600), log10(86400), 100);
    %          K=celestial.Kepler.kepler3law(1.4.*2e33, 'p',PerVec);
    %          Dls = K.a./constant.pc;
    %          for Id=1:1:numel(PerVec);TotMu(Id)=astro.binary.self_microlensing(0, 'Dls',Dls(Id)); end
    %
    %          Beta = (-3:0.1:3);
    %          for Id=1:1:numel(Beta);TotMu(Id)=astro.binary.self_microlensing(Beta(Id), 'Dls',Dls(end)); end
    
    arguments
        D             % in SrcRad units
        Args.Dl        = 1000;
        Args.Dls       = 0.01./206000;
        Args.DistUnits = 'pc';
        
        Args.SrcRad    = 6400;
        Args.LensRad   = 10;
        %Args.LensB     = 1e12;
        Args.SrcRadUnits = 'km';
        
        
        Args.Mass      = 1.4;
        Args.MassUnits = 'SunM';
        Args.TotL      = 1;
        Args.Nstep     = [];
        Args.Oversampling = 3;
        
        Args.LimbFun       = @astro.binary.limb_darkening;
        Args.LimbFunPars   = {'constant'};
    end
    
    SrcRad  = convert.length(Args.SrcRadUnits, Args.DistUnits, Args.SrcRad);   % in DistUnits
    LensRad = convert.length(Args.SrcRadUnits, Args.DistUnits, Args.LensRad);  % in DistUnits
    Ds      = Args.Dl+Args.Dls;
    AngSrcRad    = SrcRad./Ds;   % [rad]
    AngLensRad   = LensRad./Ds;   % [rad]
    
    if isempty(Args.Nstep)
        % auto selection of Nstep
        % such that the step size is like the lens size, so when we remove
        % the infinte magnification point this is equivalent to the
        % obstruction by the lens...
        Args.Nstep = Args.Oversampling.*ceil(AngSrcRad./AngLensRad);
    end
    
    
    Vec = AngSrcRad.*(-1:1./Args.Nstep:1);   % [rad]
    [MatX, MatY] = meshgrid(Vec, Vec);
    
    MatR = sqrt(MatX.^2 + MatY.^2);
    MatL = ones(size(MatR));
    LR   = Args.LimbFun(MatR, Args.LimbFunPars{:});
    
    
    FlagR = MatR>AngSrcRad;
    MatL(FlagR) = 0;
    MatL        = MatL.*LR;
    MatL        = MatL.*Args.TotL./sum(MatL, 'all');
    
    MatRd       = sqrt((AngSrcRad.*D-MatX).^2 + MatY.^2);
    
    
    Res = astro.microlensing.ps_lens('Mass',Args.Mass, 'MassUnits',Args.MassUnits,...
                                     'Dl',Args.Dl, 'Ds',Ds, 'DistUnits',Args.DistUnits,...
                                     'Beta',MatRd, 'BetaUnits','rad','OutUnits','rad');
                                 
    
    
    Res.AngSrcRad  = AngSrcRad;
    Res.AngLensRad = AngLensRad;
    % The Agol (2003) magnification in the limit of RE<<R*:
    Res.AgolMagnification = (pi.*Res.AngSrcRad.^2+2.*pi.*Res.ER.^2)./(pi.*Res.AngSrcRad.^2);
    
    
    % remove all magnifications which are within the lens-radius.
    % I.e., the lens is obscuring the source:
    U = Res.AngLensRad./Res.ER;
    % magnification at the ER:
    MagAtER = (U.^2+2)./(U.*sqrt(U.^2+4));
    FlagInf = isinf(Res.MuTot);
    FlagInf = Res.MuTot>MagAtER;
    Res.MuTot(FlagInf) = 0;  %max(Res.MuTot(~FlagInf),[],'all');
    
        
    TotMu = sum(MatL .* Res.MuTot, 'all');
    
    % Magnetic field is not relevant because the light was already emitted
    %B = Args.LensB .* (MatRd./AngLensRad).^-3;  % [Gauss]
    %FlagInf = isinf(B);
    %B(FlagInf)         = Args.LensB;
    %TotB = sum(MatL.*B, 'all')
    
    
end
