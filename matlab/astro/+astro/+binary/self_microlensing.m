function [TotMu,Res]=self_microlensing(ImpactPar, Args)
    % Calculate the self microlensing for binary stars
    % Input  : - A vector of the impact parameters at which to calculate
    %            the total magnification, in units of the SrcRad.
    %          * ...,key,val,...
    %            'ImpactParUnits' - Units of the ImpactPar argument:
    %                   'SrcRad' - Src radius (default).
    %                   'SrcRadUnits' - The sams as SrcRadUnits.
    %            'Dl' - Dist. to lens. Default is 1000.
    %            'Dls' - Dist from lens to source. Default is 0.01./206000
    %            'DistUnits' - Dist. units. Default is 'pc'.
    %            'SrcRad' - Source radius. Default is 6400.
    %            'LensRad' - Lens radius. Default is 10.
    %            'SrcRadUnits' - Source/Lens radius units. Default is 'km'.
    %            'Mass' - Lens mass. Default is 1.4
    %            'MassUnits' - Lens mass units. Default is 'SunM'.
    %            'Algo'   - Algorithm. Default is '1dfast' (1d integral).
    %            'IntStep' - Integration step in Einstein Radius units.
    %                   Default is 1e-5 (usually good to accuracy of 1e-4
    %                   in magnification).
    %
    %            'TotL' - Unlensed source luminosity. Default is 1.
    %            'Nstep' - Integration rsteps for the '2d' algorithm.
    %                   Recomended to use step that
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
    %            
    %            'PrepMovie' - Default is false.
    %            'MovieName' - Default is 'try.avi'.
    %            'LC'        - Add LC to movie. Default is [].
    % Output : - Total magnification.
    %          - A structure with additional information.
    % Reference: See also Agol 2003
    % Author : Eran Ofek (Sep 2023)
    % Example: K=celestial.Kepler.kepler3law(1.4.*2e33, 'p',86400); %3600);
    %          Dls = K.a./constant.pc;
    %          [TM,Res]=astro.binary.self_microlensing(1, 'Dls',Dls);
    %
    %          PerVec = logspace(log10(600), log10(86400), 100);
    %          K=celestial.Kepler.kepler3law(1.4.*2e33, 'p',PerVec);
    %          Dls = K.a./constant.pc;
    %          for Id=1:1:numel(PerVec);TotMu(Id)=astro.binary.self_microlensing(0, 'Dls',Dls(Id)); end
    %
    %          Beta = (-3:0.1:3);
    %          for Id=1:1:numel(Beta);TotMu(Id)=astro.binary.self_microlensing(Beta(Id), 'Dls',Dls(end)); end
    %
    %          [TM,Res]=astro.binary.self_microlensing(1, 'Dls',Dls, 'Algo','2d');
    %
    %          % generate movie
    %          Beta=[0:0.01:3];
    %          [TM,Res]=astro.binary.self_microlensing(Beta, 'Dls',Dls, 'Algo','1dfast', 'LC',Args.LC);
    %          Args.LC=[Beta.', TM(:)];
    %           [TM,Res]=astro.binary.self_microlensing(Beta, 'Dls',Dls, 'Algo','2d', 'LC',Args.LC);  
    
    arguments
        ImpactPar             % in SrcRad units
        Args.ImpactParUnits  = 'SrcRad';  % 'SrcRad','SrcRadUnits'
        Args.Dl        = 1000;
        Args.Dls       = 0.01./206000;
        Args.DistUnits = 'pc';
        
        Args.SrcRad    = 6400;
        Args.LensRad   = 15;
        %Args.LensB     = 1e12;
        Args.SrcRadUnits = 'km';
                
        Args.Mass      = 1.4;
        Args.MassUnits = 'SunM';
        Args.TotL      = 1;
                
        Args.IntStep       = 1e-5;   % in units of ER
        Args.LimbFun       = @astro.binary.limb_darkening;
        Args.LimbFunPars   = {'constant'};
        
        Args.Algo          = '1dfast';
        Args.Nstep         = [];
        Args.Oversampling  = 3;

        % 2d integration parameters
        Args.Nsim          = 1e8;  % total number of simulations
        Args.NsimBlock     = 1e6;  % number of simotanous ismulations
        
        % limb darkening
        Args.LimbDarkCoef = zeros(1,4); %astro.stars.getClaret2020_LimbDarkeningWD(10000,[7]);
        
        Args.UseIndivMag logical  = true;
        
        Args.PrepMovie logical  = false;
        Args.MovieName          = 'try.avi';
        Args.LC                 = [];
    end
    
    
    
    switch Args.ImpactParUnits
        case 'SrcRad'
            % do nothing
        case 'SrcRadUnits'
            ImpactPar = ImpactPar./Args.SrcRad;
        otherwise
            error('Unknown ImpactParUnits option');
    end
    
    SrcRad  = convert.length(Args.SrcRadUnits, Args.DistUnits, Args.SrcRad);   % in DistUnits
    LensRad = convert.length(Args.SrcRadUnits, Args.DistUnits, Args.LensRad);  % in DistUnits
    Ds      = Args.Dl+Args.Dls;
    AngSrcRad    = SrcRad./Ds;   % [rad]
    AngLensRad   = LensRad./Ds;   % [rad]
    
    switch Args.Algo
        case '1dfast'
            % Convert to ER units
            if nargout>1
                Res = astro.microlensing.ps_lens('Mass',Args.Mass, 'MassUnits',Args.MassUnits,...
                                             'Dl',Args.Dl, 'Ds',Ds, 'DistUnits',Args.DistUnits,...
                                             'Beta',0, 'BetaUnits','rad','OutUnits','rad');
            else
                Mass_gr  = convert.mass(Args.MassUnits,'gr',Args.Mass);
                DistConv = convert.length(Args.DistUnits,'cm');
                Dls_cm   = Args.Dls.*DistConv;
                Dl_cm    = Args.Dl.*DistConv;
                Ds_cm    = Dls_cm + Dl_cm;
                
                Res.ER = sqrt(4.*constant.G.*Mass_gr.*Dls_cm./(constant.c.^2 .*Dl_cm.*Ds_cm));
            end
            Rstar = AngSrcRad./Res.ER;  % ER units
            Rlens = AngLensRad./Res.ER; % ER units

            Beta = ImpactPar(:).'.*Rstar; % ER units
            Nbeta = numel(Beta);
            
            CosFun = @(R,u,b) real(acos((u.^2+b.^2 - R.^2)./(2.*u.*b)));
            
            %U = (Rlens:Args.IntStep:(max(Beta)+Rstar+Rlens)).';   % ER units
            % Integration start with U=0 (which corresponds to theta>0)
            % te selection of Theta>LensRad is done later
            U = linspace(Args.IntStep, max(Beta)+Rstar+Rlens, ceil(1./Args.IntStep)).';
            U2 = U.^2;
            if Args.UseIndivMag
                U0     = sqrt(U2 + 4); %.*Res.ER.^2);
                Theta1 = 0.5.*(U + U0);
                Theta2 = 0.5.*(U - U0);
                MagBase = (U2 + 2)./(2.*U.*U0);
                Mag1   = MagBase + 0.5;
                Mag2   = MagBase - 0.5;
                % Flags for images that are occulted by the lens
                FlagT1 = double(abs(Theta1)>Rlens);
                FlagT2 = double(abs(Theta2)>Rlens);
                Mag    = Mag1.*FlagT1 + Mag2.*FlagT2;
            else
                Mag = (U2 + 2)./(U.*sqrt(U2 + 4));
            end
            UMag = 2.*U.*Mag;
            
            TotMu  = zeros(1,Nbeta);
            for Ib=1:1:Nbeta

                %CF = CosFun(Rstar, U, Beta(Ib));
                CF = real(acos((-Rstar.^2 +U.^2+Beta(Ib).^2)./(2.*U.*Beta(Ib))));
                CF(isnan(CF)) = 0;

                TotMu(Ib) = trapz(U, UMag.*CF, 1);  % noramlize to area of src
            end
            TotMu = TotMu./(pi.*Rstar.^2);
            
            Res.AngSrcRad  = AngSrcRad;
            Res.AngLensRad = AngLensRad;
            % The Agol (2003) magnification in the limit of RE<<R*:
            Res.AgolMagnification = (pi.*Res.AngSrcRad.^2+2.*pi.*Res.ER.^2)./(pi.*Res.AngSrcRad.^2);

            
        case '2d'
            % develop
            
            if nargout>1
                Res = astro.microlensing.ps_lens('Mass',Args.Mass, 'MassUnits',Args.MassUnits,...
                                             'Dl',Args.Dl, 'Ds',Ds, 'DistUnits',Args.DistUnits,...
                                             'Beta',0, 'BetaUnits','rad','OutUnits','rad');
            else
                Mass_gr  = convert.mass(Args.MassUnits,'gr',Args.Mass);
                DistConv = convert.length(Args.DistUnits,'cm');
                Dls_cm   = Args.Dls.*DistConv;
                Dl_cm    = Args.Dl.*DistConv;
                Ds_cm    = Dls_cm + Dl_cm;
                
                Res.ER = sqrt(4.*constant.G.*Mass_gr.*Dls_cm./(constant.c.^2 .*Dl_cm.*Ds_cm));
            end
            Rstar = AngSrcRad./Res.ER;
            Rlens = AngLensRad./Res.ER;

            Beta = ImpactPar(:).'.*Rstar;
            Nbeta = numel(Beta);
            
            CosFun = @(R,u,b) real(acos((-R.^2 +u.^2+b.^2)./(2.*u.*b)));
            TotMu  = zeros(1,Nbeta);
                        
            
                            
            Nblock = ceil(Args.Nsim./Args.NsimBlock);
            
            if Args.PrepMovie
                Nblock = 1;
                Vid = VideoWriter(Args.MovieName);
                Vid.FrameRate = 30;
                open(Vid);
                Hf = figure;
                Hax1 = axes('Position',[0.1 0.1 0.3 0.4]);
                plot(Args.LC(:,1), Args.LC(:,2), 'k-')
                hold on;
                
                Hax2 = axes('Position',[0.45 0.1 0.45 0.4]);
                Hax2.XAxis.Visible='off';
                Hax2.YAxis.Visible='off';
            end
            
            for Ib=1:1:Nbeta        
                Mag = zeros(Nblock,1);
                
                for Iblock=1:1:Nblock
                    [X,Y, R] = tools.rand.randInCirc(Rstar, Args.NsimBlock, 1);
                    % apply limb darkening (using R)
                    % ...
                    
                    [Imu] = astro.stars.limbDarkening(Args.LimbDarkCoef, R./Rstar, 'MuUnits','r', 'Fun','4par');
                    
                    U2 = (X - Beta(Ib)).^2 + (Y).^2;
                    U  = sqrt(U2);

                    U0     = sqrt(U2 + 4);
                    Theta1 = 0.5.*(U + U0);
                    Theta2 = 0.5.*(U - U0);
                    MagBase = (U2 + 2)./(2.*U.*U0);
                    Mag1   = MagBase + 0.5;
                    Mag2   = MagBase - 0.5;
                    % Flags for images that are occulted by the lens
                    FlagT1 = double(abs(Theta1)>Rlens);
                    FlagT2 = double(abs(Theta2)>Rlens);
                    % note that FlagT may be shorter than NsimBlock
                    % (because of occultations) and hence the need to
                    % divide by NsimBlock
                    Mag(Iblock)    = sum(Imu.*(Mag1.*FlagT1 + Mag2.*FlagT2))./sum(Imu); %./Args.NsimBlock;
                    if Args.PrepMovie && Iblock==1
                        axes(Hax1)
                        Hp=plot(ImpactPar(Ib), Args.LC(Ib,2),'ko','MarkerFaceColor','k');
                        axes(Hax2)
                        scatter(X,Y,[],log10(Imu.*(Mag1.*FlagT1 + Mag2.*FlagT2)),'filled')
                        Hax2.XAxis.Visible='off';
                        Hax2.YAxis.Visible='off';
                        Hc = colorbar;
                        Hc.Limits = [0, max((Args.LC(:,2)))];
                        Frame = getframe(gcf);
                        
                        writeVideo(Vid,Frame)
                        Hp.reset;
                        
                    end
                end
                TotMu(Ib) = mean(Mag);     
            end
            if Args.PrepMovie
                close(Vid);
            end
    
            
        case '2dgpu'
            
      
        case '1dold'
            % Convert to ER units
            if nargout>1
                Res = astro.microlensing.ps_lens('Mass',Args.Mass, 'MassUnits',Args.MassUnits,...
                                             'Dl',Args.Dl, 'Ds',Ds, 'DistUnits',Args.DistUnits,...
                                             'Beta',0, 'BetaUnits','rad','OutUnits','rad');
            else
                Mass_gr  = convert.mass(Args.MassUnits,'gr',Args.Mass);
                DistConv = convert.length(Args.DistUnits,'cm');
                Dls_cm   = Args.Dls.*DistConv;
                Dl_cm    = Args.Dl.*DistConv;
                Ds_cm    = Dls_cm + Dl_cm;
                
                Res.ER = sqrt(4.*constant.G.*Mass_gr.*Dls_cm./(constant.c.^2 .*Dl_cm.*Ds_cm));
            end
            Rstar = AngSrcRad./Res.ER;
            Rlens = AngLensRad./Res.ER;

            Beta = ImpactPar(:).'.*Rstar;
            Nbeta = numel(Beta);
            
            CosFun = @(R,u,b) real(acos((-R.^2 +u.^2+b.^2)./(2.*u.*b)));
            TotMu  = zeros(1,Nbeta);
            for Ib=1:1:Nbeta
                U = (Rlens:Args.IntStep:(Beta(Ib)+Rstar+Rlens)).';

                CF = CosFun(Rstar, U, Beta(Ib));
                CF(isnan(CF)) = 0;


                if Args.UseIndivMag
                    U0     = sqrt(U.^2 + 4); %.*Res.ER.^2);
                    Theta1 = 0.5.*(U + U0);
                    Theta2 = 0.5.*(U - U0);
                    MagBase = (U.^2 + 2)./(2.*U.*sqrt(U.^2 + 4));
                    Mag1   = MagBase + 0.5;
                    Mag2   = MagBase - 0.5;
                    % Flags for images that are occulted by the lens
                    FlagT1 = double(abs(Theta1)>Rlens);
                    FlagT2 = double(abs(Theta2)>Rlens);
                    Mag    = Mag1.*FlagT1 + Mag2.*FlagT2;
                else
                    Mag = (U.^2 + 2)./(U.*sqrt(U.^2 + 4));
                end

                TotMu(Ib) = trapz(U, 2.*pi.*U.*Mag.*CF./pi, 1)./(pi.*Rstar.^2);  % noramlize to area of src
            end
            
            Res.AngSrcRad  = AngSrcRad;
            Res.AngLensRad = AngLensRad;
            % The Agol (2003) magnification in the limit of RE<<R*:
            Res.AgolMagnification = (pi.*Res.AngSrcRad.^2+2.*pi.*Res.ER.^2)./(pi.*Res.AngSrcRad.^2);
            
        case '2d_old'
            % not good enough
            
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
    
end
