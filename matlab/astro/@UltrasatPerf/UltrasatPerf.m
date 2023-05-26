% Ultrasat performance container class.
% Load/recieve all basic optical design components:
%    - CaF2 AR transimition (one surface)
%    - Number of CaF2 AR-coated surfaces
%    - FS AR transmition (one surface)
%    - Number of FS AR-coated surfaces
%    - Mirror reflection
%    - Sapphire filter trasmition (AR coating already included,AOI dependent)
%    - Detector QE (AOI dependent)
%    - AOI distribution for filter+detector (radial dependent)
%    - Obscuration (radial dependent)
%    - chromPSF (chromatic radial dependet EE50/FWHM)
%
% Calcuate:
%    - Total throughput - (one for each radial position)
%    - ULTRASAT AstroFilters - using the total throughput
%    - Gaia_Bp-Rp
%    - ULTRASAT-GaiaG
%    - ULTRASAT-GalexNUV
%    - effFWHM
%    - Limiting magntiude
%    - Saturation limit
%    - (SNR cell array) - default not
%    - Sensitivity stracture
%
% Possible plots:
%    - Total throughput
%    - ChromPSF
%    - Color-Color relation
%    - EffPSF
%    - Lim mag
%    - SNR
%
% Possible output files (mat/txt):
%    - Performance table: Source names, Gaia_Bp-Rp
%                          + fives of [ULTRASAT-GaiaG, ULTRASAT-GalexNUV, effFWHM, Limiting magnitude, Saturation limit]
%    - Sensitivity staructure - SNR as a function of magnitude for various
%                               sources at various radial poistion
%    - ChromPSF
%
%
% Preperation functions:
%       prep_raw_transmission    % for lenses AR and mirror
%       prep_raw_obscuration
%       read_QE_csv
%       prep_QE


classdef UltrasatPerf < Component
    % Component should contain:
    
    properties (Access = public)
        % Phase-space Parameters 
        wavelength(:,1)     double = 2000:11000 ; % [Ang] Wavelength range for all ULTRASAT calcualtions
        Rmm(1,:)            double = linspace(0,45,25)*sqrt(2); % [mm] Radial distance vector in mm 
        Rdeg(1,:)           double = []; % [deg] Radial distance vector in deg
        AOI_grid(:,1)       double = 0:1:45; % [deg] Detector+Filter AOI grid vector
        Specs(:,1)          AstSpec = []; % Specs

        % Design
        FLmm                double = 360; % [mm] focal length
        obscuration(1,:)    double = [];  % [%] Obscuration as a function of radial distance
        N_CaF2              double = 4;   % Number of CaF2 AR-coated surfaces
        N_FS                double = 4;   % Number of FS AR-coated surfaces
        
        T_CaF2(:,1)         double = [];  % [%]  Transmission of single CaF2 AR-coated surface
        T_FS(:,1)           double = [];  % [%]  Transmission of single FS AR-coated surface
        R_Mirror(:,1)       double = [];  % [%]  Mirror reflection
        
        T_Filter(:,:)       double = [];  % [%]  2D (WL vs. AOI) Transmission of the sapphire filter (including ARC)
        QE(:,:)             double = [];  % [%]  2D (WL vs. AOI) detector QE
        AOI_dist(:,:)       double = [];  % [%]  2D (AOI vs R) AOI distribution for filter+detector

        chromPSF(:,:)       double = [];  % [arcsec] 2D (WL vs. R) chromatic PSF
        
        % SNR param
        Aper                double = 33;     % Aperture [cm]
        FL                  double = 36;     % Focal Length [cm]
        PixSize             double = 9.5;    % Pixel size [micron]
        RN                  double = 3.5;    % Read Noise [e-/pixel]
        StrayLight          double = 3.5.^2; % [e-/pix]
        DC                  double = 0.026;   % [e-/pix/s]
        Gain                double = 2;      % [e-/ADU]
        WC                  double = 160000; % Full well capacity [e-]
        
        SNR_ClearAper       double = 1; % Already taken into account
        SNR_Trans           double = 1; % Already taken into account
        SNR_Reflection      double = 1; % Already taken into account
        SNR_QE              double = 1; % Already taken into account
        
        % Performance 
        TotT(:,:)                double = []; % 2D (WL vs. R) Total throughput
        U_AstFilt(:,1)           AstFilter = []; % Array of AstFilters ( one per R) with Total throughput
        C_Gaia_BpRp(:,1)         double =[]; % Gaia Bp-Rp colors for the Specs 
        C_ULTRASAT_GaiaG(:,:)    double = []; % 2D (Specs vs R) ULTRASAT-GaiaG colors
        C_ULTRASAT_GalexNUV(:,:) double = []; % 2D (Specs vs R) ULTRASAT-GaiaG colors
        EffPSF(:,:)              double = []; % 2D (Specs vs R) effective PSF
        LimMag(:,:)              double = []; % 2D (Specs vs R) limiting mag
        SatMag(:,:)              double = []; % 2D (Specs vs R) saturation mag
        AW_EffPSF(:,1)           double = []; % Area weighted effective PSF in central 170 deg (approximated by r=7.4deg circle;
        AW_LimMag(:,1)           double = []; % Area weighted limiting mag in central 170 deg (approximated by r=7.4deg circle;
        AW_SatMag(:,1)           double = []; % Area weighted saturation mag in central 170 deg (approximated by r=7.4deg circle;
        ZP(:,:)                  double = []; % 2D (Specs vs R) Zero points
        VarPerPix(:,:)           double = []; % 2D (Specs vs R) Variance per Pixels
    end
    
    properties (Hidden, Constant)
        RawDataDir   = '/home/yossishv/Dropbox (Weizmann Institute)/WIS/ULTRASAT/ULTRASAT_Reviews/Science/Observation_planning/SNR/@UltrasatPerf/Rawdata';
    end
        
    methods  % Constructor
        function Obj = UltrasatPerf(Nobj, Args)
            % Basic constructor for UltrasatPerf class.
            % Input  : - A vector of the requested size of the empty
            %            AstroWCS object (e.g., [2 2]).
            % Output : - An AstroWCS object with fields populated with the defaults.
            % Author : Yossi Shvartzvald (February 2022)
            % Example:
            %          UF = UltrasatPerf(1);
            %          UF = UltrasatPerf([2 2]);

            arguments
                Nobj           = 1;   % array size
                Args.PSF_name  = 'chromPSF_1';
                Args.calcPerf  = false;
                Args.Init = true;           % True to initialize, added by @Chen, 21/05/2023 for debugging
            end
            
            Obj.setName('UltrasatPerf');            
            Obj.msgLog(LogLevel.Debug, 'constructor started');
            
            if Args.Init
                
                % create an empty AstroWCS object
                List = cell(Nobj);
                Nh = numel(List);
                for Ih=1:1:Nh
                    Obj(Ih).Rdeg = Obj(Ih).Rmm * convert.angular('rad','deg') / Obj(Ih).FLmm; % Fill Rdeg
                    Obj(Ih).populate_Design('PSF_name',Args.PSF_name); % Populate design
                    if Args.calcPerf
                        Obj(Ih).calculatePerformance;
                    end

                end
                Obj = reshape(Obj,size(List));
            end
            
            Obj.msgLog(LogLevel.Debug, 'constructor done');
        end
        
    end

    methods  % setters/getter
      
    end

    methods  % General functions
       
        function SNR = calcSNR(Obj,Args)
            arguments
                Obj
                Args.SN                   = 5;
                Args.R                    = 1; % Index of radial position
                Args.SrcINd               = 41; % Index of source spec
                
                Args.Mag                  = 22.0;
                Args.CalibFilterFamily    = [];    % filter family of input magnitude by which to scale the mag
                Args.CalibFilter          = [];
                Args.CalibMagSys          = 'AB';

                Args.Name                 = {};     % override all pthe parameters provided in the list
                Args.FWHM                 = [];     % FWHM [arcsec]
                Args.PSFeff               = 0.8;    % PSF photometry efficiency                
                
                Args.ExpTime              = 300;    % [s]
                Args.Nim                  = 3;

                Args.TargetSpec           = []; %2e4; %3e3; %2e4;    % if given override Mag
                Args.BackSpec             = @telescope.sn.back_comp;    % per arcsec^2 | handle | AstSpec | matrix
                Args.BackCompFunPar       = {'CerenkovSupp',21};
                Args.Ebv                  = 0.02;
                Args.Filter               = [];
                Args.FilterFamily         = [];
                Args.MagSys               = 'AB';

                Args.InterpMethod         = 'linear';                
            end
            
            if isempty(Args.CalibFilterFamily)  % Default use with calculated ULTRASAT filters for specific R
                Args.CalibFilterFamily    = Obj.U_AstFilt(Args.R);
                Args.CalibFilter          = [];
            end
            
            if isempty(Args.FWHM)               % Default use with caulcated EffPSF for specific src and R
               Args.FWHM = Obj.EffPSF(Args.SrcINd,Args.R);
            end
            
            if isempty(Args.TargetSpec)         % Default use with selected specific src
               Args.TargetSpec = Obj.Specs(Args.SrcINd);
            end
            
            if isempty(Args.FilterFamily)  % Desfult use with calculated ULTRASAT filters for specific R
                Args.FilterFamily    = Obj.U_AstFilt(Args.R);
                Args.Filter          = [];
            end
            
            SNR=telescope.sn.snr('SN',Args.SN,...
                                 'Mag',Args.Mag,...%SNR(Imag,1),...                                 
                                 'CalibFilterFamily',Args.CalibFilterFamily,...
                                 'CalibFilter',Args.CalibFilter,...      
                                 'CalibMagSys',Args.CalibMagSys,...
                                 'Name',Args.Name,...                                 
                                 'FWHM',Args.FWHM,...
                                 'PSFeff',Args.PSFeff,...
                                 'Aper',Obj.Aper,...
                                 'FL',Obj.FL,...
                                 'PixSize',Obj.PixSize ,...
                                 'RN',Obj.RN,...                                 
                                 'StrayLight',Obj.StrayLight,...                                 
                                 'DC',Obj.DC,...                                 
                                 'Gain',Obj.Gain,...                                 
                                 'WC',Obj.WC,...                                 
                                 'ExpTime',Args.ExpTime,...                                 
                                 'Nim',Args.Nim,... 
                                 'ClearAper',Obj.SNR_ClearAper,...
                                 'Trans',Obj.SNR_Trans,...
                                 'Reflection',Obj.SNR_Reflection,...
                                 'QE',Obj.SNR_QE,...
                                 'TargetSpec',Args.TargetSpec,...
                                 'BackSpec',Args.BackSpec,... 
                                 'BackCompFunPar',Args.BackCompFunPar,...                                 
                                 'Ebv',Args.Ebv,... 
                                 'Filter',Args.Filter,...
                                 'FilterFamily',Args.FilterFamily,...
                                 'MagSys',Args.MagSys,...
                                 'Wave',Obj.wavelength,...
                                 'InterpMethod',Args.InterpMethod);
                       
            
        end
        
        function h = plot(Obj,plotname,Args)
            arguments
                Obj
                plotname  = 'TR';
                Args.savePlot = false;
                Args.filetype = 'fig';
                Args.filename = 'ULTRASAT_plot';
                Args.folder = '.';
                Args.FontSize = 16;
                Args.AxisFontSize = 16;
                Args.LegendFontSize = 12;
                Args.SrcInd = [41,17,27];
                Args.SrcLegend = {'Source: Blackbody T=20,000 [K]','Source: $G$ dwarf','Source: $M$ dwarf'};
                Args.SrcColor = {'b','g','r'};
            end
            
            h = figure('WindowStyle','docked','Color',[1 1 1]); box on; hold on;
            set(gca,'FontSize',Args.AxisFontSize);
            
            switch plotname
                case 'TR'
                    plot(Obj.wavelength/10,Obj.TotT(:,1),'Color',[0 0 0],'Linewidth',2);
                    for R = 2:25
                        plot(Obj.wavelength/10,Obj.TotT(:,R),'Color',[1 1 1 ].*(0.5+R/80),'Linewidth',0.5);                 
                    end
                    plot(Obj.wavelength/10,Obj.TotT(:,1),'Color',[0 0 0],'Linewidth',2);
                    
                    xlabel('Wavelength [nm]','interpreter','latex','FontSize',Args.FontSize);
                    ylabel('Total throughput','interpreter','latex','FontSize',Args.FontSize);                    
                    xlim([200 1100]);
                    ylim([5e-7,1]);
                    set(gca,'Yscale','log');
                    grid on;
                    hold off;

                    h = figure('WindowStyle','docked','Color',[1 1 1]); box on; hold on;
                    set(gca,'FontSize',Args.AxisFontSize);
                    plot(Obj.wavelength/10,Obj.TotT(:,1),'Color',[0 0 0],'Linewidth',2);
                    for R = 2:25
                        plot(Obj.wavelength/10,Obj.TotT(:,R),'Color',[1 1 1 ].*(0.5+R/80),'Linewidth',0.5);                 
                    end
                    plot(Obj.wavelength/10,Obj.TotT(:,1),'Color',[0 0 0],'Linewidth',2);

                    xlim([220 300]);
                    ylim([0,0.4]);
                    grid on;
                    legend('FoV center','Radial distances: 0-10 deg','Location','best','interpreter','latex','FontSize',Args.LegendFontSize)
                    
                case 'ColorColor'
                    F_BB = contains({Obj.Specs.ObjName},{'Planck'});

                    plot(Obj.C_Gaia_BpRp(~F_BB),Obj.C_ULTRASAT_GaiaG(~F_BB),'ob','MarkerSize',6,'MarkerFaceColor','b');
                    plot(Obj.C_Gaia_BpRp(F_BB),Obj.C_ULTRASAT_GaiaG(F_BB),'or','MarkerSize',6,'MarkerFaceColor','r');
                    grid on
                    xlabel('$Gaia$ BP-RP [AB mag]','interpreter','latex','FontSize',Args.AxisFontSize);
                    ylabel('ULTRASAT - $Gaia$ G [AB mag]','interpreter','latex','FontSize',Args.AxisFontSize);
                    legend('Dwarfs','Blackbody sources','Location','best')
                    xlim([-1,4]);
                    ylim([-2,9]);
                 
                case 'EffPSF'
                    for i = 1:numel(Args.SrcInd)
                        plot(Obj.Rdeg,Obj.EffPSF(Args.SrcInd(i),:),'Color',Args.SrcColor{i},'Linewidth',2);
                    end
                    legend(Args.SrcLegend,'Location','best','FontSize',Args.LegendFontSize);
                    xlabel('Radial distance [deg]','interpreter','latex');
                    xlim([0,10]);                  
                    set(gca,'Yscale','log');
                    ylim([1,200]); 
                    grid on
                    ylabel('Effective PSF FWHM (arcsec)','interpreter','latex','FontSize',Args.AxisFontSize);

                case 'LimMag'
                    for i = 1:numel(Args.SrcInd)
                        plot(Obj.Rdeg,Obj.LimMag(Args.SrcInd(i),:),'Color',Args.SrcColor{i},'Linewidth',2);
                    end
                    legend(Args.SrcLegend,'Location','best','FontSize',Args.LegendFontSize);
                    xlabel('Radial distance [deg]','interpreter','latex');
                    xlim([0,10]);                  
                    set(gca,'Yscale','log');
                    ylim([19.3,23.1]); 
                    grid on; grid minor; grid minor;
                    ylabel('Limiting Magnitude [AB mag]','interpreter','latex','FontSize',Args.AxisFontSize);
                    
                case 'SNR' %
                    SNRvMAG_G_R1  = Obj.calcSNRvMAG(1,17);
                    SNRvMAG_G_R13 = Obj.calcSNRvMAG(13,17);
                    SNRvMAG_M_R1  = Obj.calcSNRvMAG(1,27);
                    SNRvMAG_M_R13 = Obj.calcSNRvMAG(13,27);
                    
                    plot(SNRvMAG_G_R1(:,1),SNRvMAG_G_R1(:,2),'--g','Linewidth',2);
                    plot(SNRvMAG_G_R13(:,1),SNRvMAG_G_R13(:,2),'g','Linewidth',2);
                    plot(SNRvMAG_M_R1(:,1),SNRvMAG_M_R1(:,2),'--r','Linewidth',2);
                    plot(SNRvMAG_M_R13(:,1),SNRvMAG_M_R13(:,2),'r','Linewidth',2);
                    
                    plot(SNRvMAG_G_R1(1,1),SNRvMAG_G_R1(1,2),'og','MarkerSize',8,'MarkerFaceColor','g');
                    plot(SNRvMAG_G_R13(1,1),SNRvMAG_G_R13(1,2),'og','MarkerSize',8,'MarkerFaceColor','g');
                    plot(SNRvMAG_M_R1(1,1),SNRvMAG_M_R1(1,2),'or','MarkerSize',8,'MarkerFaceColor','r');
                    plot(SNRvMAG_M_R13(1,1),SNRvMAG_M_R13(1,2),'or','MarkerSize',8,'MarkerFaceColor','r');
                    
                    legend({'$G$ dwarf at $R=0^\circ$','$G$ dwarf at $R=5^\circ$','$M$ dwarf at $R=0^\circ$','$M$ dwarf at $R=5^\circ$'},'interpreter','latex','Location','best','FontSize',Args.LegendFontSize);
                    xlabel('Magnitude [AB mag]','interpreter','latex','FontSize',Args.AxisFontSize);
                    xlim([9,23]);                  
                    set(gca,'Yscale','log');
                    ylim([5,1e4]);
                    grid on;
                    ylabel('SNR','interpreter','latex','FontSize',Args.AxisFontSize);
                    
            end
            
            if Args.savePlot
                saveas(gcf,[Args.folder '/' Args.filename],Args.filetype);
            end
            hold off;
        end
        
        function SNRvMAG = calcSNRvMAG(Obj,R,SrcINd,Args)
            arguments
                Obj
                R
                SrcINd
                Args.mags  =  [];
            end
            
            if isempty(Args.mags)
                Mags = linspace(Obj.SatMag(SrcINd,R),Obj.LimMag(SrcINd,R),20);
            else
                Mags = Args.mags;
            end
            
            SNRvMAG(:,1) = Mags;
            Nmag = size(SNRvMAG,1);
            
            for Imag = 1:Nmag
                curr_SN=Obj.calcSNR('Mag',SNRvMAG(Imag,1),'R',R,'SrcINd',SrcINd);  
                SNRvMAG(Imag,2) = curr_SN.SNRm;
            end
            
        end
        
        function Obj = calculatePerformance(Obj,Args)
            arguments
                Obj
                Args.AW_radius   = 7.4; % degrees for 170 deg central region
                Args.AW_r_step   = 0.1; % steps
            end
            
            Obj.Specs = Obj.create_Specs('wavelength',Obj.wavelength);
            Obj.C_Gaia_BpRp = Obj.calcColor(Obj.Specs,'GAIA','BP','GAIA','RP');
            Obj.C_ULTRASAT_GaiaG = Obj.calcColor(Obj.Specs,Obj.U_AstFilt,[],'GAIA','G');
            Obj.C_ULTRASAT_GalexNUV = Obj.calcColor(Obj.Specs,Obj.U_AstFilt,[],'GALEX','NUV');
            
            Nsrc = numel(Obj.Specs);
            Nr = numel(Obj.Rdeg);
            Obj.EffPSF = zeros(Nsrc,Nr);
            Obj.LimMag = zeros(Nsrc,Nr);
            Obj.SatMag = zeros(Nsrc,Nr);
            
            r = 0:Args.AW_r_step:Args.AW_radius;
            Obj.AW_EffPSF = zeros(Nsrc,1);
            Obj.AW_LimMag = zeros(Nsrc,1);
            Obj.AW_SatMag = zeros(Nsrc,1);           
            
            for Sidx = 1:Nsrc
                for R = 1:Nr
                    Obj.EffPSF(Sidx,R) = Obj.calc_eff_PSF(Obj.chromPSF(:,R),Obj.Specs(Sidx),Obj.U_AstFilt(R),Obj.wavelength);
                    curr_SN=Obj.calcSNR('R',R,'SrcINd',Sidx);
                    Obj.LimMag(Sidx,R) = curr_SN.LimMag;
                    Obj.SatMag(Sidx,R) = curr_SN.SatLimit;
                    Obj.ZP(Sidx,R) = curr_SN.ZP;
                    Obj.VarPerPix(Sidx,R) = curr_SN.TotalVar / curr_SN.PsfEffAreaPix;
                    % add in future - save SN stracture
                end
                sampled_effPSF = interp1(Obj.Rdeg,Obj.EffPSF(Sidx,:),r);
                Obj.AW_EffPSF(Sidx) = trapz(r,sampled_effPSF.*r)/trapz(r,r);
                
                sampled_LimMag = interp1(Obj.Rdeg,Obj.LimMag(Sidx,:),r);
                Obj.AW_LimMag(Sidx) = trapz(r,sampled_LimMag.*r)/trapz(r,r);

                sampled_SatMag = interp1(Obj.Rdeg,Obj.SatMag(Sidx,:),r);
                Obj.AW_SatMag(Sidx) = trapz(r,sampled_SatMag.*r)/trapz(r,r);
            end
            
        end
        
        function Obj = populate_Design(Obj,Args)
            
            arguments
                Obj
                Args.OBSC_name   = 'Obscuration';                
                Args.CaF2_name   = 'CaF2_AR';
                Args.FS_name     = 'FS_AR';
                Args.Mirror_name = 'M2';%'Mirror';
                Args.Filter_name = 'Filter';
                Args.QE_name     = 's3_T2_211';%T2_211';
                Args.QE_subDir   = 'QE_scouts_experimental';%'QE_scouts_AOI_weighted';
                Args.PSF_name    = 'chromPSF_1';
                Args.EE50_subDir = 'EE50';                
                Args.AOI_fname   = 'aoi.txt';
                Args.interp_mthd = 'linear';%'cubic'; % cubic generate negative tranimission....
            end

            % Obscuration
            io.files.load1(fullfile(UltrasatPerf.RawDataDir,Args.OBSC_name));
            OBSC = eval(Args.OBSC_name);
            Obj.obscuration = interp1(OBSC.Rdeg,OBSC.obscuration,Obj.Rdeg,Args.interp_mthd);
            
            % CaF2
            io.files.load1(fullfile(UltrasatPerf.RawDataDir,Args.CaF2_name));
            CaF2_AR = eval(Args.CaF2_name);
            Obj.T_CaF2 = interp1(CaF2_AR.wavelength,CaF2_AR.transmission,Obj.wavelength,Args.interp_mthd);
            
            % FS
            io.files.load1(fullfile(UltrasatPerf.RawDataDir,Args.FS_name));
            FS_AR = eval(Args.FS_name);
            Obj.T_FS = interp1(FS_AR.wavelength,FS_AR.transmission,Obj.wavelength,Args.interp_mthd);
            
            % Mirror
            io.files.load1(fullfile(UltrasatPerf.RawDataDir,Args.Mirror_name));
            Mirror = eval(Args.Mirror_name);
            Obj.R_Mirror = interp1(Mirror.wavelength,Mirror.transmission,Obj.wavelength,Args.interp_mthd);
            
            % Filter
            io.files.load1(fullfile(UltrasatPerf.RawDataDir,Args.Filter_name));
            Filter = eval(Args.Filter_name);
            Obj.T_Filter = interp2(Filter.angles,Filter.wavelength,Filter.transmission,Obj.AOI_grid',Obj.wavelength,Args.interp_mthd,-1);
            Obj.T_Filter(Obj.T_Filter<0) = nan;
            
            
            % old Filter
            %switch size(Filter_table,2)
            %    case 2
            %        Filter = interp1(Filter_table.wavelength,Filter_table.transmission,Obj.wavelength,Args.interp_mthd);
            %        Obj.T_Filter = repmat(Filter,1,numel(Obj.Rdeg));
            %    case 26
            %        Filter =  table2array(Filter_table(:,2:end));
            %        Obj.T_Filter = interp2(Obj.Rdeg,Filter_table.wavelength,Filter,Obj.Rdeg,Obj.wavelength,Args.interp_mthd); % assuming same R
            %    otherwise
            %        error('Filter data is bad!');
            %end
            
            % QE
            Obj.populate_QE;
            
            % OLD - QE AOIw
            %io.files.load1(fullfile(UltrasatPerf.RawDataDir,Args.QE_subDir,Args.QE_name));
            %QE_table = eval(Args.QE_name);
            %QE_mat = table2array(QE_table(:,2:end));
            %Obj.QE = interp2(Obj.Rdeg,QE_table.wavelength,QE_mat,Obj.Rdeg,Obj.wavelength,Args.interp_mthd);  % assuming same R   
            
            
            % OLD - chromPSF
            %io.files.load1(fullfile(UltrasatPerf.RawDataDir,Args.PSF_name));
            %PSF_table = eval(Args.PSF_name);
            %PSF = table2array(PSF_table(:,2:end));
            %Obj.chromPSF = interp2(Obj.Rdeg,PSF_table.wavelength,PSF,Obj.Rdeg,Obj.wavelength,Args.interp_mthd);  % assuming same R 
            
            % chromPSF
            io.files.load1(fullfile(UltrasatPerf.RawDataDir,Args.EE50_subDir,Args.PSF_name));
            cPSF = eval(Args.PSF_name);
            if max(abs(cPSF.Rdeg-Obj.Rdeg))>1e-5
                error('PSF Rdeg do not match');
            end
            Obj.chromPSF = interp2(Obj.Rdeg,cPSF.wavelength,cPSF.PSF,Obj.Rdeg,Obj.wavelength,Args.interp_mthd);  % assuming same R             
            
            % AOI
            Obj.read_AOI_dist(fullfile(UltrasatPerf.RawDataDir,Args.AOI_fname));
            
            % totT
            Obj.TotT = zeros(numel(Obj.wavelength),numel(Obj.Rdeg));
            for R = 1:numel(Obj.Rdeg)
                Obj.TotT(:,R) = Obj.calc_TotT_R(R);
            end    
            
            Obj.U_AstFilt = Obj.populate_U_AstFilter;
           
        end
        

        function U_AstFilter = populate_U_AstFilter(Obj,Args)
            arguments
                Obj
                Args.Family = 'ULTRASAT';
                Args.BaseBandName = 'R';
                Args.BaseComment = 'deg off FOV center';
                Args.source_info = 'Measured QE + Measured Mirror + Measured CaF2 (4 surfaces) + Theoretical FS (4 surfaces) + Measured filter + Therotical Obscuration'      
            end
            
            Nr = numel(Obj.Rdeg);
            
            % First filter
            R =1;
            newF = AstFilter;
            newF.family = Args.Family;
            newF.band = [Args.BaseBandName num2str(R)];
            newF.comments = sprintf('%.2f %s',Obj.Rdeg(R),Args.BaseComment);
            newF.source = Args.source_info;
            newF.UserData = Obj.Rdeg(R);
            
            fnan = ~isnan(Obj.TotT(:,R));
            newF.T(:,1) = Obj.wavelength(fnan);
            newF.T(:,2) = Obj.TotT(fnan,R);
            
            U_AstFilter = newF.add_filter([]);
            
            for R = 2:Nr
                newF = AstFilter;
                newF.family = Args.Family;
                newF.band = [Args.BaseBandName num2str(R)];
                newF.comments = sprintf('%.2f %s',Obj.Rdeg(R),Args.BaseComment);
                newF.source = Args.source_info;
                newF.UserData = Obj.Rdeg(R);
                
                fnan = ~isnan(Obj.TotT(:,R));
                newF.T(:,1) = Obj.wavelength(fnan);
                newF.T(:,2) = Obj.TotT(fnan,R);
                
                U_AstFilter = newF.add_filter(U_AstFilter);
            end
        end
        
        function totT = calc_TotT_R(Obj,R)
            
            AOI = Obj.AOI_dist(:,R);
            AOI_m = repmat(AOI',numel(Obj.wavelength),1);
            T_filter_QE = sum(Obj.T_Filter.*Obj.QE.*AOI_m,2);
            
            totT = (Obj.T_CaF2.^Obj.N_CaF2) .* (Obj.T_FS.^Obj.N_FS) .* Obj.R_Mirror .* T_filter_QE .* (1-Obj.obscuration(R));
        end
        
        function Obj = populate_QE(Obj,Args)
            
            
            arguments
                Obj        
                Args.QE_name     = 's3_T2_211';
                Args.QE_subDir   = 'QE_scouts_experimental';
                Args.interp_mthd = 'cubic';
                Args.Fname_below_220 = 'QE_below_220nm';
            end
            
            % load below 2200
            F1 = Obj.wavelength<2200; 
           
            Below_bool = false;
            if ~isempty(Args.Fname_below_220)
                io.files.load1(fullfile(UltrasatPerf.RawDataDir,Args.QE_subDir,Args.Fname_below_220));
                if contains(Args.QE_name,'Tstd')
                    QE_below_IB = interp1(QE_below_220nm.wavelength,QE_below_220nm.Tstd,Obj.wavelength(F1));
                elseif  contains(Args.QE_name,'T1')
                    QE_below_IB = interp1(QE_below_220nm.wavelength,QE_below_220nm.T1,Obj.wavelength(F1));
                elseif contains(Args.QE_name,'T2')
                    QE_below_IB = interp1(QE_below_220nm.wavelength,QE_below_220nm.T2,Obj.wavelength(F1));
                else
                    error('unkown ARC type');
                end
                Below_bool = true;
            end            
            
            % load and prep each angle
            mQE_list = dir([fullfile(UltrasatPerf.RawDataDir,Args.QE_subDir,Args.QE_name) '*']);         
            N_mQE = numel(mQE_list);
            mQE_angles = zeros(N_mQE,1);
            
            Nw = numel(Obj.wavelength);
            interp_wl_mQE = zeros(Nw,N_mQE);
            
            for i = 1:N_mQE
                currQEname = regexprep(mQE_list(i).name,'.mat','');
                curr_angle = regexprep(currQEname,[Args.QE_name '_'],'');
                mQE_angles(i) = str2double(curr_angle);
                
                io.files.load1([mQE_list(i).folder '/' mQE_list(i).name]);
                currQE = eval(currQEname);
                
                interp_wl_mQE(:,i) = interp1(currQE.wavelength,currQE.R_qe,Obj.wavelength,Args.interp_mthd);
                
                if Below_bool
                   interp_wl_mQE(F1,i) =  QE_below_IB;
                end
            end
            
            Obj.QE = zeros(Nw,numel(Obj.AOI_grid));
            
            for wl = 1:Nw
                Obj.QE(wl,:) = interp1(mQE_angles,interp_wl_mQE(wl,:),Obj.AOI_grid,Args.interp_mthd);
            end
                   
        end
        
        function Obj = read_AOI_dist(Obj,AOI_file_fullpath) 
            
            aoi_MC = io.files.load1(AOI_file_fullpath);
            edges = [Obj.AOI_grid-(Obj.AOI_grid(2)-Obj.AOI_grid(1))/2 ;Obj.AOI_grid(end)+(Obj.AOI_grid(2)-Obj.AOI_grid(1))/2];
            Obj.AOI_dist = zeros(numel(Obj.AOI_grid),size(aoi_MC,2));
            for i = 1:size(aoi_MC,2)
                Obj.AOI_dist(:,i) = histcounts(aoi_MC(:,i),edges);
            end
            Obj.AOI_dist = Obj.AOI_dist./size(aoi_MC,1);           
                      
        end
            
    end
    
    methods (Static)  % static methods
        function [effPSF,rPSF,PSF_all] = calc_eff_PSF(FWHM,ASpec,ASFilter,wavelength,Args)
            arguments
                FWHM
                ASpec
                ASFilter
                wavelength
                Args.dr = 0.2;
            end
            
            rPSF(:,1) = (0:Args.dr:100);


            ASpec = interp(ASpec,wavelength);
            ASFilter = interp(ASFilter,wavelength);
            
            TargetSpecPh = convert.flux(ASpec.Int,'cgs/A','ph/A',wavelength,'A'); % change Int to photons
            Photon_flux = TargetSpecPh.*ASFilter.T(:,2);
            
            Sig_PSF = FWHM/2.355;
            
            PSF_all = zeros(numel(wavelength),size(rPSF,1))';
            for i = 1:length(wavelength)
                PSF_all(:,i) = normpdf(rPSF(:,1),0,Sig_PSF(i))*Photon_flux(i);
            end           
            
            fnan = isnan(ASFilter.T(:,2)); % avoid nans
            rPSF(:,2) = sum(PSF_all(:,~fnan),2);
            %norm_fac = 2*pi*sum(r.*rPSF(:,2)*dr);
            norm_fac = rPSF(1,2);
            rPSF(:,2) = rPSF(:,2)/norm_fac;

            effPSF = interp1(rPSF(:,2),rPSF(:,1),0.5).*2;           

        end        
        
        
        function Color = calcColor(Specs,F1_family,F1_band,F2_family,F2_band,Args)
            arguments
                Specs
                F1_family
                F1_band
                F2_family
                F2_band
                Args.MagSys = 'AB';
            end
            
            Nsrc = numel(Specs);
            if AstFilter.isAstFilter(F1_family)
                NF1 = numel(F1_family);
            else
                NF1 = 1; % assume a single filter family name
            end
            if AstFilter.isAstFilter(F2_family)
                NF2 = numel(F2_family);
            else
                NF2 = 1; % assume a single filter family name
            end            
            
            if NF1>1 && NF2>1
                error('only one filter can be an array');
            end
            
            NC = NF1*NF2;
            Color = zeros(Nsrc,NC);
            
            for Sidx = 1:Nsrc
                for C = 1:NC
                    if NF1>1
                       [MagF1,~] = astro.spec.synthetic_phot([Specs(Sidx).Wave Specs(Sidx).Int],F1_family(C),[],Args.MagSys);
                       [MagF2,~] = astro.spec.synthetic_phot([Specs(Sidx).Wave Specs(Sidx).Int],F2_family,F2_band,Args.MagSys);
                    elseif NF2>1
                       [MagF1,~] = astro.spec.synthetic_phot([Specs(Sidx).Wave Specs(Sidx).Int],F1_family,F1_band,Args.MagSys);
                       [MagF2,~] = astro.spec.synthetic_phot([Specs(Sidx).Wave Specs(Sidx).Int],F2_family(C),[],Args.MagSys);
                    else
                       [MagF1,~] = astro.spec.synthetic_phot([Specs(Sidx).Wave Specs(Sidx).Int],F1_family,F1_band,Args.MagSys);
                       [MagF2,~] = astro.spec.synthetic_phot([Specs(Sidx).Wave Specs(Sidx).Int],F2_family,F2_band,Args.MagSys);
                    end
                    Color(Sidx,C) = MagF1-MagF2;
                end
            end
            
            
        end        
        
        function Specs = create_Specs(Args)
            arguments
                Args.wavelength(:,1) = 2000:11000 ;
                Args.MStype          = 'V';
                Args.T_BB             = [2e3 ,4e3 ,6e3 ,8e3 ,1e4 ,2e4 ,3e4 ,4e4, 5e4, 6e4, 7e4];
            end
            
            Specs = AstSpec.get_pickles([],Args.MStype );
            
            for T = Args.T_BB
                Specs(end+1) = AstSpec.blackbody(T,Args.wavelength);
            end
        end
        
        
        function TR = prep_raw_transmission(RawFname,WL_col,T_cols,Args)
% CaF2_AR = UltrasatPerf.prep_raw_transmission('CaF2_AR_Full.txt',1,5,'T_scale',1/100,'fill_maxWL',11000,'fill_maxTR',0.955,'SaveBool',true,'MatFname','CaF2_AR');
% FS_AR   = UltrasatPerf.prep_raw_transmission('FS_AR.txt',1,3,'fill_minWL',2000,'fill_minTR',0.87,'fill_maxWL',11000,'fill_maxTR',0.89,'SaveBool',true,'MatFname','FS_AR');
% Mirror  = UltrasatPerf.prep_raw_transmission('Mirror.txt',1,3,'SaveBool',true,'MatFname','Mirror');
% M1      = UltrasatPerf.prep_raw_transmission('mirror 200-1100.xlsx',1,2,'T_scale',1/100,'SaveBool',true,'MatFname','M1');
% M2      = UltrasatPerf.prep_raw_transmission('mirror 200-1100.xlsx',1,3,'T_scale',1/100,'SaveBool',true,'MatFname','M2');
% M3      = UltrasatPerf.prep_raw_transmission('mirror 200-1100.xlsx',1,4,'T_scale',1/100,'SaveBool',true,'MatFname','M3');
% M4      = UltrasatPerf.prep_raw_transmission('mirror 200-1100.xlsx',1,5,'T_scale',1/100,'SaveBool',true,'MatFname','M4');
            arguments
                RawFname
                WL_col
                T_cols
                Args.WL_scale  = 10; % nm -> A (default case)
                Args.T_scale   = 1;  % Potentially be 1/100
                Args.MatFname  = 'TR';
                Args.SaveBool  = false;
                Args.fill_minWL = inf; % 2000 FS,
                Args.fill_minTR = 0.87; % 0.87 FS
                Args.fill_maxWL = -inf;% 11000 FS, 11000 Caf2
                Args.fill_maxTR = 0.89; %0.89 FS, 0.955 CaF2
            end
            
            raw = importdata(fullfile(UltrasatPerf.RawDataDir,RawFname));
            raw = raw.data;
            
            TR= table();
            TR.wavelength  = raw(:,WL_col)*Args.WL_scale;
            
            transmission = raw(:,T_cols)*Args.T_scale;
            transmission(transmission>1) = 1; % avoid >100%
            transmission(transmission<0) = nan; % avoid <0%
            
            TR.transmission = transmission;
            TR = sortrows(TR,1);
            
            Fnonzero = TR.transmission>0;
            TR = TR(Fnonzero,:); % avoid <0%     
            
            if TR.wavelength(1)>Args.fill_minWL
                fillT = table();
                fillT.wavelength = Args.fill_minWL;
                fillT.transmission = Args.fill_minTR;
                TR = [fillT ; TR];
            end

            if TR.wavelength(end)<Args.fill_maxWL
                fillT = table();
                fillT.wavelength = Args.fill_maxWL;
                fillT.transmission = Args.fill_maxTR;
                TR = [TR; fillT];
            end            
            
            if Args.SaveBool
                eval(sprintf('%s = TR;',Args.MatFname));
                save(fullfile(UltrasatPerf.RawDataDir,[Args.MatFname,'.mat']),Args.MatFname);
            end
            
        end
        
        function Filter = prep_raw_filter(RawFname,WL_col,T_cols,Args)
% Filter = UltrasatPerf.prep_raw_filter('Weizmann_SWP285_Thick S1_S2.xlsx',1,28:40,'SaveBool',true,'MatFname','Filter');

            arguments
                RawFname
                WL_col
                T_cols
                Args.WL_scale  = 10; % nm -> A (default case)
                Args.T_scale   = 1/100;  % Potentially be 1/100
                Args.MatFname  = 'TR';
                Args.SaveBool  = false;
                Args.TRsheet   = 'ThickS1_S2Scans'; % if several sheets in excel file
                Args.Name_break_str  = ' ';              
            end
            
            raw = importdata(fullfile(UltrasatPerf.RawDataDir,RawFname));
            if isempty(Args.TRsheet)
                rawdata = raw.data;
                rawcolnames = raw.textdata;
            else
                rawdata = eval(sprintf('raw.data.%s',Args.TRsheet));
                rawcolnames = eval(sprintf('raw.textdata.%s',Args.TRsheet));
            end

            rawdata = sortrows(rawdata,1);
                        
            Filter.wavelength  = rawdata(:,WL_col)*Args.WL_scale;
            
            Filter.transmission = rawdata(:,T_cols)*Args.T_scale;
            Filter.transmission(Filter.transmission>1) = 1; % avoid >100%
            
           
            Nang = numel(T_cols);
            Filter.angles = zeros(1,Nang);
            for Iang = 1:Nang
                currName = rawcolnames{T_cols(Iang)};
                Name_break_ind = strfind(currName,Args.Name_break_str);
                currAN = currName((Name_break_ind(1)+1):(Name_break_ind(2)-2));
                Filter.angles(Iang) = str2double(currAN);
                
                %interpolutare over negative values
                
                % take care of edges
                if (Filter.transmission(1,Iang)<0)
                    Filter.transmission(1,Iang) = Filter.transmission(find(Filter.transmission(:,Iang)>0,1),Iang);
                end
                    
                Fminus = Filter.transmission(:,Iang)<0;
            
                Filter.transmission(Fminus,Iang) = interp1(Filter.wavelength(~Fminus),Filter.transmission(~Fminus,Iang),Filter.wavelength(Fminus),'linear'); % avoid <0%
            end
                        
            if Args.SaveBool
                eval(sprintf('%s = Filter;',Args.MatFname));
                save(fullfile(UltrasatPerf.RawDataDir,[Args.MatFname,'.mat']),Args.MatFname);
            end
            
        end
        
        function OBSC = prep_raw_obscuration(RawFname,R_col,OBSC_col,Args)
%     OBSC = UltrasatPerf.prep_raw_obscuration('CDR_Obscuration_1D.txt',1,2,'SaveBool',true,'MatFname','Obscuration');

            arguments
                RawFname
                R_col
                OBSC_col
                Args.R_scale      = 1; % If Rmm is given, should use (180/pi) / Obj(Ih).FLmm;
                Args.OBSC_scale   = 1; % Potentially be 1/100
                Args.MatFname = 'OBSC';
                Args.SaveBool = false;
            end
            
            raw = importdata(fullfile(UltrasatPerf.RawDataDir,RawFname));
            raw = raw.data;
            
            OBSC= table();
            OBSC.Rdeg  = raw(:,R_col)*Args.R_scale;
            OBSC.obscuration = raw(:,OBSC_col)*Args.OBSC_scale;
            OBSC = sortrows(OBSC,1);

            if Args.SaveBool            
                eval(sprintf('%s = OBSC;',Args.MatFname));
                save(fullfile(UltrasatPerf.RawDataDir,[Args.MatFname,'.mat']),Args.MatFname);
            end
            
        end
        
        
        function [AllQE,QE_names] = read_QE_csv(CSV_Fname,Args)
% AOIw:
% [AllQE,QE_names] = UltrasatPerf.read_QE_csv('aoi_weighted_qe_data_wcubic_icubic_v3.csv','SaveBool',true);
% Experimental:
% [AllQE,QE_names] = UltrasatPerf.read_QE_csv('experimental_qe_data.csv','SaveBool',true,'QE_subDIr','QE_scouts_experimental','Name_end_count',4,'QE_break_str','target_wave');
% S0: 220-250nm; 213
% s1: 220-280nm; 204, 205, 207, 211,212,213
% s2: 220-230nm; 204, 205, 207, 211,212,213
% s3: 220-1100nm; 212,213
% s4: 300-1100nm; 212,213           
            arguments
                CSV_Fname
                Args.QE_break_str    = 'wavelength';
                Args.Name_break_str  = '_';
                Args.Name_end_count  = 3;
                Args.WL_col_name     = 'wavelength';
                Args.WL_scale        = 10; %nm -> A
                Args.SaveBool        = false;
                Args.QE_subDir       = 'QE_scouts_AOI_weighted';
            end
            
            
            QE = io.files.readtable1(fullfile(UltrasatPerf.RawDataDir,Args.QE_subDir,CSV_Fname));
            QE_break_col = find(contains(QE.Properties.VariableNames,Args.QE_break_str));
            N_QE = numel(QE_break_col);
            QE_names = cell(N_QE,1);
            
            AllQE = cell(N_QE,1);
            
            for sIdx = 1:N_QE
                % Cut curr_QE from the full table
                if sIdx<N_QE
                    AllQE{sIdx} = QE(:, QE_break_col(sIdx):( QE_break_col(sIdx+1)-1));
                else
                    AllQE{sIdx} = QE(:, QE_break_col(sIdx):end);
                end
                
                % Get scout name
                Name_break_ind = strfind(AllQE{sIdx}.Properties.VariableNames{1},Args.Name_break_str);
                QE_names{sIdx} = AllQE{sIdx}.Properties.VariableNames{1}(1:(Name_break_ind(Args.Name_end_count)-1));
                
                %modify QE cols names
                for V = 2:size(AllQE{sIdx},2)
                   AllQE{sIdx}.Properties.VariableNames{V} = ...
                       ['R_' AllQE{sIdx}.Properties.VariableNames{V}((Name_break_ind(Args.Name_end_count)+1):end)];
                end              

                %modify wavelength col name and scale
                AllQE{sIdx}.Properties.VariableNames{1} = Args.WL_col_name;
                eval(sprintf('AllQE{%d}.%s = AllQE{%d}.%s*Args.WL_scale;',sIdx,Args.WL_col_name,sIdx,Args.WL_col_name)); %nm -> A

                % remove nan rows
                f_remnan = ~isnan(eval(sprintf('AllQE{%d}.%s',sIdx,Args.WL_col_name)));
                AllQE{sIdx} = AllQE{sIdx}(f_remnan,:);                
                
                % save curr QE
                if Args.SaveBool
                    eval(sprintf('%s = AllQE{%d};',QE_names{sIdx},sIdx));
                    eval(sprintf('save(''%s/%s/%s.mat'',''%s'');',UltrasatPerf.RawDataDir,Args.QE_subDir,QE_names{sIdx},QE_names{sIdx}));
                end
            end

            % Save scouts names
            if Args.SaveBool
                save(fullfile(UltrasatPerf.RawDataDir,Args.QE_subDir,'QE_names.mat'),'QE_names');
            end
        end
        
        function QE = prep_QE_AOIw(Fname_inband,Fname_above_280nm,Args)
% Tstd_207 = UltrasatPerf.prep_QE_AOIw('s3_Tstd_207',[],'QE_name','Tstd_207','SaveBool',true);           
% Tstd_217 = UltrasatPerf.prep_QE_AOIw('s1_Tstd_217','s3_Tstd_207','QE_name','Tstd_217','SaveBool',true);           
% T1_212   = UltrasatPerf.prep_QE_AOIw('s3_T1_212',[],'QE_name','T1_212','SaveBool',true);
% T1_213   = UltrasatPerf.prep_QE_AOIw('s3_T1_213',[],'QE_name','T1_213','SaveBool',true);
% T1_205   = UltrasatPerf.prep_QE_AOIw('s1_T1_205','s3_T1_212','QE_name','T1_205','SaveBool',true);
% T2_211   = UltrasatPerf.prep_QE_AOIw('s3_T2_211',[],'QE_name','T2_211','SaveBool',true);
% T2_204   = UltrasatPerf.prep_QE_AOIw('s1_T2_204','s3_T2_211','QE_name','T2_204','SaveBool',true);
% T2_222   = UltrasatPerf.prep_QE_AOIw('s1_T2_222','s3_T2_211','QE_name','T2_222','SaveBool',true);
% T2_223   = UltrasatPerf.prep_QE_AOIw('s1_T2_223','s3_T2_211','QE_name','T2_223','SaveBool',true);

            arguments
                Fname_inband
                Fname_above_280nm
                Args.wavelength = (1950:10:11000)';
                Args.QE_subDir = 'QE_scouts_AOI_weighted';
                Args.Fname_below_220 = 'QE_below_220nm';
                Args.QE_name = '';
                Args.SaveBool = false;
            end
            
            Nwl = numel(Args.wavelength);
            F1 = Args.wavelength<2200;
            F2 = Args.wavelength>=2200 & Args.wavelength<=2800;
            F3 = Args.wavelength>2800;
            
           
            Below_bool = false;
            if ~isempty(Args.Fname_below_220)
                io.files.load1(fullfile(UltrasatPerf.RawDataDir,Args.QE_subDir,Args.Fname_below_220));
                if contains(Fname_inband,'Tstd')
                    QE_below_IB = interp1(QE_below_220nm.wavelength,QE_below_220nm.Tstd,Args.wavelength(F1));
                elseif  contains(Fname_inband,'T1')
                    QE_below_IB = interp1(QE_below_220nm.wavelength,QE_below_220nm.T1,Args.wavelength(F1));
                elseif contains(Fname_inband,'T2')
                    QE_below_IB = interp1(QE_below_220nm.wavelength,QE_below_220nm.T2,Args.wavelength(F1));
                else
                    error('unkown ARC type');
                end
                Below_bool = true;
            else
                F2 = F2 | F1;
            end
            
            io.files.load1(fullfile(UltrasatPerf.RawDataDir,Args.QE_subDir,Fname_inband));
            QE_IB = eval(Fname_inband);
            
            Above_bool = false;
            if ~isempty(Fname_above_280nm)
                io.files.load1(fullfile(UltrasatPerf.RawDataDir,Args.QE_subDir,Fname_above_280nm));
                QE_above_IB = eval(Fname_above_280nm);
                Above_bool = true;
            else
                F2 = F2 | F3;
            end
            
            QE = table();
            QE.wavelength = Args.wavelength;
            
            for Ridx = 1:25
                currQE = zeros(Nwl,1);
                if Below_bool
                    currQE(F1) = QE_below_IB;
                end
                
                currQE(F2) = interp1(QE_IB.wavelength,table2array(QE_IB(:,2*Ridx)),QE.wavelength(F2));
                
                if Above_bool
                    currQE(F3) = interp1(QE_above_IB.wavelength,table2array(QE_above_IB(:,2*Ridx)),QE.wavelength(F3));
                end
                eval(sprintf('QE.R%d(:) = currQE;',Ridx)); 
            end
            
            if Args.SaveBool
                eval(sprintf('%s = QE;',Args.QE_name));
                save(fullfile(UltrasatPerf.RawDataDir,Args.QE_subDir,Args.QE_name),Args.QE_name);
            end
            
        end
        
        function chromPSF = prep_chromPSF_txt(PSF_fname,Args)
% chromPSF_G6 = UltrasatPerf.prep_chromPSF('UltraSat_G6_PSF.xlsx','remove_jitter',2.35,'assembly_tolrances_factor',1.4,'MatFname','chromPSF_G6','SaveBool',true);
            arguments
                PSF_fname
                Args.FL            = 360;% mm
                Args.Nr            = 25; % number of radial position for PSFs
                Args.EEn           = 50; % Choose EE50 or EE80
                %Args.Rmm           = linspace(0,45,25)*sqrt(2); % [mm] Radial distance vector in mm
                Args.WL_scale      = 10; %nm -> A
                Args.remove_jitter = 0; % 2.35 if Bahalul added
                Args.birefringence = 0.5;
                Args.jitter_XY_3sig = sqrt(2)*2;
                Args.jitter_Z_3sig = 10;
                Args.assembly_tolrances_factor = 1;% ~1.4, if nominal is given, i.e. 40% assembly tolrances 
                Args.MatFname = 'chromPSF';
                Args.EE50_subDir       = 'EE50';
                Args.SaveBool = false;                
            end
            
            rawEE = importdata(fullfile(UltrasatPerf.RawDataDir,Args.EE50_subDir,PSF_fname));

            Rmm(1,:) = sscanf(sprintf(' %s',rawEE.textdata{1:Args.Nr}),'%f');
            chromPSF.Rdeg = Rmm * convert.angular('rad','deg')/Args.FL;
           
            
            f_ee = rawEE.data(:,Args.Nr+2)==Args.EEn; % filter EE50 / EE80
            Nwl = sum(f_ee);
            
            chromPSF.wavelength = rawEE.data(f_ee,Args.Nr+1)*Args.WL_scale;
          
            chromPSF.PSF = rawEE.data(f_ee,1:Args.Nr); % get raw PSF
            
            chromPSF.PSF = sqrt(chromPSF.PSF.^2-Args.remove_jitter.^2); % Remove the jitter added by Eran Bahlul (I'm adding it later below)
            chromPSF.PSF = chromPSF.PSF + Args.birefringence;           % adding for extraordinary rays
            
            Rarcsec = chromPSF.Rdeg * convert.angular('deg','arcsec'); 
            R_3sig_jitter = sqrt(Args.jitter_XY_3sig.^2 +...
                    (tan(Args.jitter_Z_3sig*convert.angular('arcsec','rad'))*Rarcsec).^2);
            R_EE_jitter = R_3sig_jitter/3*2.355;
            
            EE_jitter = repmat(R_EE_jitter,Nwl,1);
            chromPSF.PSF = sqrt((chromPSF.PSF * Args.assembly_tolrances_factor).^2 + EE_jitter.^2);
            
            if Args.SaveBool
                eval(sprintf('%s = chromPSF;',Args.MatFname));
                save(fullfile(UltrasatPerf.RawDataDir,Args.EE50_subDir,[Args.MatFname,'.mat']),Args.MatFname);
            end
            
        end
        
        
        function chromPSF = prep_chromPSF_EXCELL(PSF_fname,Args)
% chromPSF_G6 = UltrasatPerf.prep_chromPSF('UltraSat_G6_PSF.xlsx','remove_jitter',2.35,'assembly_tolrances_factor',1.4,'MatFname','chromPSF_G6','SaveBool',true);
            arguments
                PSF_fname
                Args.FL            = 360;% mm
                Args.EEsheet       = 'EE50';
                Args.Rmm           = linspace(0,45,25)*sqrt(2); % [mm] Radial distance vector in mm
                Args.WL_scale      = 10; %nm -> A
                Args.remove_jitter = 0; % 2.35 if Bahalul added
                Args.birefringence = 0.5;
                Args.jitter_XY_3sig = sqrt(2)*2;
                Args.jitter_Z_3sig = 10;
                Args.assembly_tolrances_factor = 1;% ~1.4, if nominal is given, i.e. 40% assembly tolrances 
                Args.MatFname = 'chromPSF';
                Args.SaveBool = false;                
            end
            
            
            
            RAD = 180./pi;
            Rarcsec = Args.Rmm * convert.angular('rad','arcsec')/Args.FL;
            
            rawEE = importdata(fullfile(UltrasatPerf.RawDataDir,PSF_fname));
            rawEE = eval(sprintf('rawEE.data.%s',Args.EEsheet));
            
            
            chromPSF = table();
            chromPSF.wavelength = rawEE(:,1)*Args.WL_scale;
            
            rawEE = rawEE(:,2:end);
            rawEE = sqrt(rawEE.^2-Args.remove_jitter.^2); % Remove the jitter added by Eran Bahlul (I'm adding it later below)
            rawEE = rawEE + Args.birefringence;           % adding for extraordinary rays
            
            for R = 1:numel(Rarcsec)
                currEE = rawEE(:,R);
                curr_3sig_jitter = sqrt(Args.jitter_XY_3sig.^2 +...
                    (tan(Args.jitter_Z_3sig*convert.angular('arcsec','rad'))*Rarcsec(R))^2);
                curr_EE_jitter = curr_3sig_jitter/3*2.355;
                currEE = sqrt((currEE * Args.assembly_tolrances_factor).^2 + curr_EE_jitter^2);
                eval(sprintf('chromPSF.R%d = currEE;',R));
            end
            
            if Args.SaveBool
                eval(sprintf('%s = chromPSF;',Args.MatFname));
                save(fullfile(UltrasatPerf.RawDataDir,[Args.MatFname,'.mat']),Args.MatFname);
            end
            
        end
  
    end

    %----------------------------------------------------------------------
    methods (Access = protected)
        function NewObj = copyElement(Obj)
            % Custom copy of object properties
            % Called from copy() of matlab.mixin.Copyable decendents
            
            % Make shallow copy of all properties

        end
    end
    
    %----------------------------------------------------------------------
    
    methods (Static) % Unit-Test
        Result = unitTest()
            % Unit-Test
                        
    end
    
end
