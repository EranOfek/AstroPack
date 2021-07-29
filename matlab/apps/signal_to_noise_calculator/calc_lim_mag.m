load('../Total_throughput/Scouts_opt.mat');
load('../Comp_scouts_opt.mat');
load('../effPSF/Specs.mat');

SL_bool = 1; % Factor Straylight according OOB supression.

ELOP_SL_SUPRESSION = 1e-9;%1e-10;

BaseStrayLight = 3.5^2;
Req_SL_Supression = 2.2e-11;
RefAttentuation = Comp_scouts_opt.Elop2_T2_211.mean_supression(1);
SL_factor = 1;


N_comb = numel(Scouts_opt.name);
N_srcs = numel(Specs.name);

for opt = 1:N_comb
    load(sprintf('../Total_throughput/%s.mat',Scouts_opt.name{opt}));
end

ClearAper = 1; % Obscuration already taken into account
%Wave = (2000:10:10000)';


tic;
for Sidx = 1:numel(Specs.Spec)
    clear AstFilterCat
    AstFilter.get;
    
    TargetSpec  = Specs.Spec(Sidx);
    
    
    [MagGaia,~] = AstroUtil.spec.synphot([TargetSpec.Wave TargetSpec.Int],'GAIA','G','AB');
    [MagGaiaBP,~] = AstroUtil.spec.synphot([TargetSpec.Wave TargetSpec.Int],'GAIA','BP','AB');
    [MagGaiaRP,~] = AstroUtil.spec.synphot([TargetSpec.Wave TargetSpec.Int],'GAIA','RP','AB');
    eval(sprintf('Comp_scouts_opt.GaiaBPRP.%s = MagGaiaBP-MagGaiaRP;',Specs.name{Sidx}));
    
    for opt = 1:N_comb
        eval(sprintf('Curr_scout_opt = %s;',Scouts_opt.name{opt}));
        
        curr_LimMag = zeros(25,1);
        curr_SN = cell(25,1);
        curr_Color = zeros(25,1);
        
        for R = 1:25
            
            % Create filter object
            F_curr = AstFilter;
            F_curr.family = 'ULTRASAT';
            
            % Filter
            band_name = sprintf('%s_R%d',Scouts_opt.name{opt},R );       
            F_curr.band = band_name;
            
            eval(sprintf('F_curr.T = [%s.wavelength %s.R%d];', Scouts_opt.name{opt}, Scouts_opt.name{opt}, R));
            
            % Create object 
            AstFilterCat = F_curr.add_filter();
            F_curr = AstFilterCat.get('ULTRASAT',band_name);
            
            %
            eval(sprintf('FWHM = Comp_scouts_opt.%s.effFWHM_%s(%d);',Scouts_opt.name{opt},Specs.name{Sidx}, R));
            
            if (SL_bool)
                eval(sprintf('CurrAttentuation = Comp_scouts_opt.%s.mean_supression(%d);',Scouts_opt.name{opt}, R));
                SL_factor = (ELOP_SL_SUPRESSION/Req_SL_Supression) * (CurrAttentuation/RefAttentuation);
            end
            
            [curr_SN{R}] = telescope.sn.snr('FWHM',FWHM,'ClearAper',ClearAper,'TargetSpec',TargetSpec,'FilterFamily',F_curr,...
                                          'StrayLight',BaseStrayLight*SL_factor,'Trans',1);
                                      
                                      
            curr_LimMag(R) = curr_SN{R}.LimMag;
            [NUV_Mag,~] = AstroUtil.spec.synphot([TargetSpec.Wave TargetSpec.Int],F_curr,'bla','AB');
            curr_Color(R) = NUV_Mag-MagGaia;
        end
        eval(sprintf('Comp_scouts_opt.%s.LimMag_%s(:) = curr_LimMag;',Scouts_opt.name{opt},Specs.name{Sidx}));
       % eval(sprintf('Comp_scouts_opt.%s.SN_%s(:) = curr_SN;',Scouts_opt.name{opt},Specs.name{Sidx}));
        eval(sprintf('Comp_scouts_opt.%s.Color_%s(:) = curr_Color;',Scouts_opt.name{opt},Specs.name{Sidx}));
        eval(sprintf('Comp_scouts_opt.%s.G_LimMag_%s(:) = curr_LimMag-curr_Color;',Scouts_opt.name{opt},Specs.name{Sidx}));
    end
    toc;
end

%
save('../Comp_scouts_opt.mat','Comp_scouts_opt');

%
save('AstFilterCat.mat', 'AstFilterCat');



