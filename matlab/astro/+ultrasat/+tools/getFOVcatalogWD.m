function Cat = getFOVcatalogWD(RA, Dec, Args)
    % get an extract of GAIA catalog for the given ULTRASAT position
    %     Optional detailed description
    % Input  : - RA, deg (center of FOV)
    %          - Dec, deg (center of FOV)
    %          * ...,key,val,... 
    % Output : - a catalog of objects with ULTRASAT SNR and magnitude
    % Author : A.M. Krassilchtchikov (2024 Dec) 
    % Example: Cat = ultrasat.tools.getFOVcatalog(67.,-59.,'Radius',0.1,'Ebv',0.02);
    arguments
        RA                     = 67.        % ULTRASAT S1
        Dec                    = -59.       % ULTRASAT S1
        Args.Radius            = 0.3;       % search radius in [deg]
        Args.Ebv               = 0.02;      % average Ebv for the field unless we have Ebv in the catalog
        Args.ExpTime           = 300;       % ULTRASAT exposure time
        Args.CatName           = 'WDEDR3';  % name of one of the available catsHTM catalogs
        Args.MagnitideColumn   = 'Gmag';    % name of magnitude column e.g., phot_g_mean_mag, Gmag
        Args.TemperatureColumn = 'TeffH';   % name of temperature column e.g., teff_gspphot, TeffH
        Args.USFilter          = 1;  
        Args.WriteFile         = false;      
    end
    %
    RAD = 180/pi;    
    % load the UP object with Ultrasat filters
    I = Installer;
    UP_db = sprintf('%s%s',I.getDataDir('ULTRASAT_Properties'),'/P90_UP_test_60_ZP_Var_Cern_21.mat');  
    io.files.load1(UP_db,'UP');
    % get the catalog  
    [Cat,~]= catsHTM.cone_search(Args.CatName,RA/RAD,Dec/RAD,Args.Radius,'RadiusUnits','deg','OutType','table');
    Nobj   = height(Cat);    
    % use Ebv from the catalog or 1 value for the whole field 
    if ismember('Ebv', Cat.Properties.VariableNames)
        Ebv = Cat.Ebv;
    else
        Ebv = repmat(Args.Ebv,1,Nobj);
    end
    fprintf('Total number of objects: %d\n',Nobj);
    % 
    Cat.USat_SNR = zeros(height(Cat), 1); 
    Cat.USat_Mag = zeros(height(Cat), 1); 
    Cat.TeffFromColor = zeros(height(Cat), 1);
    Cat.Class    = repmat(' ',1,height(Cat))'; 
    % empirical fit:
    BP_RP = Cat.BPmag-Cat.RPmag;
    Cat.TeffFromColor = 10.^( 4.11-0.677.*BP_RP+0.294*BP_RP.^2);
    
    for Iobj=1:Nobj
        if isnan(Cat.(Args.TemperatureColumn)(Iobj)) % witout Teff or a spectrum we cannot run telescope.sn.snr
            SNR = telescope.sn.snr('TargetSpec',Cat.TeffFromColor(Iobj),'Mag',Cat.(Args.MagnitideColumn)(Iobj),...
                'CalibFilterFamily','GAIA','CalibFilter','g','FilterFamily',UP.U_AstFilt(Args.USFilter),...
                'Filter',' ','Ebv',Ebv(Iobj),'ExpTime',Args.ExpTime);
            Cat.USat_SNR(Iobj) = SNR.SNR;
            Cat.USat_Mag(Iobj) = SNR.Mag;
%             Cat.USat_SNR(Iobj) = 0;
%             Cat.USat_Mag(Iobj) = 99;
        else           
            SNR = telescope.sn.snr('TargetSpec',Cat.(Args.TemperatureColumn)(Iobj),'Mag',Cat.(Args.MagnitideColumn)(Iobj),...
                'CalibFilterFamily','GAIA','CalibFilter','g','FilterFamily',UP.U_AstFilt(Args.USFilter),...
                'Filter',' ','Ebv',Ebv(Iobj),'ExpTime',Args.ExpTime);
            Cat.USat_SNR(Iobj) = SNR.SNR;
            Cat.USat_Mag(Iobj) = SNR.Mag;
        end
        if mod(Iobj, ceil(Nobj/100)) == 0
            fprintf('Progress: %.0f%%\n', (Iobj/Nobj) * 100);
        end       
    end
    % write the output object
    if Args.WriteFile
        FN = sprintf('extcat_%s_RA%.1fDec%.1fRad%.1f.mat',Args.CatName,RA,Dec,Args.Radius);
        save(FN,'Cat');
    end
end
