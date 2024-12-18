function Cat = getFOVcatalog(RA, Dec, Args)
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
        Args.Radius            = 0.1;       % search radius in [deg]
        Args.Ebv               = 0.02;      % average Ebv for the field unless we have Ebv in the catalog
        Args.CatName           = 'GAIADR3'; % name of one of the available catsHTM catalogs
        Args.USFilter          = 1;  
        Args.WriteFile         = true; 
        Args.Plot              = false;
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
    Cat.USat_SNR = zeros(height(Cat), 1); % For numeric data
    Cat.USat_Mag = zeros(height(Cat), 1); % For numeric data
%     Cat.Class    = repmat(' ',1,height(Cat))'; % Object Class
    for Iobj=1:Nobj
        if isnan(Cat.teff_gspphot(Iobj)) % witout Teff or a spectrum we cannot run telescope.sn.snr
            Cat.USat_SNR(Iobj) = 0;
            Cat.USat_Mag(Iobj) = 99;
        else
            SNR = telescope.sn.snr('TargetSpec',Cat.teff_gspphot(Iobj),'Mag',Cat.phot_g_mean_mag(Iobj),...
                'CalibFilterFamily','GAIA','CalibFilter','g','FilterFamily',UP.U_AstFilt(Args.USFilter),...
                'Filter',' ','Ebv',Ebv(Iobj));
            Cat.USat_SNR(Iobj) = SNR.SNR;
            Cat.USat_Mag(Iobj) = SNR.Mag;
        end
        if mod(Iobj, ceil(Nobj/100)) == 0
            fprintf('Progress: %.0f%%\n', (Iobj/Nobj) * 100);
        end       
    end
    % write the output object
    if Args.WriteFile
        FN = sprintf('extcatRA%.1fDec%.1fRad%.1f.mat',RA,Dec,Args.Radius);
        save(FN,'Cat','-v7.3');
    end
    % plot the HR diagram
    if Args.Plot
        F = Cat.ErrPlx<(0.1*Cat.Plx);
        figure(10)
%         plot(Cat.phot_bp_mean_mag(F)-Cat.phot_rp_mean_mag(F),Cat.phot_g_mean_mag(F)-5*log10(100./(Cat.Plx(F))),'.');
        plot(Cat.bp_rp(F),Cat.phot_g_mean_mag(F)-5*log10(100./(Cat.Plx(F))),'.'); set(gca,'YDir','reverse')
    end
end
