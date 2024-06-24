function CalStars = calibrationStars(Args)
    % provides a structure with calibration star spectra and coordinates
    % for the low-candence and high-cadence fields of ULTRASAT 

    arguments
        Args.DataDir = '~/ULTRASAT/CalibStellarLibs/';
        Args.Cat     = 'STISngsl10';           % 'STISngsl10' or 'Starlib23';
    end

    RAD = 180./pi;

    % the gal. plane borders at |b| = 30
    gal_l = 0:360; gal_b30p = 30.*ones(1,361); gal_b30m = -gal_b30p;

    % read the properties DB
    UP_db = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/P90_UP_test_60_ZP_Var_Cern_21.mat');
    io.files.load1(UP_db,'UP');

    % read the HST stellar catalog, convert to deg, and add the galactic coordinates to the table
    cd(Args.DataDir); % go to the the data dir
    
    switch Args.Cat 
        case 'STISngsl10'
            tab = readtable('STISngsl10list.csv');
            fprintf('%d catalog stars read\n',size(tab,1))
            ra1  = tab.Var2;  ra2 = tab.Var3;  ra3 = tab.Var4;
            dec1 = tab.Var5; dec2 = tab.Var6; dec3 = tab.Var7;
            
            decdeg = (dec1/abs(dec1))*( abs(dec1)+dec2/60+dec3/3600 ); decRAD = decdeg/RAD;
            radeg  = ra1*(360/24)+ra2*(360/24)/60+ra3*(360/24)/3600;    raRAD = radeg/RAD;
            
            [gal_lon, gal_lat] = celestial.coo.convert_coo(raRAD,decRAD);
            gal_lon = gal_lon .* RAD; gal_lat = gal_lat .* RAD;
            
            tab = addvars(tab, gal_lon, gal_lat, radeg, decdeg, 'NewVariableNames', {'l', 'b', 'RA', 'Dec'});
            tab.Properties.VariableNames(1) = {'obj'}; tab.Properties.VariableNames(10) = {'class'};
            tab.Properties.VariableNames(8) = {'B'}; tab.Properties.VariableNames(9) = {'V'}; 
        case 'Starlib23'
            tab  = readtable('Starlib23_Tab3_v3.csv');
            tabc = readtable('Starlib23_Tab3_cooSimbad1.csv');
            RA = tabc.Var1; Dec = tabc.Var2;
            [gal_lon, gal_lat] = celestial.coo.convert_coo(RA/RAD,Dec/RAD);
            gal_lon = gal_lon .* RAD; gal_lat = gal_lat .* RAD;
            tab = addvars(tab, RA, Dec, gal_lon, gal_lat,'NewVariableNames', {'RA','Dec','l','b'});
            tab.Properties.VariableNames{'SimbadName'} = 'obj';
            tab.Properties.VariableNames{'V_mag_'} = 'V';
            tab.obj=regexprep(tab.obj,'\s+',''); % remove the blanks
%             S = regexp(tab.MASTFile,'stis_\w*[^_]','match'); % try this
        otherwise
            error('The requested catalog is not available. Please, check the name.');
    end
    
    lcad = tab(abs(tab.b) > 30,:);
    fprintf('%d stars in the low cadence region\n',size(lcad,1))
    hcad = lcad(abs(lcad.Dec) > 50,:);
    fprintf('%d stars in the high cadence region\n',size(hcad,1))

    % write the star lists and plot them upon the sky
    writetable(lcad, 'lcad.txt', 'Delimiter', '\t'); 
    writetable(hcad, 'hcad.txt', 'Delimiter', '\t'); 

    figure(1); subplot(2,1,1)
    plot(lcad.l,lcad.b,"*"); hold on
    plot(hcad.l,hcad.b,"o",'Color','red')
    plot(gal_l,gal_b30p, 'Color','black'); plot(gal_l,gal_b30m, 'Color','black');
    xlabel l; ylabel b; xlim([0 361]); ylim([-91 91]); hold off

    subplot(2,1,2)
    plot(lcad.RA,lcad.Dec,"*"); hold on
    plot(hcad.RA,hcad.Dec,"o",'Color','red')
    [gal30RA, gal30Dec] = celestial.coo.convert_coo(gal_l/RAD,gal_b30p/RAD,'g','j2000');
    plot(gal30RA*RAD,gal30Dec*RAD,'Color','black' );
    [gal30RA , gal30Dec] = celestial.coo.convert_coo(gal_l/RAD,gal_b30m/RAD,'g','j2000');
    plot(gal30RA*RAD,gal30Dec*RAD,'Color','black');
    xlabel RA; ylabel Dec; xlim([0 361]); ylim([-91 91]); hold off

    % visibility of the objects:
    JD = celestial.time.julday('2026-07-01 00:00:00') + (0:0.1:365)';
    VisHi = ultrasat.ULTRASAT_restricted_visibility(JD,[hcad.RA,hcad.Dec]);
    VisLo = ultrasat.ULTRASAT_restricted_visibility(JD,[lcad.RA,lcad.Dec]);
    CumLimitHi = VisHi.SunLimits .* VisHi.EarthLimits .* VisHi.MoonLimits;
    CumLimitLo = VisLo.SunLimits .* VisLo.EarthLimits .* VisLo.MoonLimits;
    AnnualVisHi = sum(CumLimitHi,1); AnnualVisLo = sum(CumLimitLo,1); % number of 0.1 day long windows per year

    figure(2); 
    subplot(2,1,1); plot(AnnualVisHi,'*'); xlabel 'high candence obj'; ylabel '0.1 day-long frames/yr'; title 'Annual visibility'
    subplot(2,1,2); plot(AnnualVisLo,'*'); xlabel 'low cadence obj'  ; ylabel '0.1 day-long frames/yr'
    
    % plot the lg(T) -- lg(g) diagram (for 'Starlib23' only)
    if strcmp(Args.Cat,'Starlib23')
        figure(10)
        plot(log10(lcad.Teff_K_),lcad.logG,'*'); hold on
        plot(log10(hcad.Teff_K_),hcad.logG,'o','Color','green')
        xlabel lg(T_{eff}); ylabel lg(g)
    end

    % individual spectral data of the high-cadence objects:
    NHi = size(hcad,1); 
    SpecHi = repmat(AstroSpec,1,NHi);
    fprintf('high-cadence stars: \n')
    for Isrc=1:1:NHi
        fprintf('%s',hcad.obj{Isrc});
        Fname = sprintf('Starlib23Spec/%s.fits',hcad.obj{Isrc});
        Ftab  = fitsread(Fname,'binarytable');
        Sp = [Ftab{1} Ftab{6} Ftab{7}];
        SpecHi(Isrc) = AstroSpec(Sp);
        MagV0(Isrc) = hcad.V(Isrc);     % table value of the V magnitude
        MagV(Isrc)  = astro.spec.synthetic_phot([Ftab{1} Ftab{6}],'Johnson','V','AB');          % check the value of V
        MagU1h(Isrc) = astro.spec.synthetic_phot([Ftab{1} Ftab{6}],UP.U_AstFilt(1),'R1','AB');   % ULTRASAT R1 magn.
        SNR250h(Isrc) = Sp(576,2)/Sp(576,3); % the S/N ratio at 250 nm
        
        if (1-MagV0(Isrc)/MagV(Isrc)) > 0.1
            fprintf(' V0 = %.2f, V - %.2f \n',MagV0(Isrc),MagV(Isrc))
        else
            fprintf('\n')
        end

        figure(3); subplot(2,1,1)
        plot(Sp(:,1),log10(Sp(:,2)),'.'); xlim([2000 3000]); ylabel 'Lg(F)'
        if strcmp(Args.Cat,'STISngsl10')
            Name = sprintf('%s class %s',hcad.obj{Isrc},hcad.class{Isrc}); title(Name)
        else
            Name = sprintf('%s',hcad.obj{Isrc}); title(Name)
        end
        subplot(2,1,2)
        errorbar(Sp(:,1),Sp(:,2),Sp(:,3),'.'); xlim([2000 3000]); ylabel 'F, erg/cm(2)/s/A'
        Name = sprintf('V=%.2f V0=%.2f U(R1)=%.2f',MagV(Isrc),MagV0(Isrc),MagU1h(Isrc)); title(Name)

%         pause(1);
%         pause;   
    end
    
    % individual spectra of low-cadence objects:
    NLo = size(lcad,1); 
    SpecLo = repmat(AstroSpec,1,NLo);
    fprintf('low-cadence stars: \n')
    Skip = 0; try
    for Isrc=1:1:NLo
        fprintf('%s',lcad.obj{Isrc});
        Fname = sprintf('Starlib23Spec/%s.fits',lcad.obj{Isrc});
        Ftab  = fitsread(Fname,'binarytable');
        Sp = [Ftab{1} Ftab{6} Ftab{7}];
        SpecLo(Isrc) = AstroSpec(Sp);
        MagV0(Isrc) = lcad.V(Isrc);     % table value of the V magnitude
        MagV(Isrc)  = astro.spec.synthetic_phot([Ftab{1} Ftab{6}],'Johnson','V','AB');          % check the value of V
        MagU1l(Isrc) = astro.spec.synthetic_phot([Ftab{1} Ftab{6}],UP.U_AstFilt(1),'R1','AB');   % ULTRASAT R1 magn.
        SNR250l(Isrc) = Sp(576,2)/Sp(576,3); % the S/N ratio at 250 nm
        
        if (1-MagV0(Isrc)/MagV(Isrc)) > 0.1
            fprintf(' V0 = %.2f, V - %.2f \n',MagV0(Isrc),MagV(Isrc))
        else
            fprintf('\n')
        end

%         figure(3); subplot(2,1,1)
%         plot(Sp(:,1),log10(Sp(:,2)),'.'); xlim([2000 3000]); ylabel 'Lg(F)'
%         Name = sprintf('%s class %s',hcad.obj{Isrc},hcad.class{Isrc}); title(Name)
%         subplot(2,1,2)
%         errorbar(Sp(:,1),Sp(:,2),Sp(:,3),'.'); xlim([2000 3000]); ylabel 'F, erg/cm(2)/s/A'
%         Name = sprintf('V=%.2f V0=%.2f U(R1)=%.2f',MagV(Isrc),MagV0(Isrc),MagU1l(Isrc)); title(Name)

    end
    catch
        cprintf('err','\nsome of the low-cadence filenames are corrupted\n');
        Skip = 1;
    end

    % plot the ULTRASAT magnitudes and SNR ratios, write down the lists and
    % make the output structures
    figure(4); subplot(2,2,1); hold off
    plot(hcad.l,12.*ones(1,NHi),'DisplayName','sat. 300s'); hold on
    plot(hcad.l,10.*ones(1,NHi),'DisplayName','sat. 30s'); 
    plot(hcad.l,7.*ones(1,NHi),'DisplayName' ,'sat. 3s');
    plot(hcad.l,MagU1h,'*','DisplayName','objects')
    title 'high cadence stars'; legend('Location', 'best');
    ylabel 'ULTRASAT R1 magn'; xlabel 'l, deg';
    subplot(2,2,2); hold off
    plot(hcad.l,log10(SNR250h),'*')
    ylabel 'lg(S/N) at 250 nm'; xlabel 'l, deg';
    
    hcad2 = addvars(hcad, MagU1h',SNR250h', 'NewVariableNames', {'MagU_R1','SNR_250nm'});
%     hcad2 = removevars(hcad2,{'B','V','Var2','Var3','Var4','Var5','Var6','Var7'});
    hcad2 = sortrows(hcad2,'MagU_R1','descend');
    writetable(hcad2, 'hcad2.txt', 'Delimiter', '\t'); 
    
    CalStars.SpecHi = SpecHi;
    CalStars.CatHi = [hcad2.l hcad2.b hcad2.RA hcad2.Dec hcad2.MagU_R1 hcad2.SNR_250nm];
    CalStars.NamesHi = hcad2.obj;
    
    % low cadence: 
    subplot(2,2,3)
    plot(lcad.l,12.*ones(1,NLo),'DisplayName', 'sat. 300s'); hold on
    plot(lcad.l,10.*ones(1,NLo),'DisplayName',  'sat. 30s'); 
    plot(lcad.l, 7.*ones(1,NLo),'DisplayName',   'sat. 3s');
    if ~Skip 
        plot(lcad.l,MagU1l,'*','DisplayName','objects')
        title 'low cadence stars'; 
        ylabel 'ULTRASAT R1 magn'; xlabel 'l, deg';
        subplot(2,2,4); hold off
        plot(lcad.l,log10(SNR250l),'*'); hold on
        plot(lcad.l,1.*ones(1,NLo))
        ylabel 'lg(S/N) at 250 nm'; xlabel 'l, deg';
        
        lcad2 = addvars(lcad, MagU1l',SNR250l', 'NewVariableNames', {'MagU_R1','SNR_250nm'});
%         lcad2 = removevars(lcad2,{'B','V','Var2','Var3','Var4','Var5','Var6','Var7'});
        lcad2 = sortrows(lcad2,'MagU_R1','descend');
        writetable(lcad2, 'lcad2.txt', 'Delimiter', '\t');
        
        CalStars.SpecLo = SpecLo;
        CalStars.CatLo = [lcad2.l lcad2.b lcad2.RA lcad2.Dec lcad2.MagU_R1 lcad2.SNR_250nm];
        CalStars.NamesLo = lcad2.obj;
    end
    
end
