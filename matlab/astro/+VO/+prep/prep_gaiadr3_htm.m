function prep_gaiadr3_htm(varargin)
% SHORT DESCRIPTION HERE
% Package: VO
% Description: 
% Input  : - 
%          * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
% Output : - 
% License: GNU general public license version 3
%     By : Eran O. Ofek                    Apr 2018
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: VO.prep.prep_gaia_htm
% Reliable: 
%--------------------------------------------------------------------------



DefV.URL                  = 'http://cdn.gea.esac.esa.int/Gaia/gedr3/gaia_source/';
%http://o501sd3ggsiwxpbv8jpv.push-19.cdn77.com/Gaia/gdr2/gaia_source/csv/';
DefV.Dir                  = 'zz1';
InPar = InArg.populate_keyval(DefV,varargin,mfilename);

%%
% get all files for GAIA-DR2 sources
List=www.find_urls(InPar.URL,'strfind','.csv.gz');
www.pwget(List,'',15);

% !gzip -d *.gz



%% read single file
% File = 'GaiaSource_1000172165251650944_1000424567594791808.csv';
% FID  = fopen(File,'r');
% Line = fgetl(FID);
% ColCellAll = regexp(Line,',','split');
% fclose(FID);

%%
ColCell = {'RA','Dec','Epoch','ErrRA','ErrDec','Plx','ErrPlx','PMRA','ErrPMRA','PMDec','ErrPMDec',...
           'ra_dec_corr','astrometric_n_obs_al','astrometric_n_obs_ac',...
           'astrometric_n_good_obs_al', 'astrometric_n_bad_obs_al',...
            'astrometric_gof_al',...
            'astrometric_excess_noise',...
            'astrometric_excess_noise_sig',...
            'astrometric_chi2_al',...
            'astrometric_params_solved',...
            'nu_eff_used_in_astrometry',...
            'pseudocolour',...
            'pseudocolour_error',...
            'astrometric_sigma5d_max',...
            'phot_g_n_obs',...
            'phot_g_mean_mag',...
            'phot_g_mean_flux_over_error',...
            'phot_bp_mean_mag',...
            'phot_bp_mean_flux_over_error',...
            'phot_rp_mean_mag',...
            'phot_rp_mean_flux_over_error',...
            'phot_bp_rp_excess_factor',...
            'bp_rp',...
            'radial_velocity',...
            'radial_velocity_error',...
            'rv_amplitude_robust',...
            'phot_variable_flag',...
            'in_qso_candidates',...
            'in_galaxy_candidates',...
            'non_single_star',...
            'has_xp_continuous',...
            'teff_gspphot',...
            'teff_gspphot_lower',...
            'teff_gspphot_upper',...
            'logg_gspphot',...
            'logg_gspphot_lower',...
            'logg_gspphot_upper',...
            'mh_gspphot',...
            'mh_gspphot_lower',...
            'mh_gspphot_upper',...
            'azero_gspphot',...
            'azero_gspphot_lower',...
            'azero_gspphot_upper'};
           
%%
% list of required columns and their new names:
%            GAIA name , catsHTM name, catsHTM position

ColNames = {'ra',       'RA';...
            'dec',      'Dec';...
            'ref_epoch','Epoch';...
            'ra_error', 'ErrRA';...
            'dec_error','ErrDec';...
            'parallax','Plx';...
            'parallax_error','ErrPlx';...
            'pmra','PMRA';...
            'pmra_error','ErrPMRA';...
            'pmdec','PMDec';...
            'pmdec_error','ErrPMDec';...            
            'ra_dec_corr','ra_dec_corr';...
            'astrometric_n_obs_al','astrometric_n_obs_al';...
            'astrometric_n_obs_ac','astrometric_n_obs_ac';...
            'astrometric_n_good_obs_al','astrometric_n_good_obs_al';...
            'astrometric_n_bad_obs_al','astrometric_n_bad_obs_al';...
            'astrometric_gof_al','astrometric_gof_al';...
            'astrometric_excess_noise','astrometric_excess_noise';...
            'astrometric_excess_noise_sig','astrometric_excess_noise_sig';...
            'astrometric_chi2_al','astrometric_chi2_al';...
            'astrometric_params_solved','astrometric_params_solved';...
            'nu_eff_used_in_astrometry','nu_eff_used_in_astrometry';...
            'pseudocolour','pseudocolour';...
            'pseudocolour_error','pseudocolour_error';...
            'astrometric_sigma5d_max','astrometric_sigma5d_max';...
            'phot_g_n_obs','phot_g_n_obs';...
            'phot_g_mean_mag','phot_g_mean_mag';...
            'phot_g_mean_flux_over_error','phot_g_mean_flux_over_error';...
            'phot_bp_mean_mag','phot_bp_mean_mag';...
            'phot_bp_mean_flux_over_error','phot_bp_mean_flux_over_error';...
            'phot_rp_mean_mag','phot_rp_mean_mag';...
            'phot_rp_mean_flux_over_error','phot_rp_mean_flux_over_error';...
            'phot_bp_rp_excess_factor','phot_bp_rp_excess_factor';...
            'bp_rp','bp_rp';...
            'radial_velocity','radial_velocity';...
            'radial_velocity_error','radial_velocity_error';...
            'rv_amplitude_robust','rv_amplitude_robust';...
            'in_qso_candidates','in_qso_candidates';...
            'in_galaxy_candidates','in_galaxy_candidates';...
            'non_single_star','non_single_star';...
            'has_xp_continuous','has_xp_continuous';...
            'teff_gspphot','teff_gspphot';...
            'teff_gspphot_lower','teff_gspphot_lower';...
            'teff_gspphot_upper','teff_gspphot_upper';...
            'logg_gspphot','logg_gspphot';...
            'logg_gspphot_lower','logg_gspphot_lower';...
            'logg_gspphot_upper','logg_gspphot_upper';...
            'mh_gspphot','mh_gspphot';...
            'mh_gspphot_lower','mh_gspphot_lower';...
            'mh_gspphot_upper','mh_gspphot_upper';...
            'azero_gspphot','azero_gspphot';...
            'azero_gspphot_lower','azero_gspphot_lower';...
            'azero_gspphot_upper','azero_gspphot_upper'}
            
ColUnits = {'rad','rad','JYear','mas','mas','mas','mas',...
        'mas/yr','mas/yr','mas/yr','mas/yr',...
        '','','','','','',...
        'mas','',...
        '','','','','',...
        'mas',...
        '','mag','s/n','mag','s/n','mag','s/n',...
        '','mag',...
        'km/s','km/s','km/s',...
        'bool','bool','bool','bool',...
        'K','K','K','logg','logg','logg','[Fe/H]','[Fe/H]','[Fe/H]',...
        'mag','mag','mag'};
        
        
        
%%
FID = fopen('GaiaSource_779915-780373.csv','r');
Line = fgetl(FID);
while strcmp(Line(1),'#')
    Line = fgetl(FID);
end
fclose(FID);
SpLine = regexp(Line,',','split');


% ColNames - [GAIA column name,  catsHTM column name, GAIA coulumn index]

Ncol = size(ColNames,1);
for Icol=1:1:Ncol
    GaiaColInd = find(strcmp(ColNames{Icol,1}, SpLine));
    ColNames{Icol,3} = GaiaColInd;
end
    
SelectedCol = cell2mat(ColNames(:,3)).';


%%
%[~,SI]=sort(cell2mat(ColNames(:,3)));

BolList = {'has_xp_continuous','in_qso_candidates','in_galaxy_candidates'};
Nbl = numel(BolList);

Dir = dir('GaiaSource_*.csv');

Nfile = numel(Dir);
SumC  = nan(Nfile,7);
K = 0;

for If=1:1:Nfile
    [If, Nfile]
    File = Dir(If).name;
    
    tic;
	
    TT = io.files.readtable1(File,'Delimiter',',','CommentStyle','#','NumHeaderLines',1,'TreatAsMissing','null');
    TT = TT(:,SelectedCol);
    TT.Properties.VariableNames = ColNames(:,2).';
    NT = size(TT, 1);
    
    for Ibl=1:1:Nbl
        FlagT =strcmp(TT.(BolList{Ibl}),'True');
        TT.(BolList{Ibl}) = false(NT,1);
        TT.(BolList{Ibl})(FlagT) = true;
    end
    Mat = table2array(TT);
    
    K = K + 1;
    SaveFile = sprintf('H5_%d.hdf5',K);
    %cd(InPar.Dir)
    HDF5.save(Mat,SaveFile);
    %cd ..

    Ifile = 1;
    SumC(K,:) = [Ifile, K, min(Mat(:,1)), max(Mat(:,1)), min(Mat(:,2)), max(Mat(:,2)), size(Mat,1)];
    toc
end
    
save SumC.mat SumC


%% Count sources in Dec strips
%cd /raid/eran/catsHTM/GAIA/DRE3/
cd /data/euler/catsHTM/GAIA/DR3/prep/src
load SumC.mat

VecDec = (-90:0.1:90)';
Ndec   = numel(VecDec);

Nsum = size(SumC,1);
Status = false(Nsum,Ndec-1);
for Isum=1:1:Nsum
    %
    tic;
    CurInd = SumC(Isum,2);
        
    StrInd = num2str(CurInd);
%     switch StrInd(1)
%         case '1'
%             if (numel(StrInd)==1)
%                 Dir = 'zz10';
%             else
%                 Dir = sprintf('zz%s',StrInd(1:2));
%             end
%         otherwise
%             Dir = sprintf('zz%s',StrInd(1));
%     end
    %[StrInd, Dir]
    Dir = 'zz1';
    
    File = sprintf('H5_%s.hdf5',StrInd);
    %cd(Dir)
    Data = h5read(File,'/V');
    %cd ..
    
    % 
    for Idec=1:1:Ndec-1
        D1 = VecDec(Idec);
        D2 = VecDec(Idec+1);
        
        Status(Isum,Idec) = any(Data(:,2)>=D1 & Data(:,2)<D2);
    end
    [Isum, toc]
end

save Status.mat Status
%% prep HDF5 files of strips
RAD = 180./pi;
        
load Status.mat
load SumC.mat

%VecDec = (-90:0.2:90)';
Ndec   = numel(VecDec);
SumDec = zeros(Ndec-1,1);
for Idec=1:1:Ndec-1
    Idec
    tic;
    
    D1 = VecDec(Idec);
    D2 = VecDec(Idec+1);
    
    Flag    = Status(:,Idec);
    IndRead = find(Flag);
    Nread   = numel(IndRead);
    
    [Idec, D1, D2, Nread]
    %Flag = (MinDec>=D1 & MinDec<=D2) | (MaxDec>=D1 & MaxDec<=D2) | (MinDec<=D1 & MaxDec>=D2);
    
    Ind = SumC(Flag,2);
    Nind = numel(Ind);
    DataDec = zeros(0,27);
    for Iread=1:1:Nread
        
        Ind = IndRead(Iread);
        
        StrInd = num2str(Ind);
%         switch StrInd(1)
%             case '1'
%                 if (numel(StrInd)==1)
%                     Dir = 'zz10';
%                 else
%                     Dir = sprintf('zz%s',StrInd(1:2));
%                 end
%             otherwise
%                 Dir = sprintf('zz%s',StrInd(1));
%         end
        Dir = 'zz1';
        %[StrInd, Dir]
        
        File = sprintf('H5_%s.hdf5',StrInd);
        %cd(Dir)
        Data = h5read(File,'/V');
        %cd ..
        
        FlagSrc = Data(:,2)>=D1 & Data(:,2)<D2;
        sum(FlagSrc);
        Data = Data(FlagSrc,:);
        
        All(Iread).D = Data.';
        %DataDec = [DataDec; Data];
    end
    DataDec = [All.D].';
    clear All;
    DataDec = sortrows(DataDec,2);
    SumDec(Idec) = size(DataDec,1);
    
    if (D1<0)
        D1s = 'm';
    else
        D1s = '';
    end
    if (D2<0)
        D2s = 'm';
    else
        D2s = '';
    end
    
    FileW = sprintf('GaiaDRE3_%s%04.1f_%s%04.1f.hdf5',D1s,abs(D1),D2s,abs(D2));
    DataDec(:,1:2) = DataDec(:,1:2)./RAD;
    Size = size(DataDec);
    %h5create(FileW,'/V',Size, 'ChunkSize',[min(Size(1), 1e5), Size(2)]);
    %h5write(FileW,'/V',DataDec);
    
    HDF5.save(DataDec,FileW,'/V');
    'a'
    toc
end


%%
VO.prep.prep_generic_htm


    
