function getGRB_cat(FileName)
    % get and read the Swift online GRB catalog
   
    %%
    % query from:
    % https://swift.gsfc.nasa.gov/archive/grb_table/
    
    % get https://swift.gsfc.nasa.gov/archive/grb_table/tmp/grb_table_1656573191.txt
    FileName = 'grb_table_1656573191.txt';
    S = io.files.readtable1(FileName, 'DurationType','text');
    
    % with columns
    %Col
    Col={'GRB','JD','BAT_RA','BAT_Dec','BAT_ErrPos','BAT_T90','BAT_Fluence_15_150keV','BAT_FluenceErr','BAT_PeakFlux1s','BAT_PeakFlux1sErr','BAT_PhotonIndex',...
        'BAT_PhotonIndexErr','XRT_RA','XRT_Dec','XRT_ErrPos','XRT_Time2firstObs','XRT_earlyFlux','XRT_11hr_Flux','XRT_24hr_Flux','XRT_TemporalIndex',...
        'XRT_SpectralIndex','XRT_NH','UVOT_RA','UVOT_Dec','UVOT_ErrPos','UVOT_Time2firstObs','UVOT_Vmag','z'};
    ColUnits = {'','JD','deg','deg','arcmin','s','10^-7 erg/cm^2','10^-7 erg/cm^2','ph/cm^2/s','ph/cm^2/s','','','deg','deg','arcsec','s',...
        '10^-11 erg/cm^2/s','10^-11 erg/cm^2/s','10^-11 erg/cm^2/s','','','10^21 cm^-2','deg','deg','arcsec','s','mag',''};
    
    S.Properties.VariableNames = Col; 
    S.Properties.VariableUnits = ColUnits; 
    
    
    Ngrb = size(S,1);
    
    % calc JD
    Frac = nan(Ngrb,1);
    for I=1:1:Ngrb
        try
            Frac(I)=celestial.coo.convertdms(S.JD(I),'SH','f');
        end
    end
    JD = nan(Ngrb,1);
    for I=1:1:Ngrb
        Year = str2double(S.GRB{I}(1:2)) + 2000;
        Month = str2double(S.GRB{I}(3:4));
        Day   = str2double(S.GRB{I}(5:6));
        JD(I) = celestial.time.julday([Day Month Year Frac(I)]);
    end
    
    % seperated photon index and model
    PhotonIndex = nan(Ngrb,1);
    PhotonModel = cell(Ngrb,1);
    for I=1:1:Ngrb
        if ~strcmp(S.BAT_PhotonIndex{I},'n/a')
            SpModel = split(S.BAT_PhotonIndex{I},',');
            PhotonIndex(I) = str2double(SpModel{1});
            if numel(SpModel)>1
                PhotonModel{I} = SpModel{2};
            end
        end
    end
    
    % UVOT coo
    U_RA  = nan(Ngrb,1);
    U_Dec = nan(Ngrb,1);
    U_Err = nan(Ngrb,1);
    for I=1:1:Ngrb
        if ~strcmp(S.UVOT_RA{I},'n/a')
            U_RA(I) = celestial.coo.convertdms(S.UVOT_RA{I},'SH','d');
        end
        if ~strcmp(S.UVOT_Dec{I},'n/a')
            if ~(strcmp(S.UVOT_Dec{I}(1),'-') || strcmp(S.UVOT_Dec{I}(1),'+'))
                S.UVOT_Dec{I} = ['+',S.UVOT_Dec{I}];
            end
            U_Dec(I) = celestial.coo.convertdms(S.UVOT_Dec{I},'SD','d');
        end
        if ~strcmp(S.UVOT_ErrPos{I},'n/a')
            U_Err(I) = str2double(S.UVOT_ErrPos{I});
        end
    end
    
    % XRT coo
    X_RA  = nan(Ngrb,1);
    X_Dec = nan(Ngrb,1);
    for I=1:1:Ngrb
        if ~strcmp(S.XRT_RA{I},'n/a')
            S.XRT_RA{I} = regexprep(S.XRT_RA{I},'\s',':');
            X_RA(I) = celestial.coo.convertdms(S.XRT_RA{I},'SH','d');
        end
        if ~strcmp(S.XRT_Dec{I},'n/a')
            if ~(strcmp(S.XRT_Dec{I}(1),'-') || strcmp(S.XRT_Dec{I}(1),'+'))
                S.XRT_Dec{I} = ['+',S.XRT_Dec{I}];
            end
            S.XRT_Dec{I} = regexprep(S.XRT_Dec{I},'\s',':');
            X_Dec(I) = celestial.coo.convertdms(S.XRT_Dec{I},'SD','d');
        end
        
    end
    
    
    
    % XRT_earlyFlux
    XRT_earlyFlux = nan(Ngrb,1);
    for I=1:1:Ngrb
        if ~strcmp(S.XRT_earlyFlux{I},'n/a')
            XRT_earlyFlux(I) = str2double(S.XRT_earlyFlux{I});
        end
    end
    
    % UVOT_Vmag
    UVOT_Vmag = nan(Ngrb,1);
    for I=1:1:Ngrb
        if ~strcmp(S.UVOT_Vmag{I},'n/a')
            if contains(S.UVOT_Vmag{I}, '=')
                UVOT_Vmag(I) = str2double(S.UVOT_Vmag{I}(3:end));
            end
            if contains(S.UVOT_Vmag{I}, '>')
                UVOT_Vmag(I) = -1.*str2double(S.UVOT_Vmag{I}(3:end));
            end
            
        end
    end
    
    % z
    Z = nan(Ngrb,1);
    for I=1:1:Ngrb
        Tmp = regexp(S.z{I}, '(?<redshift>\d\.\d+)\s.*','names');
        if ~isempty(Tmp)
            Z(I) = str2double(Tmp.redshift);
        end
    end
        
    %
    
    Table = S;
    Table.JD  = JD;
    Table.BAT_PhotonIndex = PhotonIndex(:);
    Table.BAT_PhotonModel = PhotonModel;
    Table.UVOT_RA     = U_RA;
    Table.UVOT_Dec    = U_Dec;
    Table.UVOT_ErrPos = U_Err;
    Table.XRT_RA     = X_RA;
    Table.XRT_Dec    = X_Dec;
    Table.XRT_earlyFlux = XRT_earlyFlux;
    Table.UVOT_Vmag     = UVOT_Vmag;
    Table.z             = Z;
    
    %%
    
    SwiftGRB = AstroCatalog;
    SwiftGRB.Catalog = Table;
    SwiftGRB.Name = 'Swift GRB catalog - 30-06-2022';
    
    
    
end