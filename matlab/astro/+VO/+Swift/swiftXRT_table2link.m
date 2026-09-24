function [DataURL] = swiftXRT_table2link(T,AddPathXRT)
    % Convert Swift-XRT table to data link (aux fun)
    % Input  : - Table geerated by VO.Swift.swiftXRT_obs
    % Output : - String array of links to the obsid in each line in the
    %            input table.
    % Author : ChatGPT + Eran Ofek (2025 Nov) 
    % Example: 

    
    Nt = size(T,1);
    DataURL = strings(Nt,1);
    for It=1:1:Nt
        Row   = T(It,:);                                % your selected line
        Obsid = string(Row.obsid);                     % or string(row.ObsID_str)
        %St    = datetime(string(Row.start_time),'InputFormat','yyyy-MM-dd''T''HH:mm:ss','TimeZone','UTC');
        St = local_to_datetime(Row.start_time);   % robust ISO/MJD/JD → datetime(UTC)

        Yyyy_mm = sprintf('%04d_%02d', year(St), month(St));
        
        S3    = "s3://nasa-heasarc/swift/data/obs/" + Yyyy_mm + "/" + Obsid + "/";
        DataURL(It) = "https://heasarc.gsfc.nasa.gov/FTP/swift/data/obs/" + Yyyy_mm + "/" + Obsid + "/";
        %disp(S3), disp(DataURL)
    end

    if AddPathXRT
        DataURL = DataURL + "xrt/event/";
    end


end



function Dt = local_to_datetime(V)
% Robust converter: ISO string(s) or JD/MJD number(s) → datetime(UTC)
    if isa(V,'datetime')
        Dt = V; 
        if isempty(Dt.TimeZone), Dt.TimeZone = 'UTC'; end
        return
    end

    if isstring(V) || ischar(V)
        S = string(V);
        % Try ISO-8601 with milliseconds, then seconds
        try
            Dt = datetime(S,'InputFormat','yyyy-MM-dd''T''HH:mm:ss.SSS','TimeZone','UTC');
            return
        catch, end
        try
            Dt = datetime(S,'InputFormat','yyyy-MM-dd''T''HH:mm:ss','TimeZone','UTC');
            return
        catch, end
        % Maybe numeric-in-string (JD/MJD)
        D = str2double(S);
        if all(~isnan(D))
            Dt = local_from_mjd_or_jd(D);
            return
        end
        % Fallback
        Dt = NaT(size(S)); Dt.TimeZone = 'UTC';
        return
    end

    if isnumeric(V)
        Dt = local_from_mjd_or_jd(V);
        return
    end

    % Final fallback
    Dt = NaT(size(V)); Dt.TimeZone = 'UTC';
end

function Dt = local_from_mjd_or_jd(X)
% Accepts scalar or array. Treat values > 2.4e6 as JD, else MJD.
    Epoch = datetime(1858,11,17,'TimeZone','UTC');  % MJD epoch
    X = double(X);
    IsJD = X > 2400000;                % JD if huge; else MJD
    MJD = X; 
    MJD(IsJD) = X(IsJD) - 2400000.5;   % JD → MJD
    Dt = Epoch + days(MJD);
end
