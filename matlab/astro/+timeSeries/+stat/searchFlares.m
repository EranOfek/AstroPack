function [FlagAny,Flag,Median,Std] = searchFlares(Mat, Args)
    % Search flares and eclipses in equally spaced time series
    %   Given a roughly evenly spaced matrix of time series (in columns),
    %   search for positive or negative flares in data.
    %   Two methods for flares search are implemented:
    %   1. Top-hat filter + search for flares with ThresholZ sigma abive
    %   median level.
    %   2. Single group of consecutive not-NaN values, among NaN time series.
    %   The non-consectutive NaN is ignored if near the edges of the
    %   series.
    % Input  : - A matrix (Nepoch X Nsrc).
    %          * ...,key,val,... 
    %            'DimEpoch' - Dim of epoch direction. Default is 1.
    %            'MovMeanWin' - A vector containig list of the size of the
    %                   top-hat filter that will be implemented.
    %                   Default is [2 4 8].
    %            'MadType' - Type for tools.math.stat.std_mad
    %                   that is used to calculate the std of the time.
    %                   If empty, use tools.math.stat.rstd.
    %                   series. Default is [].
    %            'ThresholdZ' - Threshold for detection (sigma).
    %                   Default is 8.
    %            'MinNotNaN' - For the 2nd search method the number of
    %                   sucessive NaNs must be larger than this value.
    %                   Default is 1 (i.e., 2 not NaNs are required).
    %            'MaxNotNaN' - For the 2nd search method the number of
    %                   sucessive NaNs must be samller than this value.
    %                   Default is 11.
    %
    % Output : - A vector of logical flag indictaing, for each source, if a
    %            flare was detected.
    %          - A structure with logical flags indicating if a source was
    %            found as flaring in each of the channels.
    %          - Vector of median value for each source.
    %          - Vector of std value for each source.
    % Author : Eran Ofek (2024 Feb) 
    % Example: Mat = randn(40,4); Mat(15:20,2)=6; Mat(1:10,3)=NaN; Mat(15:end,3) = NaN;
    %          Mat(:,4) = NaN; Mat(15,4) = 100; Mat(25,4)=100;
    %          F=timeSeries.stat.searchFlares(Mat)

    arguments
        Mat
        Args.DimEpoch          = 1;
        Args.MovMeanWin        = [2 4 8];
        Args.MadType           = [];
        
        Args.ThresholdZ        = 8;
        
        Args.MinNotNaN         = 1;
        Args.MaxNotNaN         = 11;
        
    end

    if Args.DimEpoch==2
        Mat = Mat.';
    end
    
    [Nep, Nsrc] = size(Mat);
    
    
    Nfilt       = numel(Args.MovMeanWin);
    
    IsNN        = ~isnan(Mat);
    
    Median      = median(Mat, 1, 'omitnan');
    if isempty(Args.MadType)
        Std         = tools.math.stat.rstd(Mat, 1);
    else
        Std         = tools.math.stat.std_mad(Mat, Args.MadType, 1);
    end
    
    
    Flag.FlagZ  = false(Nsrc, Nfilt);
    for Ifilt=1:1:Nfilt
    
        % Search #1 - top-hat matched filter
        FilteredMag = movmean(Mat, Args.MovMeanWin(Ifilt), 1, 'includenan');
    
        StdF        = Std./sqrt(Args.MovMeanWin(Ifilt));
        FiltZ       = abs(FilteredMag - Median)./StdF;
        MaxZ        = max(FiltZ,[],1);
    
        Flag.FlagZ(:,Ifilt)  = MaxZ>Args.ThresholdZ;
        
       
    end
    
    % search #2 - sucessive detections above NaN
    NumNN = sum(IsNN, 1);  % number of not NaN
    Flag.FlagN = false(Nsrc,1);
    for Isrc=1:1:Nsrc
        IndNN = find(IsNN(:,Isrc));
        % prepare list of consecutive detections
        ConsNN = tools.find.findListsOfConsecutiveTrue(IsNN(:,Isrc));
        % select only sources which have one event with length in the range
        % of MinNotNaN to MaxNotNaN
        if numel(ConsNN)==1 && numel(ConsNN{1})>Args.MinNotNaN && numel(ConsNN{1})<Args.MaxNotNaN
            % only one peak was found
            Flag.FlagN(Isrc) = true;
        end
    end
        
    FlagAny = any(Flag.FlagZ,2) | Flag.FlagN;
    
end
