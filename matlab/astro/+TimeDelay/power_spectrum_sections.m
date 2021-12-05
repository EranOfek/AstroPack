function [PS,Section]=power_spectrum_sections(LC,varargin)
% Calculate the power spectrum of unevenly spaced time series with gaps
% Package: +TimeDelay
% Description: Given an unevenly spaced time series, look for sections that
%              do not have gaps larger than MaxGapT days, and MinNep data
%              points. Interpolate each section into an equally spaced
%              timeseries. Calculate the power spectrum of each section and
%              add all the power spectra in phase.
% Input  : - Light curve [Time, Flux, Err]



% [PS,Section]=power_spectrum_sections([LC.Time, LC.LC_Flux]);

InParR = inputParser;
addOptional(InParR,'InterpSampling',1);
addOptional(InParR,'InterpMethod','linear');

addOptional(InParR,'MaxGapT',10);
addOptional(InParR,'MinNep',10);
addOptional(InParR,'MinSectionDuration',100);


addOptional(InParR,'ColT',1);
addOptional(InParR,'ColF',2);
addOptional(InParR,'ColE',3);

parse(InParR,varargin{:});
InPar = InParR.Results;

Time = LC(:,InPar.ColT);
Flux = LC(:,InPar.ColF);
if size(LC,2)>2
    Err  = LC(:,InPar.ColE);
else
    Err = zeros(size(Time));
end

Nt   = numel(Time);

% divide the light curve to contigues parts
DiffTime = diff(Time);

GapInd = find(DiffTime>InPar.MaxGapT);
GapInd = [1; GapInd; Nt];
Ngap   = numel(GapInd);
Isec   = 0;
Section = [];
for Igap=1:1:Ngap-1
    
    SectionInd = (GapInd(Igap):1:GapInd(Igap+1));
    SectionTime = Time(SectionInd);
    if numel(SectionTime)>=InPar.MinNep && range(SectionTime)>=InPar.MinSectionDuration
        % section of times (SectionTime) satisfy criteria
        % number of epochs is >=InPar.MinNep
        % time span >=InPar.MinGapDuration
        
        Isec = Isec + 1;
        % interpolate light curve
        SectionFlux = Flux(SectionInd);
        Section(Isec).Ind  = SectionInd;
        Section(Isec).Time = (min(SectionTime):InPar.InterpSampling:max(SectionTime)).';
        Section(Isec).Flux = interp1(SectionTime, SectionFlux, Section(Isec).Time, InPar.InterpMethod);
        Section(Isec).Err  = median(Err(SectionInd)).*ones(size(Section(Isec).Time));
        Section(Isec).TimeRange = range(Section(Isec).Time);
        Section(Isec).Nt        = numel(Section(Isec).Time);
    end
end

if isempty(Section)
    FT = [];
    PS = [];
else
    
    % chose frequencies for power spectrum based on Timae ranges of all the
    % sections
    MinRange = min([Section.TimeRange]);
    FreqVec = TimeDelay.fft_freq(MinRange,1);
    OmegaVec = 2.*pi.*FreqVec(:).';
    
%     MinFreq  = 0.5./MaxRange;
%     FreqVec  = (-.5./InPar.InterpSampling:MinFreq:0.5./InPar.InterpSampling).';
%     OmegaVec = 2.*pi.*FreqVec(:).';

    % calc the power spectrum for all sections
    Nsec = numel(Section);
    for Isec=1:1:Nsec
        Section(Isec).FluxMin = Section(Isec).Flux(1:MinRange);
        Section(Isec).TimeMin = Section(Isec).Time(1:MinRange);
        %Section(Isec).TimeMin = Section(Isec).TimeMin - min(Section(Isec).TimeMin) +0;
        
        Section(Isec).FT = sum(Section(Isec).FluxMin.*exp(-1i.*OmegaVec.*Section(Isec).TimeMin),1).';
        % ortho normalization
        Section(Isec).FT = Section(Isec).FT./sqrt(MinRange);
        Section(Isec).FreqVec = FreqVec;
        Section(Isec).OmegaVec = OmegaVec;
        
%         Section(Isec).FreqVec = ifftshift(TimeDelay.fft_freq(Section(Isec).Nt)./Section(Isec).Nt);  % used to be TimeRange !!!
% 
%         Section(Isec).PS = fft(Section(Isec).Flux);
%     %     [FT,FreqVec,FT2] = TimeDelay.fft_withphase(Section(Isec).Time,Section(Isec).Flux);
%     %     Section(Isec).PS = FT2;
%     %     
% 
%         %'need to use direct PS in order to sum the PS with the correct phase'
%         %OmegaVec = 2.*pi.*Section(Isec).FreqVec.';
%         %Section(Isec).PS = sum(Section(Isec).Flux.*exp(-1i.*OmegaVec.*Section(Isec).Time),1).';
% 
%         % interpolate all PS to common frequencies
% 
%         Section(Isec).CommonFreqVec = FreqVec;
%         Section(Isec).CommonPS      = interp1(Section(Isec).FreqVec, Section(Isec).PS, Section(Isec).CommonFreqVec, InPar.InterpMethod);
    end


    PS.FreqVec = FreqVec;
    PS.OmegaVec = OmegaVec;
    PS.FT = mean([Section.FT],2);
    PS.PS = abs(PS.FT).^2;
    
%     FT = [FreqVec, sum([Section.CommonPS],2)];
%     % remove NaNs from combined PS
%     FF = ~isnan(FT(:,2));
%     FT = FT(FF,:);
%     PS = FT;
%     PS(:,2) = abs(FT(:,2)).^2;  
end

