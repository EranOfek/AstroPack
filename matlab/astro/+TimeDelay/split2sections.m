function [Section]=split2sections(LC,varargin)
% Split a time series to sections in which the gaps have a maximum.
% Package: +TimeDelay
% Description: Given an unevenly spaced time series, look for sections that
%              do not have gaps larger than MaxGapT days, and MinNep data
%              points. Interpolate each section into an equally spaced
%              timeseries. Calculate the power spectrum of each section and
%              add all the power spectra in phase.
% Input  : - Light curve [Time, Flux, Err]



% [Section]=TimeDelay.spli2sections([LC.Time, LC.LC_Flux]);

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
GapInd = [0; GapInd; Nt];
Ngap   = numel(GapInd);
Isec   = 0;
Section = [];
for Igap=1:1:Ngap-1
    IndStart = GapInd(Igap)+1;
    IndEnd   = GapInd(Igap+1);
    SectionInd = (IndStart:1:IndEnd);
    
    %SectionInd = (GapInd(Igap):1:GapInd(Igap+1));
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
