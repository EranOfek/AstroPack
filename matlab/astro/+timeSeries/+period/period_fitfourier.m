function [PS,Par,Stat,H]=period_fitfourier(Data,FreqVec,Args)
% Fit a Fourier series to a time series
% Package: timeSeries.period
% Description: Fit a polynomial and fourier series as a function of
%              frequency to a time series.
%              The fitted series is:
%              Y = A_ + A_*T + A_*T^3,... A_*sin(2*pi*f*T*H(1)) +
%                                         A_*sin(2*pi*f*T*H(2)) + ...
%                                         A_*cos(2*pi*f*T*H(1)) +
%                                         A_*cos(2*pi*f*T*H(2)) + ...
%                                         Const_1*Flag1 + ...
%              Here Const is a fitted additive constant to selected data
%              points sorted by index.
%              See period.m for a more flexiable function.
% Input  : - Two column matrix containing the time series
%            [Time, measurment] or [Time, measurment, error].
%          - Frequency range and interval in which to calculate the
%            power spectrum.
%            This is a column vector of frequencies at which to
%            calculate the power spectrum.
%          * ...,key,val,...
%            'Harmon' - Row vector of Harmonies to fit, e.g. [1 2 3].
%                   Default is [1 2].
%            'PolyN' - Row vector of polynomials degree to fit.
%                   If empty, do not add polynomials. Default is [0].
%            'Const' - This parameter allows to fit a different additive constant
%                   for each group of data. The grousp are specifoed by
%                   indexes of 1 to N. If empty, then do not fit such a
%                   constant. Default is [].
%            'Method' - Fitting method. 'lscov'|'slash'.
%                   'slash' is twice as fast but do not return errors, and
%                   do not use weights.
%                   Default is 'lscov'
%
% Output : - Two columns matrix of the power spectrum equivalent
%            [frequency, amplitude], where amplitude is the total amplitude
%            of all the harmonies at a given frequency.
%          - Structure containing the best fit parameters and errors
%            for each frequency.
%            [1 T T^2 T^3,... sin(2*pi*f*H*T), ..., cos(2*pi*f*H*T),...]
%          - Structure with the following fields:
%            .Npar
%            .Dof
%            .Chi2
%            .RMS
%            .Ymodel
%          - The full design matrix.
% Tested : Matlab 7.11
% Author : Eran Ofek (May 2011)
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: N=100; T = rand(N,1).*100; Data = [T, sin(2.*pi.*0.31.*T)+randn(N,1).*0.1];
%          Const = 1 + (T>50); Data(:,2)=Data(:,2)+Const;
%          FreqVec = timeSeries.period.getFreq(T);
%          [PS,Par,Stat]=timeSeries.period.period_fitfourier(Data,FreqVec,'PolyN',[],'Const',Const);
%          plot(FreqVec,Stat.Chi2)
%          plot(FreqVec,PS)
%----------------------------------------------------------------------------

arguments
    Data
    FreqVec
    Args.Harmon = [1 2];
    Args.PolyN  = [0];
    Args.Const  = [];
    Args.Method = 'lscov';
end

Args.PolyN = Args.PolyN(:).'; % make a row vector

Col.T = 1;
Col.M = 2;
Col.E = 3;

T       = Data(:,Col.T);
N       = numel(T);
Nf      = numel(FreqVec);

% construct the design matrix:


if numel(Args.Const)==N
    % apply a different additive constant for each group specified by running index
    % in Args.Const

    MaxInd = max(Args.Const);
    Hconst = zeros(N,MaxInd);
    for Imi=1:1:MaxInd
        Hconst(Args.Const==Imi, Imi) = 1;
    end
else
    MaxInd = 0;
    Hconst = [];
end


Hpoly = T.^(Args.PolyN);

%for Ip=1:numel(0:1:Args.PolyN
%   Hpoly = [Hpoly, T.^Ip];
%end

if (size(Data,2)==2)
   Data = [Data, ones(N,1)];
end

InvVar = 1./Data(:,Col.E).^2;   % inverse variance

Stat.Npar = numel(Args.PolyN) + length(Args.Harmon).*2 + MaxInd;
Stat.Dof  = N - Stat.Npar;

% fit harmonies for each frequency
Stat.Chi2 = zeros(Nf,1);
Stat.RMS  = zeros(Nf,1);
Par.Par   = zeros(Nf,Stat.Npar);
Par.Err   = zeros(Nf,Stat.Npar);

for FreqInd=1:1:Nf
   Hharm = [sin(2.*pi.*FreqVec(FreqInd).*(T.*Args.Harmon)),...
            cos(2.*pi.*FreqVec(FreqInd).*(T.*Args.Harmon))];

   H = [Hpoly, Hharm, Hconst];
   switch lower(Args.Method)
       case 'slash'
           P = H\Data(:,Col.M);
           E = nan(size(P));
       case 'lscov'
           [P,E] = lscov(H,Data(:,Col.M),InvVar);
       otherwise
           error('Unknown Method option');
   end
   Stat.Ymodel        = H*P;
   Resid              = Data(:,2) - Stat.Ymodel;
   Stat.Chi2(FreqInd) = sum((Resid./Data(:,Col.E)).^2);
   Stat.RMS(FreqInd)  = std(Resid);
   

   Par.Par(FreqInd,:) = P.';
   Par.Err(FreqInd,:) = E.';
end

% amplitude normalization
PS = sqrt(sum(Par.Par(:,Args.PolyN+1:end).^2,2));

