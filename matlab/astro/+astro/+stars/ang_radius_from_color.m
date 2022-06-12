function [AngRad,Temp]=ang_radius_from_color(MagMat,FamilyCell,BandCell,SystemCell, Ebv, R)
% Estimate angular radius and color temperature from a set of magnitudes
% Package: +astro.stars
% Input  : - A matrix of magnitudes in several bands. Source per line.
%          - A cell array of filter family names for the columns in the
%            marix of magnitudes.
%          - A cell array of band names.
%          - A cell array of mag sys. types.
%          - E(B-V) extinction [mag]. Default is 0.
%          - R. Default is 3.08.
% Output : - ANgular radius ["]
%          - Temperature [K].
% Example: 
%MagMat=[15.886 16.465 15.168];                                                     
%FamilyCell={'GAIA','GAIA','GAIA'};
%BandCell={'Bp','Rp','G'};
%SystemCell={'Vega','Vega','Vega'};
% [AR,T]=astro.stars.ang_radius_from_color(MagMat,FamilyCell,BandCell,SystemCell)
% % effect of extinction on angular size
% BandCell={'g','r'};                                                                 
% SystemCell={'AB','AB'};                                                             
% FamilyCell={'SDSS','SDSS'};                                                         
% rVec=(0:0.1:2).';
% Evec=(0:0.1:1.5).';
% R=3.08;
% for Ir=1:1:numel(rVec)
%     for Ie=1:1:numel(Evec)
%         M = [0, rVec(Ir)];
%         [AR(Ir,Ie),T]=astro.stars.ang_radius_from_color(M,FamilyCell,BandCell,SystemCell,Evec(Ie), R);
%     end
% end
% An=AR./AR(:,1);   % normalize size by zeros extinction
% Hs=surface(Evec,rVec,An); %  contour(Evec,rVec,An,[0,0.3,1,3,10].');
% Hs.FaceAlpha=0.5; contour(Evec,rVec,An,1)
% H = colorbar; H.Label.String='Radius increase factor'; H.Label.Interpreter='latex'; H.Label.FontSize=16
% H=ylabel('$g-r$ [mag]'); H.FontSize=18; H.Interpreter='latex';
% H = xlabel('E$_{B-V}$ [mag]'); H.FontSize=18; H.Interpreter='latex';

arguments
    MagMat
    FamilyCell
    BandCell
    SystemCell
    Ebv             = 0;
    R               = 3.08;
end

RAD = 180./pi;

% apply extinction
if Ebv~=0
    A = astro.spec.extinction(Ebv,FamilyCell, BandCell, R);
    MagMat = MagMat(:).' + A(1,:);
end

T  = logspace(log10(3000),log10(30000),100)';
NT = numel(T);

Nband = numel(BandCell);
MagInBand = zeros(NT,Nband);
for Iband=1:1:Nband
    MagInBand(:,Iband) = astro.spec.blackbody_mag_c(T,FamilyCell{Iband},BandCell{Iband},SystemCell{Iband},constant.SunR,1);
end    
BaseAngeRad = constant.SunR./constant.pc .*RAD.*3600;

Nsrc = size(MagMat,1);
AngRad = zeros(Nsrc,1);
Temp   = zeros(Nsrc,1);
for Isrc=1:1:Nsrc
    RMS = std(MagMat(Isrc,:) - MagInBand,[],2);
    [~,Irms] = min(RMS);
    
    MeanDiff = nanmean(MagMat(Isrc,:) - MagInBand(Irms,:));
    
    AngRad(Isrc) = BaseAngeRad.*10.^(-0.2.*MeanDiff);
    Temp(Isrc)   = T(Irms);
end