function measure_dimm(Cube,varargin)
% 
% Example: 
% IC = imCl.fits2imCl('D*20200816-06*.fit');
% Cube=images2cube(IC);

%List = io.files.filelist('Light*.fit');
%AI=AstroImage(List(1:100));
%Cube=imProc.image.images2cube(AI);



RAD = 180./pi;

InPar = inputParser;
addOptional(InPar,'z',58);  % zenith distance [deg] 
addOptional(InPar,'Lambda',5500); %6050);  % wavelength [ang] 
addOptional(InPar,'HoleDiam',5.3); %8.7);  % D [cm]
addOptional(InPar,'HoleDist',11.5); %27.5);  % B [cm]
addOptional(InPar,'PixScale',0.604); % 0.65 %  ["/pix]
addOptional(InPar,'ColSN','SN_1'); 
addOptional(InPar,'ColX','X'); 
addOptional(InPar,'ColY','Y'); 
addOptional(InPar,'MinSN',20); 
addOptional(InPar,'Verbose',true); 

parse(InPar,varargin{:});
InPar = InPar.Results;

z         = InPar.z./RAD;
Lambda    = InPar.Lambda.*1e-8;  % cm
HoleDiam  = InPar.HoleDiam;  % cm
HoleDist  = InPar.HoleDist;  % cm
b         = HoleDist./HoleDiam;


Nim = size(Cube,3);

D  = nan(Nim,1);
PA = nan(Nim,1);
for Iim=1:1:Nim
    if InPar.Verbose
        fprintf('Analyzing image %d out of %d\n',Iim,Nim);
    end
    [Cat(Iim),~,~]=imUtil.sources.find_measure_sources(single(Cube(:,:,Iim)),'PsfFunPar',{15});
    
    XY = getCol(Cat(Iim),{'XPEAK','YPEAK'});
    SN = getCol(Cat(Iim),'SN_1');
   
    % select stars above S/N threshold
    Flag  = SN>InPar.MinSN;
    Cat(Iim).Catalog = Cat(Iim).Catalog(Flag,:);
   
    Nsrc = size(Cat(Iim).Catalog,1);
    
    XY = XY(Flag,:);
    
    if Nsrc==2
        X = XY(:,1);
        Y = XY(:,2);
        
        D(Iim)  = sqrt(diff(X).^2 + diff(Y).^2);
        PA(Iim) = atan2(diff(Y), diff(X));
        
    end
end


Theta = nanmean(PA);
R = [cos(Theta), -sin(Theta); sin(Theta), cos(Theta)];

DX = nan(Nim,1);
DY = nan(Nim,1);

for Iim=1:1:Nim
    Nsrc = size(Cat(Iim).Catalog,1);
    if Nsrc==2
        XY = getCol(Cat(Iim),{'XPEAK','YPEAK'});
        X = XY(:,1);
        Y = XY(:,2);
        
        RotXY = (R.'*[X,Y].').';
        DX(Iim) = diff(RotXY(:,1));
        DY(Iim) = diff(RotXY(:,2));
    end
end

St = nanstd(DY);
Sl = nanstd(DX);


St = St.*InPar.PixScale./(RAD.*3600);
Sl = Sl.*InPar.PixScale./(RAD.*3600);


Kl = 0.364.*(1 - 0.532.*b.^(-1./3) - 0.024.*b.^(-7./3));
Kt = 0.364.*(1 - 0.798.*b.^(-1./3) + 0.018.*b.^(-7./3));

%el = 0.98.*cos(z).^-0.6.*Lambda./r0l
el = 0.98.*(cos(z)).^-0.6 .* (HoleDiam./Lambda).^0.2 .* (Sl.^2./Kl).^0.6;
et = 0.98.*(cos(z)).^-0.6 .* (HoleDiam./Lambda).^0.2 .* (St.^2./Kt).^0.6;

r0l = 0.98.*(cos(z)).^-0.6 .*Lambda./el
SeeingEl = el.*RAD.*3600
SeeingEt = et.*RAD.*3600


