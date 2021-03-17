function measure_dimm(Cube,varargin)
% 
% Example: 
% IC = imCl.fits2imCl('D*20200816-06*.fit');
% Cube=images2cube(IC);


RAD = 180./pi;

InPar = inputParser;
addOptional(InPar,'z',58);  % zenith distance [deg] 
addOptional(InPar,'Lambda',6050);  % wavelength [ang] 
addOptional(InPar,'HoleDiam',8.7);  % D [cm]
addOptional(InPar,'HoleDist',27.5);  % B [cm]
addOptional(InPar,'PixScale',0.65);  %  ["/pix]
addOptional(InPar,'ColSN','SN_1'); 
addOptional(InPar,'ColX','X'); 
addOptional(InPar,'ColY','Y'); 
addOptional(InPar,'MinSN',10); 
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
    [Cat(Iim),~,~]=imUtil.sources.find_sources(single(Cube(:,:,Iim)),'PsfFunPar',{10});
    
    ColSN = find(strcmp(Cat(Iim).ColCell,InPar.ColSN));
    ColX  = find(strcmp(Cat(Iim).ColCell,InPar.ColX));
    ColY  = find(strcmp(Cat(Iim).ColCell,InPar.ColY));
    
    % select stars above S/N threshold
    Flag  = Cat(Iim).Cat(:,ColSN)>InPar.MinSN;
    Cat(Iim).Cat = Cat(Iim).Cat(Flag,:);
   
    Nsrc = size(Cat(Iim).Cat,1);
    
    if Nsrc==2
        X = Cat(Iim).Cat(:,ColX);
        Y = Cat(Iim).Cat(:,ColY);
        
        D(Iim)  = sqrt(diff(X).^2 + diff(Y).^2);
        PA(Iim) = atan2(diff(Y), diff(X));
        
    end
end


Theta = nanmean(PA);
R = [cos(Theta), -sin(Theta); sin(Theta), cos(Theta)];

DX = nan(Nim,1);
DY = nan(Nim,1);

for Iim=1:1:Nim
    Nsrc = size(Cat(Iim).Cat,1);
    if Nsrc==2
        X = Cat(Iim).Cat(:,ColX);
        Y = Cat(Iim).Cat(:,ColY);
        
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


