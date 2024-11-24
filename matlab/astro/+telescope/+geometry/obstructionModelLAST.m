function [AllData] = obstructionModelLAST(Itel, Args)
    % Construct and plot enclosure and telescope obstraction model for LAST
    % Input  : - Telescope index. Default is 3.
    %          * ...,key,val,... 
    %            See code for options.
    % Output : - 
    % Author : Eran Ofek (2022 Nov) 
    % Example: R=telescope.geometry.obstructionModelLAST
    %          R=telescope.geometry.obstructionModelLAST('Step',0.01)


    arguments
        Itel             = 3;
        Args.AllData     = [];
        Args.Buffer      = 3.5;   % Buffer above horizon
        Args.ht          = 1.2;   % Height of constant north/east/south walls
        Args.hwN         = 1.2;        
        Args.hwE         = 1.2;
        Args.hwS         = 1.2;
        Args.hwW         = 2.7;
    
        Args.r1          = 0.63;  % sqrt(0.335.^2 + 0.53.^2)
        Args.r2          = 0.84;  % sqrt(0.335.^2 + 0.53.^2 + 0.55.^2)
        Args.BW          = 5.32;  % building internal width
        Args.BL          = 10.8;  %10.54;  % Building length (free)
        Args.BLw         = 12.2;
        Args.NtelRow     = 6;
        Args.NtelCol     = 2;
        Args.Step        = 0.5;   % approx az resolution

        Args.BuildingRotation = 5.7;  % [deg]

        Args.Plot              = 1;
        Args.CombPlot logical  = true;

        Args.ObstractionFile   = []; %'MountObs3111.txt';  % write obstraction [Az,Alt] to this file. If empty, no file is written.
    end
    
      
    RAD = 180./pi;
    
    AllData = Args.AllData;

    Buffer = Args.Buffer;   %3.5;  % Buffer above horizon
    ht = Args.ht;  %1.2;
    hwN = Args.hwN; % 1.2;
    hwE = Args.hwE; % 1.2;
    hwS = Args.hwS; % 1.2;
    hwW = Args.hwW; %2.7;
    r1 = Args.r1; %0.63;  % sqrt(0.335.^2 + 0.53.^2)
    r2 = Args.r2; %0.84;  % sqrt(0.335.^2 + 0.53.^2 + 0.55.^2)
    
    
    BW = Args.BW; %5.32;  % building internal width
    BL = Args.BL; %10.8;  %10.54;  % Building length (free)
    BLw = Args.BLw; %12.2;
    
    
    NtelRow = Args.NtelRow; %6;
    NtelCol = Args.NtelCol; %2;
    
    Step = Args.Step; %0.5; %0.01;

    if isempty(Args.AllData)
        ItelA = 1;
    else
        ItelA = Itel;
    end

    % The effective r2 for a telescope that is alpha deg from full alignment
    if 1==1
        alpha = 15; %(0:1:50);
        D=sqrt(r2.^2-r1.^2);
        g=atand(D./r1);
        r2m = sqrt(r2.^2./(1+tand(g-alpha).^2));
    end
    
    %r2 = 0.7;
    
    
    % X is maesured East to West
    % Y is measured North to South
    
    
    Xall = (BL./NtelRow./2: BL./NtelRow : BL);
    %Xall = (0.88:2.0:10.9);
    %Xall = (1.06:2.12:10.6);
    %Xall = (1.06:2.3:10.6);
    Yall = (BW./NtelCol./2: BW./NtelCol : BW).';
    Narray = size(Xall.*Yall)
    
    Xall = Xall.*ones(Narray);
    Yall = Yall.*ones(Narray);
    
    Yall(1,:) = Yall(1,:);%+ 0.05;
    Yall(2,:) = Yall(2,:);%- 0.05;
    
    
    X = Xall(Itel);
    Y = Yall(Itel);
    
    
    
    XwallN = (0:Step:BLw).';
    YwallN = zeros(size(XwallN));
    XwallS = (0:Step:BLw).';
    YwallS = BW + zeros(size(XwallS));
    
    YwallE = (0:Step:BW).';
    XwallE = zeros(size(YwallE));
    YwallW = (0:Step:BW).';
    %XwallW = BL + zeros(size(YwallE));
    XwallW = BLw + zeros(size(YwallE));
    
    
    Az1 = atan2d(Y-0,X-0);  % Az of NE corner
    Az2 = atan2d(Y-BW,X-0);  % Az of SE cornner
    Az3 = atan2d(Y-BW,X-BLw);  % Az of SW corner
    Az4 = atan2d(Y-0,X-BLw);  % Az of NW corner
    
    % N: Az>Az1 & Az<Az4
    AzN = atan2d(Y-YwallN,X-XwallN);
    AzE = atan2d(Y-YwallE,X-XwallE);
    AzS = atan2d(Y-YwallS,X-XwallS);
    AzW = atan2d(Y-YwallW,X-XwallW);
    Az  = [AzN; AzE; AzS; AzW];
    RealAz = (90-Args.BuildingRotation)-Az;
    RealAz = mod(RealAz,360);
    
    hw  = [hwN.*ones(size(AzN)); hwE.*ones(size(AzE)); hwS.*ones(size(AzS)); hwW.*ones(size(AzW))];
    
    dwN = sqrt((X-XwallN).^2 + (Y-YwallN).^2);
    dwE = sqrt((X-XwallE).^2 + (Y-YwallE).^2);
    dwS = sqrt((X-XwallS).^2 + (Y-YwallS).^2);
    dwW = sqrt((X-XwallW).^2 + (Y-YwallW).^2);
    
    dw = [dwN; dwE; dwS; dwW];
    
    gamma=wall_alt(dw,ht,hw,r1,Az);
    
    
    
    % telescope 2 telescope
    
    % X is maesured East to West
    % Y is measured North to South
    %X = [0.88];  % 10.6./6./2  
    %Y = [1.335]; % 5.34./2./2
    
    AllData(ItelA).Enclosure = gamma+Buffer;
    
    if Args.Plot==1
        plot(RealAz,gamma+Buffer,'b.')
        hold on;
    end
    
    
    Nmount = numel(Xall);
    Chi_r1 = nan(numel(Az),Nmount);
    Chi_r2 = nan(numel(Az),Nmount);
    Chi_r2m = nan(numel(Az),Nmount);
    
    for Imount=1:1:Nmount
        if Imount~=Itel
            X2 = Xall(Imount);
            Y2 = Yall(Imount);
    
            [AltTT_r1,MaxChi_r1]=tel2tel_allalt(Az,X,Y,X2,Y2,ht,r1);
            [AltTT_r2,MaxChi_r2]=tel2tel_allalt(Az,X,Y,X2,Y2,ht,r2);
            [AltTT_r2m,MaxChi_r2m]=tel2tel_allalt(Az,X,Y,X2,Y2,ht,r2m);
    
            Chi_r1(:,Imount) = AltTT_r1; %MaxChi_r1;
            Chi_r2(:,Imount) = AltTT_r2; %MaxChi_r2;
            Chi_r2m(:,Imount) = AltTT_r2m; %MaxChi_r2;
            
            
            
            if Args.Plot==1
                plot(RealAz,AltTT_r1+Buffer,'k.');
                hold on;
                plot(RealAz,AltTT_r2+Buffer,'r.');
                plot(RealAz,AltTT_r2m+Buffer,'g.');
            end
        end
    end
    
    Max_r1 = max([Chi_r1,gamma],[],2);
    Max_r2 = max([Chi_r2,gamma],[],2);
    Max_r2m = max([Chi_r2m,gamma],[],2);
    
    AllData(ItelA).RealAz           = RealAz;
    AllData(ItelA).TT_Aligned       = Max_r1  + Buffer;
    AllData(ItelA).TT_NotAligned    = Max_r2  + Buffer;
    AllData(ItelA).TT_Aligned15deg  = Max_r2m + Buffer;
    
    % calculate the fraction of sky area accessible above 30 deg
    Nrand = 1e5;
    TH = 2*pi*rand(1,Nrand);
    PH = asin(-1+2*rand(1,Nrand));
    Flag = PH>(30./RAD);
    AzRand   = TH(Flag).*RAD - 180;
    AltRand   = PH(Flag).*RAD;
    Nrand = numel(AltRand);
    Table = [Az,Max_r2];
    [~,UI] = unique(Table(:,1));
    Table  = Table(UI,:);
    Ntable = size(Table,1);
    AltLimitRand = interp1(Table(:,1),Table(:,2),AzRand) + Buffer;
    AccessibleSkyFrac = sum(AltRand>AltLimitRand)./Nrand
    
    
    if Args.Plot==2
        plot(RealAz,Max_r2+Buffer,'.')
        hold on
    end
    
    if Args.Plot==1
        legend('Dome','Aligned','NotAligned','15deg aligned');
    end
    
    H = xlabel('Az [deg]');
    H.FontSize = 18;
    H.Interpreter = 'latex';
    H = ylabel('Alt [deg]');
    H.FontSize = 18;
    H.Interpreter = 'latex';
    H=gca;
    H.XLim = [0 360];
    
    
    FileName = sprintf('%s_%02d.jpg','ObscurationMode',Itel);
    %print(FileName,'-djpeg90');
    
    
    if Args.CombPlot && ~isempty(Args.AllData)
        % combined plot
        Min = inf(size(AllData(Itel).Enclosure));
        for Itel=1:1:12
            AllData(Itel).Max = max([AllData(Itel).Enclosure, AllData(Itel).TT_NotAligned],[],2);
            if Itel>1
                Min = min(Min, AllData(Itel).Max);
            end
        end
        
        [SAz, SI] = sort(AllData(1).RealAz);
        SAlt = Min(SI);
        
        
        Nrand = 1e6;
        TH = 2*pi*rand(1,Nrand);
        PH = asin(-1+2*rand(1,Nrand));
        Flag = PH>(30./RAD);
        AzRand   = TH(Flag).*RAD;
        AltRand   = PH(Flag).*RAD;
        Nrand = numel(AltRand);
        Table = [SAz,SAlt];
        [~,UI] = unique(Table(:,1));
        Table  = Table(UI,:);
        Ntable = size(Table,1);
        AltLimitRand = interp1(Table(:,1),Table(:,2),AzRand);
        AccessibleSkyFrac = sum(AltRand>AltLimitRand)./Nrand;
        
    
    end

    if ~isempty(Args.ObstractionFile)
        AltMax = max([AllData.TT_Aligned, AllData.TT_NotAligned, AllData.TT_Aligned15deg],[],2);
        FID = fopen(Args.ObstractionFile, 'w');
        fprintf(FID, '%5.1f %5.1f\n', [AllData.RealAz(:), AltMax(:)].');
        fclose(FID);
    end

end


function gamma=wall_alt(dw,ht,hw,r1,Az)
    % Example: gamma=wall_alt(1.3,1.2,1.2,0.52,10)
    
    % distance of telescopes to wall
    %dw = dw./cosd(Az);
    
    alpha = atand((hw - ht)./dw);
    lw    = (hw-ht)./sind(alpha);  % distance between telescope axis and tip of the wall
    lw(alpha==0) = dw(alpha==0);
    
    % a = sqrt(lw.^2 - r1.^2);
    % beta = atand(a./r1);
    % gamma = 90 - (beta-alpha);
    
    
    % a = sqrt(lw.^2 + r1.^2);
    beta = atand(r1./lw);
    
    gamma = alpha+beta; %90 - (beta-alpha);
end

function MaxChi = tel2tel_alt(ht,dt,r)
    % Example: MaxChi = tel2tel_alt(1.2,2.6,0.8)
    
    %%
    delta = (1:0.5:90).';
    epsilon = (1:0.5:90);
    
    hd = ht - r.*cosd(delta);
    hu = ht + r.*cosd(epsilon);
    dd = r.*sind(delta);
    du = r.*sind(epsilon);
    chi = atand((hu-hd)./(dt - dd - du));
    %contour(delta,epsilon,chi',1000)
    [MaxChi,I] = tools.math.stat.maxnd(chi);  % 23 deg for r1
    
    % if MaxChi>60
    %     'hi'
    % end
    
end


function [AltTT_r1,MaxChi_r1]=tel2tel_allalt(Az,X,Y,X2,Y2,ht,r1)    
    % telescope 2 telescope
    
    % X is maesured East to West
    % Y is measured North to South
    %X = [0.88];  % 10.6./6./2  
    %Y = [1.335]; % 5.34./2./2
    
    %X2 = X;
    %Y2 = Y + Y.*2;
    
    dt = sqrt( (X-X2).^2 + (Y-Y2).^2);
    
    MaxChi_r1 = tel2tel_alt(ht,dt,r1);
    %MaxChi_r2 = tel2tel_alt(ht,dt,r2);
    
    
    AzTT = atan2d(Y-Y2,X-X2);
    R = dt.*sind(AzTT-Az);
    
    Factor_r1 = sqrt(r1.^2 - R.^2)./r1;
    %Factor_r2 = sqrt(r2.^2 - R.^2)./r;
    
    
    
    
    DAz_r1 = asind(r1./dt);
    %DAz_r2 = asind(r2./dt);
    
    %AltTT_r2 = nan(size(Az));
    AltTT_r1 = MaxChi_r1.*Factor_r1;
    %AltTT_r2 = MaxChi_r2.*Factor_r2;
    
    Diff= ang_diff(Az,AzTT.*ones(size(Az)));
    
    AltTT_r1(Diff>DAz_r1) = 0;
    %AltTT_r2(abs(Az - AzTT)>DAz_r2) = 0;
    
    %plot(Az,AltTT_r1,'.')

end



