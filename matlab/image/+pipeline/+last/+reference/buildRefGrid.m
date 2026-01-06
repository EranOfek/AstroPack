function [LAST_RefIm_Grid, LAST_SubIm_Grid] = buildRefGrid(Args)
    % Building a grid for LAST reference images 
    %     Optional detailed description
    % Input  : - 
    %          * ...,key,val,... 
    %          'Save2mat' - logical, save output to a .mat file
    % Output : - structures with reference image and subimage coordinates (incl. corners) 
    % Author : Yossi Shvartzvald (2025 Jul) 
    % Example: [LAST_RefIm_Grid, LAST_SubIm_Grid] = pipeline.last.reference.buildRefGrid('Save2mat',true);
    arguments        
        Args.Save2mat          = false;   
        Args.FileName          = 'LAST_refGrid.mat';
    end
    % 
    load('~/matlab/data/LAST/RefGrid/CamRot.mat');
    load('~/matlab/data/LAST/RefGrid/dDec.mat');
    load('~/matlab/data/LAST/RefGrid/dRA.mat');
    load('~/matlab/data/LAST/RefGrid/LAST_Grid_comb.mat');

    RAD = 180/pi;    
    % New - RefIm with corners
    newT = table();
    LAST_RefIm_Grid = table();
    dRA_sub = 0.3; 
    dDec_sub= 0.3;
    for i = 1:height(LAST_Grid)
        for k = 1:height(Telescope_offset_Grid.dRA)
            [Dist,PA0]=celestial.coo.sphere_dist(0,0,Telescope_offset_Grid.dRA(k)+RefIm_offset_Grid.dRA,Telescope_offset_Grid.dDec(k)+RefIm_offset_Grid.dDec,'deg');
            [d4,r4] = reckon(LAST_Grid.Dec(i),LAST_Grid.RA(i),Dist*RAD,PA0*RAD);
            newT.mount(1:24) = i;
            newT.telescope(1:24) = k;
            newT.RA(1:24) = r4;
            newT.Dec(1:24) = d4;
            %
            [Dist,PA1]=celestial.coo.sphere_dist(0,0,Telescope_offset_Grid.dRA(k)+RefIm_offset_Grid.dRA+dRA_sub,...
                Telescope_offset_Grid.dDec(k)+RefIm_offset_Grid.dDec+dDec_sub,'deg');
            [d1,r1] = reckon(LAST_Grid.Dec(i),LAST_Grid.RA(i),Dist*RAD,PA1*RAD);
            [Dist,PA2]=celestial.coo.sphere_dist(0,0,Telescope_offset_Grid.dRA(k)+RefIm_offset_Grid.dRA+dRA_sub,...
                Telescope_offset_Grid.dDec(k)+RefIm_offset_Grid.dDec-dDec_sub,'deg');
            [d2,r2] = reckon(LAST_Grid.Dec(i),LAST_Grid.RA(i),Dist*RAD,PA2*RAD);
            [Dist,PA3]=celestial.coo.sphere_dist(0,0,Telescope_offset_Grid.dRA(k)+RefIm_offset_Grid.dRA-dRA_sub,...
                Telescope_offset_Grid.dDec(k)+RefIm_offset_Grid.dDec-dDec_sub,'deg');
            [d3,r3] = reckon(LAST_Grid.Dec(i),LAST_Grid.RA(i),Dist*RAD,PA3*RAD);
            [Dist,PA4]=celestial.coo.sphere_dist(0,0,Telescope_offset_Grid.dRA(k)+RefIm_offset_Grid.dRA-dRA_sub,...
                Telescope_offset_Grid.dDec(k)+RefIm_offset_Grid.dDec+dDec_sub,'deg');
            [d4,r4] = reckon(LAST_Grid.Dec(i),LAST_Grid.RA(i),Dist*RAD,PA4*RAD);       
            newT.RA1(1:24) = r1; newT.Dec1(1:24) = d1;
            newT.RA2(1:24) = r2; newT.Dec2(1:24) = d2;
            newT.RA3(1:24) = r3; newT.Dec3(1:24) = d3;
            newT.RA4(1:24) = r4; newT.Dec4(1:24) = d4;
            newT.PA1(1:24) = PA1; % is it the right angle? 
            newT.PA2(1:24) = PA2; 
            newT.PA3(1:24) = PA3; 
            newT.PA4(1:24) = PA4; 
            %        
            LAST_RefIm_Grid = [LAST_RefIm_Grid;newT];
        end
    end
    % New - SubIm
    newT = table();
    LAST_SubIm_Grid = table();
    for i = 1:height(LAST_Grid)
        for k = 1:height(Telescope_offset_Grid.dRA)
            [Dist,PA]=celestial.coo.sphere_dist(0,0,Telescope_offset_Grid.dRA(k)+SubIm_offset_Grid.dRA,Telescope_offset_Grid.dDec(k)+SubIm_offset_Grid.dDec,'deg');
            [d4,r4] = reckon(LAST_Grid.Dec(i),LAST_Grid.RA(i),Dist*RAD,PA*RAD);
            newT.mount(1:24) = i;
            newT.telescope(1:24) = k;
            newT.RA(1:24) = r4;
            newT.Dec(1:24) = d4;

            LAST_SubIm_Grid = [LAST_SubIm_Grid;newT];
        end
    end

    % figure('WindowStyle','docked','Color',[1 1 1]);box on;hold on;
    % axesm ('aitoff', 'Frame', 'on', 'Grid', 'on');
    % F_odd_tel = LAST_RefIm_Grid.telescope==1 | LAST_RefIm_Grid.telescope==3;
    % F_odd_mnt = mod(LAST_RefIm_Grid.mount,2)==1;
    % 
    % plotm(LAST_Grid.Dec,LAST_Grid.RA,'+k','LineWidth',2)
    %  plotm(LAST_SubIm_Grid.Dec,LAST_SubIm_Grid.RA,'.k','MarkerSize',5)
    % plotm(LAST_RefIm_Grid.Dec(F_odd_mnt & F_odd_tel & Fuse),LAST_RefIm_Grid.RA(F_odd_mnt & F_odd_tel & Fuse),'ob','MarkerSize',4,'LineWidth',2);
    % plotm(LAST_RefIm_Grid.Dec(F_odd_mnt & ~F_odd_tel & Fuse),LAST_RefIm_Grid.RA(F_odd_mnt & ~F_odd_tel & Fuse),'oc','MarkerSize',4,'LineWidth',2);
    % plotm(LAST_RefIm_Grid.Dec(~F_odd_mnt & F_odd_tel & Fuse),LAST_RefIm_Grid.RA(~F_odd_mnt & F_odd_tel & Fuse),'or','MarkerSize',4,'LineWidth',2);
    % plotm(LAST_RefIm_Grid.Dec(~F_odd_mnt & ~F_odd_tel & Fuse),LAST_RefIm_Grid.RA(~F_odd_mnt & ~F_odd_tel & Fuse),'om','MarkerSize',4,'LineWidth',2);

    % 
    LAST_RefIm_Grid.use(:) = true;

    Fmnt = mod(LAST_RefIm_Grid.mount,2);
    Fdec = LAST_RefIm_Grid.Dec>89.5;
    F = Fmnt & Fdec;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==1;
    Fdec = LAST_RefIm_Grid.Dec>85;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(4:4:end)=true;
    F_edge(3:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==1;
    Fdec = LAST_RefIm_Grid.Dec>88.5;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(2:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    %

    Ftel = LAST_RefIm_Grid.telescope==2;
    Fdec = LAST_RefIm_Grid.Dec>58.5;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(1:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==2;
    Fdec = LAST_RefIm_Grid.Dec>53.5 & LAST_RefIm_Grid.Dec<54;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(1:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==2;
    Fdec = LAST_RefIm_Grid.Dec>76;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(2:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==2;
    Fdec = LAST_RefIm_Grid.Dec>82.5 & LAST_RefIm_Grid.Dec<85;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(3:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==2;
    Fdec = LAST_RefIm_Grid.Dec>88;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(3:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==2;
    Fdec = LAST_RefIm_Grid.Dec>89;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(4:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;
    %

    Ftel = LAST_RefIm_Grid.telescope==3;
    Fdec = LAST_RefIm_Grid.Dec>79;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(1:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==3;
    Fdec = LAST_RefIm_Grid.Dec>73.5 & LAST_RefIm_Grid.Dec<75;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(1:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==3;
    Fdec = LAST_RefIm_Grid.Dec>68.5 & LAST_RefIm_Grid.Dec<69;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(1:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==3;
    Fdec = LAST_RefIm_Grid.Dec>80.5 & LAST_RefIm_Grid.Dec<81;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(2:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==3;
    Fdec = LAST_RefIm_Grid.Dec>85 & LAST_RefIm_Grid.Dec<87;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(2:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

     Ftel = LAST_RefIm_Grid.telescope==3;
    Fdec = LAST_RefIm_Grid.Dec>86 & LAST_RefIm_Grid.Dec<87;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(3:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==3;
    Fdec = LAST_RefIm_Grid.Dec>86.5 & LAST_RefIm_Grid.Dec<87;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(4:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    %
    Fmnt = mod(LAST_RefIm_Grid.mount,2);
    Fdec = LAST_RefIm_Grid.Dec<-89.5;
    F = Fmnt & Fdec;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==4;
    Fdec = LAST_RefIm_Grid.Dec<-85;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(4:4:end)=true;
    F_edge(3:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==4;
    Fdec = LAST_RefIm_Grid.Dec<-88.5;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(2:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    %

    Ftel = LAST_RefIm_Grid.telescope==3;
    Fdec = LAST_RefIm_Grid.Dec<-58.5;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(1:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==3;
    Fdec = LAST_RefIm_Grid.Dec<-53.5 & LAST_RefIm_Grid.Dec>-54;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(1:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==3;
    Fdec = LAST_RefIm_Grid.Dec<-76;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(2:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==3;
    Fdec = LAST_RefIm_Grid.Dec<-82.5 & LAST_RefIm_Grid.Dec>-85;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(3:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==3;
    Fdec = LAST_RefIm_Grid.Dec<-88;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(3:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==3;
    Fdec = LAST_RefIm_Grid.Dec<-89;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(4:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;
    %

    Ftel = LAST_RefIm_Grid.telescope==2;
    Fdec = LAST_RefIm_Grid.Dec<-79;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(1:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==2;
    Fdec = LAST_RefIm_Grid.Dec<-73.5 & LAST_RefIm_Grid.Dec>-75;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(1:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==2;
    Fdec = LAST_RefIm_Grid.Dec<-68.5 & LAST_RefIm_Grid.Dec>-69;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(1:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==2;
    Fdec = LAST_RefIm_Grid.Dec<-80.5 & LAST_RefIm_Grid.Dec>-81;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(2:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==2;
    Fdec = LAST_RefIm_Grid.Dec<-85 & LAST_RefIm_Grid.Dec>-87;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(2:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==2;
    Fdec = LAST_RefIm_Grid.Dec<-86 & LAST_RefIm_Grid.Dec>-87;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(3:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;

    Ftel = LAST_RefIm_Grid.telescope==2;
    Fdec = LAST_RefIm_Grid.Dec<-86.5 & LAST_RefIm_Grid.Dec>-87;
    F_edge = false(size(LAST_RefIm_Grid.telescope));
    F_edge(4:4:end)=true;
    F = Fdec & Ftel & F_edge;
    LAST_RefIm_Grid.use(F) = false;
    
    if Args.Save2mat
        save(Args.FileName,'LAST_RefIm_Grid','LAST_SubIm_Grid');
    end
end
