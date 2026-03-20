function [Result] = findMeasureOutFocusAnnulus(Image, Args)
    % Find out-of-focus-rings in an image and measure their radial/angular properties.
    %   Measure the mean/median/std of radial plots for sectors for the
    %   brightest out-of-focus source in the image.
    % Input  : - A 2D image.
    %          * ...,key,val,... 
    %            'Threshold' - S/N for detection threshold for rings.
    %                   Default is 50.
    %            'MaxRad' - Max. radius in which to calculate radial
    %                   profiles. Default is 100.
    %            'StepRadius' - Step in radial radius. Default is 2.
    %            'Angle' - Step in angle [deg]. Default is 30.
    %            'RinsTempBank' - See imUtil.sources.findRings for options.
    %                   Default is [11 20; 15 33; 21 50; 31 60]
    %            'SubImageCCDSEC' - CCDSEC for which to calculate radial
    %                   statistics.
    %                   Default is : [2701 3700 4301 5300;...
    %                              101 1100  101 1100;...
    %                               101 1100 8501 9500;...
    %                              5301 6300  101 1100;...
    %                              5301 6300 8501 9500];
    %            'HeightFactor' - Height factor relative to max. val. t
    %                   which to estimate radius of inner and outer annulus.
    %                   Default is 0.5.
    % Output : - A structure array with elemnt per sub image, and the
    %            following fields:
    %            .RadialVec - Vector of mid radial points.
    %            .AngleVec - Vector of mid angular points.
    %            .MeanSEctor - Mean value at [Radius X Angle].
    %            .MedianSector - Medain value at [Radius X Angle].
    %            .StdSector - Std value at [Radius X Angle].
    %            .NSector - Num of pix at [Radius X Angle].
    %            .InnerRadius - Inner radius as a function of angle.
    %            .OuterRadius - Outer radius as a function of angle.
    % Author : Eran Ofek (2026 Mar) 
    % Example: R=imUtil.psf.findMeasureOutFocusAnnulus(Img(4001:5000,3001:4000));
    %          % plot all radial plots in each sector
    %          plot(R(1).RadialVec, R(1).MeanSector)
    %          % std of radial plots over all sectors
    %          plot(R(1).RadialVec, std(R(1).MeanSector,[],2))

    arguments
        Image
        Args.Threshold   = 50;
        Args.MaxRad      = 100;
        Args.StepRadius  = 2;
        Args.Angle       = 30;
               
        Args.RingsTempBank     = [11 20; 15 33; 21 50; 31 60]; % or cube

        Args.SubImagesCCDSEC   = [2701 3700 4301 5300;...
                                   101 1100  101 1100;...
                                   101 1100 8501 9500;...
                                  5301 6300  101 1100;...
                                  5301 6300 8501 9500];
        Args.HeightFactor      = 0.5;
    end

    Nsub = size(Args.SubImagesCCDSEC,1);
    for Isub=1:1:Nsub
        SubImage = imUtil.cut.trim2d(Image, Args.SubImagesCCDSEC(Isub,:));
        SizeIm   = size(SubImage);

        [Pos,FI, Back] = imUtil.sources.findRings(SubImage, 'CalcBack',true, 'Threshold', Args.Threshold, 'RingsTempBank',Args.RingsTempBank);
        %[X,Y,SN,ImageIndex,LinaerIndexIn2D].

        % remoe positions near edge:
        Flag = Pos(:,1)>Args.MaxRad & Pos(:,2)>Args.MaxRad & Pos(:,1)<(SizeIm(2)-Args.MaxRad) & Pos(:,2)<(SizeIm(1)-Args.MaxRad);
        Pos  = Pos(Flag,:);
    
        [~,MaxI] = max(Pos(:,3));
    
        [Result(Isub).RadialVec, Result(Isub).AngleVec, Result(Isub).MeanSector, Result(Isub).MedianSector, Result(Isub).StdSector, Result(Isub).NSector] = imUtil.psf.radialAnnulusStatsByAngle(SubImage-Back, ...
                        Pos(MaxI,1), Pos(MaxI,2),...
                        'MaxRad',Args.MaxRad,...
                        'StepRadius',Args.StepRadius,...
                        'Angle',Args.Angle);

        % annulus properties
        HeightThreshold = max(Result(Isub).MeanSector,[],'all').*Args.HeightFactor;
        [Nrad, Nang] = size(Result(Isub).MeanSector);
        Result(Isub).InnerRadius  = nan(1,Nang);
        Result(Isub).OuterRadius  = nan(1,Nang);
        for Iang=1:1:Nang
            Inner = find(Result(Isub).MeanSector(:,Iang)>HeightThreshold, 1, 'first');
            Outer = find(Result(Isub).MeanSector(:,Iang)>HeightThreshold, 1, 'last');
            if ~isempty(Inner)
                Result(Isub).InnerRadius(Iang) = Inner;
            end
            if ~isempty(Outer)
                Result(Isub).OuterRadius(Iang) = Outer;
            end
        end

    end

end
