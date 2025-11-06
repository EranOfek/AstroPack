function [Result] = astrometrySingleSubImage(AI, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Nov) 
    % Example: 

    arguments
        AI
        Args.CCDSEC
        Args.OtherAW                        % AstroWCS or AstroImage
        Args.OtherCCDSEC
        Args.OtherSubCenter         = [];
        Args.SubCenter              = [];
        Args.SucessAW               = [];
        Args.ThresholdIdenticalWCS  = 0.5;  % pix
        Args.SelecteNearestJD       = true;
    end

     
    if Args.SelecteNearestJD
        JD = AI.julday;
    else
        JD = [];
    end

    if isempty(Args.SucessAW)
        Args.SucessAW = false(Naw,1);
        for Iaw=1:1:Naw
            Args.SucessAW(Iim) = AW(Iaw).Success;
        end
    end
    IsucessAW = find(Args.SucessAW); % FS
   
    if isempty(IsucessAW)
        % No guess astrometry is available - run astrometryCore:


        % Use core solution
            
            % select sub image index, after sorting by distance of sub image
            % from full image
            Iim = SI(Iobj);
            
            % astrometric solution
            if isa(Args.CatName,'AstroCatalog')
                CatName = Args.CatName(Iim);
            else
                CatName = Args.CatName;
            end
            
            % FFU: estimate RA/Dec for center of image
            
            %tic;

            % This is a patch to deal with the fact that the pointing model
            % is not accurate near the pole
            if Dec>85
                Args.CatRadius = 5000;
                Args.RangeX    = [-4000 4000];
                Args.RangeY    = [-4000 4000];
            end
            
            [ResultFit(Iim), ResultObj(Iim), AstrometricCat(Iim)] = imProc.astrometry.astrometryCore(ResultObj(Iim),...
                                                                                                     'Tran',Args.Tran,...
                                                                                                     'RA',RA,...
                                                                                                     'Dec',Dec,...
                                                                                                     'CooUnits',Args.CooUnits,...
                                                                                                     'CatRadius',Args.CatRadius,...
                                                                                                     'CatRadiusUnits','arcsec',...
                                                                                                     'EpochOut',Args.EpochOut,...
                                                                                                     'CatName',CatName,...
                                                                                                     'Scale',Args.Scale,...
                                                                                                     'RangeX',Args.RangeX,...
                                                                                                     'RangeY',Args.RangeY,...
                                                                                                     'StepX',Args.StepX,...
                                                                                                     'StepY',Args.StepY,...
                                                                                                     'Flip',Args.Flip,...
                                                                                                     'RefRangeMag',Args.RefRangeMag,...
                                                                                                     'SearchRadius',Args.SearchRadius,...
                                                                                                     'FilterSigma',Args.FilterSigma,...
                                                                                                     Args.astrometryCoreArgs{:});
                                                           

            if ResultFit(Iim).Nsolutions==0
                % astrometry failed - try another sub image
                % switch order in SI
                % and also set FilterCat to false
                SItemp = SI;

                SI(1) = SItemp(2);
                SI(2) = SItemp(1);
                Iim = SI(Iobj);

                [ResultFit(Iim), ResultObj(Iim), AstrometricCat(Iim)] = imProc.astrometry.astrometryCore(ResultObj(Iim),...
                                                                                                     'Tran',Args.Tran,...
                                                                                                     'RA',RA,...
                                                                                                     'Dec',Dec,...
                                                                                                     'CooUnits',Args.CooUnits,...
                                                                                                     'CatRadius',Args.CatRadius,...
                                                                                                     'CatRadiusUnits','arcsec',...
                                                                                                     'EpochOut',Args.EpochOut,...
                                                                                                     'CatName',CatName,...
                                                                                                     'Scale',Args.Scale,...
                                                                                                     'RangeX',Args.RangeX,...
                                                                                                     'RangeY',Args.RangeY,...
                                                                                                     'StepX',Args.StepX,...
                                                                                                     'StepY',Args.StepY,...
                                                                                                     'Flip',Args.Flip,...
                                                                                                     'RefRangeMag',Args.RefRangeMag,...
                                                                                                     'SearchRadius',Args.SearchRadius,...
                                                                                                     'FilterSigma',Args.FilterSigma,...
                                                                                                     Args.astrometryCoreArgs{:},...
                                                                                                     'FilterCat',false);
            end

                                                                                                 
            %toc
            % populate the WCS in the AstroImage
            %ResultObj(Iim).WCS = ResultFit(Iim).WCS;
            if ResultFit(Iim).Nsolutions == 0
                % problem - no solution found
                error('problem - no solution found');
            else
            
                ResultRefineFit(Iim).ParWCS = ResultFit(Iim).ParWCS;
                ResultRefineFit(Iim).Tran   = ResultFit(Iim).Tran;
                ResultRefineFit(Iim).ResFit = ResultFit(Iim).ResFit;
                ResultRefineFit(Iim).WCS    = ResultFit(Iim).WCS;

                % check qulity of solution
                Sucess(Iim) = ResultFit(Iim).WCS.Success;
                %[Sucess(Iim), QualitySummary(Iim)] = imProc.astrometry.assessAstrometricQuality(ResultFit(Iim).ResFit, Args.assessAstrometricQualityArgs{:});
            end



    else %if isempty(IsucessAW)
        % construct an initial guess WCS
        [RefWCS, Iccdsec]=imProc.astrometry.remapWCS(CCDSEC, AI, AI_CCDSEC, 'OtherSubCenter',Args.OtherSubCenter,...
                                                                        'OtherCCDSEC',Ars.OtherCCDSEC,...
                                                                        'SubCenter',Args.SubCenter,...
                                                                        'SucessAW',Args.SucessAW,...
                                                                        'ThresholdIdenticalWCS',Args.ThresholdIdenticalWCS,...
                                                                        'JD',JD);
        %


    end %if isempty(IsucessAW)


end
