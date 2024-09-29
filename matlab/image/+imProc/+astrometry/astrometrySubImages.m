function [ResultRefineFit, ResultObj, AstrometricCat] = astrometrySubImages(Obj, Args)
    % Solve astrometry for sub images of a single contigious image
    %       The solution is done by executing astrometryCore for a limited
    %       number of sub images, and astrometryRefine for all the rest,
    %       based on the solution from astrometryCore.
    % Input  : - An AstroImage object with multiple sub images of a
    %            contigous field of view.
    %          * ...,key,val,...
    %            'CCDSEC' - A mandatory argument. This is a 4 column matrix
    %                   of [Xmin, Xmax, Ymin, Ymax] of the CCDSEC fir each
    %                   sub image.
    %                   This is typically obtained from the second output
    %                   argument of imProc.image.image2subimages.
    %            'CenterXY' -
    %            'RA'
    %            'Dec'
    %            'CooUnits'
    %            'Scale'
    %            'Tran'
    %            'EpochOut'
    %            'CreateNewObj'
    %            'CatName'
    %            'astrometryCoreArgs'
    %            'astrometryRefineArgs'
    % Output : - 
    % Author : Eran Ofek (Aug 2021)
    % Example:
   
    arguments
        Obj AstroImage                    % AstroImage array with catalogs
        Args.CCDSEC(:,4)
        Args.CenterXY                            = [];  % [X,Y] pix Center of Full image, If empty, calculate from CCDSEC
        Args.RA                                  = 'RA';
        Args.Dec                                 = 'DEC';
        Args.CooUnits                            = 'deg';
        Args.Scale                               = 1.0;
        Args.Tran                                = Tran2D('poly3');
        
        Args.EpochOut                            = [];
        
        Args.CreateNewObj logical                = false;
        
        Args.CatName                             = 'GAIAEDR3';  % or AstroCatalog array
        Args.CooOffset                           = [0 0];
        Args.astrometryCoreArgs cell             = {};
        Args.astrometryRefineArgs cell           = {};
        
        Args.MinNumberCoreSolutions              = 1;
        %Args.assessAstrometricQualityArgs cell   = {};

        Args.CatRadius                           = 3600; %2700; %2700; %3600;
        Args.RangeX                              = [-1000 1000].*2.5;
        Args.RangeY                              = [-1000 1000].*2.5;
        Args.StepX                               = 2;
        Args.StepY                               = 2;
        Args.Flip                                = [1 1;-1 -1];
        Args.RefRangeMag                         = [10 17.0];  % [12 18]
        Args.SearchRadius                        = 6;
        Args.FilterSigma                         = 3;
    end
    
    if Args.CreateNewObj
        ResultObj = Obj.copy;
    else
        ResultObj = Obj;
    end
    
    % get approximate coordinates for field center
    [RA, Dec] = getCoo(Obj(1).HeaderData);
    RA        = RA  + Args.CooOffset(1);
    Dec       = Dec + Args.CooOffset(2);
    
    if isempty(Args.EpochOut)
        Args.EpochOut = julday(Obj(1).HeaderData);
    end
            
    
    %Args.Tran.symPoly;   % will work only for handle class: copyObject/CreateNewObj
    
    Nobj = numel(Obj);
    if Nobj ~= size(Args.CCDSEC,1)
        error('Number of lines in CCDSEC must be equal to the number of elements in the input Obj');
    end
    
    % Center of sub images
    [SubCenterX, SubCenterY] = imUtil.ccdsec.center_ccdsec(Args.CCDSEC);
    SubCenterX = SubCenterX(:);
    SubCenterY = SubCenterY(:);
    if isempty(Args.CenterXY)
        % calculate CenterXY of full image from Args.CCDSEC
        Args.CenterXY = [0.5.*(min(Args.CCDSEC(:,1) + max(Args.CCDSEC(:,2)))),...
                         0.5.*(min(Args.CCDSEC(:,3) + max(Args.CCDSEC(:,4))))];
    end
    % Distance of sub images from full image center
    SubDistFromCenter = sqrt((SubCenterX - Args.CenterXY(1)).^2 + (SubCenterY - Args.CenterXY(2)).^2);
    % Sort SubImages by distance from image center (nominal position)
    [~,SI]  = sort(SubDistFromCenter);
    
    
    Sucess              = false(size(Obj));  % sucessful solution
    
    % do we need to define this if CatName is AstroCatalog???
    AstrometricCat      = AstroCatalog(size(Obj));
    for Iobj=1:1:Nobj
        % for each sub image
        
        if sum(Sucess)>=Args.MinNumberCoreSolutions
            % use astrometryCore
            UseRefine = true;
        else
            % switch to refine mode
            UseRefine = false;
        end
       
        if ~UseRefine
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
        else
            % run astrometryRefine
            % find a sub image which have nearby sucessful solution
            % matrix of distances : rows - sucssful; lines - ~sucessful
            DistSubMat   = sqrt((SubCenterX(Sucess) - SubCenterX(~Sucess).').^2 + (SubCenterY(Sucess) - SubCenterY(~Sucess).').^2);
            [~,IndMin]   = min(DistSubMat,[],'all','linear');
            [MinI,MinJ]  = imUtil.image.ind2sub_fast(size(DistSubMat), IndMin);
            FS           = find(Sucess);
            FNS          = find(~Sucess);
            IndSucess    = FS(MinI); 
            IndNotSucess = FNS(MinJ);
            % Index of image to solve
            Iim  = IndNotSucess;
            % index of image from which to take the WCS solution
            Iref = IndSucess;
            
            % shift solution to current CCDSEC
            % ?? This line doesn't copy the Tran object - is this ok?
            RefWCS = ResultObj(Iref).WCS.copy();
            
%             % the shift in CRPIX between the image and the ref
            ShiftX = Args.CCDSEC(Iim,1) - Args.CCDSEC(Iref,1);
            ShiftY = Args.CCDSEC(Iim,3) - Args.CCDSEC(Iref,3);
            % add shift to CRPIX - why??????
            RefWCS.CRPIX = RefWCS.CRPIX - [ShiftX, ShiftY];
            
            % call astrometryRefine with RefWCS and includeDistortion=false
            if isa(Args.CatName,'AstroCatalog')
                CatName = Args.CatName(Iim);
            else
                CatName = Args.CatName;
            end
            
            
            % DEBUGING
            % Test that the Iim image with the translated Iref WCS is ok
            %Tmp = ResultObj(Iim);
            %Tmp.WCS = RefWCS;
            %Tmp.HeaderData = wcs2header(RefWCS, Tmp.HeaderData);
            %ds9(Tmp)
          
                        
            % DEBUGING
            % ResultObj(Iim).WCS = RefWCS;
            % ResultObj(Iim).HeaderData = wcs2header(RefWCS);
            % ds9(ResultObj(Iim))
%             tic;

            %[Iim, Iref]

            if UseRefine
                Nsrc = ResultObj(Iim).CatData.sizeCatalog;
                % set the search radius according to the number of sources
                if Nsrc<100
                    RefineSearchRadius = 30;
                else
                    if Nsrc<200
                        RefineSearchRadius = 8;
                    else
                        RefineSearchRadius = 5;
                    end
                end
                
                [ResultRefineFit(Iim), ResultObj(Iim), AstrometricCat(Iim)] = imProc.astrometry.astrometryRefine(ResultObj(Iim),...
                                                                                                           'WCS',RefWCS, ...
                                                                                                           'IncludeDistortions',false,...
                                                                                                           'Tran',Args.Tran,...
                                                                                                           'SearchRadius',RefineSearchRadius,...
                                                                                                           'Scale',Args.Scale,...
                                                                                                           'RA',[],...
                                                                                                           'Dec',[],...
                                                                                                           'CooUnits','deg',...
                                                                                                           'EpochOut',Args.EpochOut,...
                                                                                                           'CatName',CatName,...  
                                                                                                           Args.astrometryCoreArgs{:});
                %
                %ResultRefineFit(Iim).WCS.populateSuccess;
                if isempty(ResultRefineFit(Iim).WCS)
                    Sucess(Iim) = false;
                else
                    Sucess(Iim) = ResultRefineFit(Iim).WCS.Success;
                end
            end
                                                                                                      
            if ~UseRefine || ~Sucess(Iim)
              



                % Estimate RA/Dec of SubImage center
                CenterX = (Args.CCDSEC(Iim,2) - Args.CCDSEC(Iim,1)).*0.5;
                CenterY = (Args.CCDSEC(Iim,4) - Args.CCDSEC(Iim,3)).*0.5;
                [RA, Dec] = RefWCS.xy2sky(CenterX, CenterY, 'OutUnits',Args.CooUnits, 'includeDistortion',false);

                [ResultFit(Iim), ResultObj(Iim), AstrometricCat(Iim)] = imProc.astrometry.astrometryCore(ResultObj(Iim),...
                                                                                                         'Tran',Args.Tran,...
                                                                                                         'RA',RA,...
                                                                                                         'Dec',Dec,...
                                                                                                         'RangeX',[-1000 1000].*1,...
                                                                                                         'RangeY',[-1000 1000].*1,...
                                                                                                         'CatRadius',3600.*1,...
                                                                                                         'RefRangeMag',[10 18],...
                                                                                                         'CooUnits',Args.CooUnits,...
                                                                                                         'EpochOut',Args.EpochOut,...
                                                                                                         'CatName',CatName,...
                                                                                                         'Scale',Args.Scale,...
                                                                                                         Args.astrometryCoreArgs{:});

                % populate the WCS in the AstroImage
                %ResultObj(Iim).WCS = ResultFit(Iim).WCS;

                ResultRefineFit(Iim).ParWCS = ResultFit(Iim).ParWCS;
                ResultRefineFit(Iim).Tran   = ResultFit(Iim).Tran;
                ResultRefineFit(Iim).ResFit = ResultFit(Iim).ResFit; % RMS errors are measured in [deg]
                ResultRefineFit(Iim).WCS    = ResultFit(Iim).WCS;
                
                %Sucess(Iim) = ResultRefineFit(Iim).WCS.Success;
                Sucess(Iim) = true; %ResultFit(Iim).WCS.Success;
                %error('astrometryCore after astrometryRefine failed');
                %UseRefine   = true;   % needed in order to avoid infinite loop
            end
                
          
        end
        
    
    end
    
end
    