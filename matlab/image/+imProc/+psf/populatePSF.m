function [Obj,Result]=populatePSF(Obj, Args)
    % Populate PSF in an AstroImage
    %   Construct a PSF using: imUtil.psf.constructPSF
    %   and populate the AstroPSF object in an AstroImage object.
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %            'RePopulatePSF' - A logical indicating if to repopulate
    %                   the PSF even if exist.
    %                   Default is false.
    %            'ColSN' - A cell array of S/N columns in catalog for using
    %                   the S/N diff method to select good PSF stars.
    %                   Default is {'SN_1', 'SN_2'}.
    %
    %            AFTER DEBUGING COPY HELP FROM imUtil.psf.constructPSF
    %            
    % Output : - An AstroImage object with the populated PSF.
    %          - A structure array of data regrading the selection of PSF
    %            stars in the images. One element per element in the input
    %            AstroImage.
    % Author : Eran Ofek (Jul 2023)
    % Example: AI = imProc.psf.populatePSF(AI);
   
    arguments
        Obj AstroImage
        Args.RePopulatePSF logical     = false;
        Args.ColSN                     = {'SN_1','SN_2'};
        
        Args.SubAnnulusBack logical    = true;
        Args.RadiusPSF                 = 8;
        Args.image2cutoutsArgs cell    = {};
        
        %Args.Threshold                 = 5;
        Args.ThresholdPSF              = 20;
        Args.RangeSN                   = [50 1000];
        Args.InitPsf                   = @imUtil.kernel2.gauss
        Args.InitPsfArgs cell          = {[0.1;2]};
        Args.Conn                      = 8;
        Args.CleanSources              = true;
        Args.cleanSourcesArgs cell     = {};
        Args.backgroundCubeArgs cell   = {};
        
        Args.SNdiff                    = 0;  % if empty skip
        Args.moment2Args cell          = {};
        Args.DeltaSigma                = 0.5;   % if empty skip
        Args.NighRadius                = 7;     % if empty skip
        Args.MinNumGoodPsf             = 5;
        
        Args.constructPSF_cutoutsArgs cell = {};
        Args.SumMethod                 = 'median';
        
        Args.SmoothWings logical       = true;
        Args.SuppressFun               = @imUtil.kernel2.cosbell;
        Args.SuppressWidth             = 3;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % for each AstroImage elenment
        
        if Obj(Iobj).PSFData.isemptyPSF || Args.RePopulatePSF
            % check if catalog of sources is available
            if Obj(Iobj).CatData.isemptyCatalog
                % catalog is not available
                X  = [];
                Y  = [];
                SN = [];
            else
                % catalog is available
                XY = Obj(Iobj).CatData.getXY;
                X  = X(:,1);
                Y  = Y(:,2);
                SN = Obj(Iobj).CatData.getCol(Args.ColSN);
            end
            [Result(Iobj), MeanPSF, VarPSF, NimPSF] = imUtil.psf.constructPSF(Obj(Iobj).Image,...
                                                                        'X',X, 'Y',Y,...
                                                                        'SN',SN,...
                                                                        'Back',Obj(Iobj).Back,...
                                                                        'Var',Obj(Iobj).Var,...
                                                                        'SubAnnulusBack',Args.SubAnnulusBack,...
                                                                        'RadiusPSF',Args.RadiusPSF,...
                                                                        'image2cutoutsArgs',Args.image2cutoutsArgs,...
                                                                        'ThresholdPSF',Args.ThresholdPSF,...
                                                                        'RangeSN',Args.RangeSN,...
                                                                        'InitPsf',Args.InitPsf,...
                                                                        'InitPsfArgs',Args.InitPsfArgs,...
                                                                        'Conn',Args.Conn,...
                                                                        'CleanSources',Args.CleanSources,...
                                                                        'cleanSourcesArgs',Args.cleanSourcesArgs,...
                                                                        'backgroundCubeArgs',Args.backgroundCubeArgs,...
                                                                        'SNdiff',Args.SNdiff,...
                                                                        'moment2Args',Args.moment2Args,...
                                                                        'DeltaSigma',Args.DeltaSigma,...
                                                                        'NighRadius',Args.NighRadius,...
                                                                        'MinNumGoodPsf',Args.MinNumGoodPsf,...
                                                                        'constructPSF_cutoutsArgs',Args.constructPSF_cutoutsArgs,...
                                                                        'SumMethod',Args.SumMethod,...
                                                                        'SmoothWings',Args.SmoothWings,...
                                                                        'SuppressFun',Args.SuppressFun,...
                                                                        'SuppressWidth',Args.SuppressWidth);
                                                                        
            % insert PSF data
            Obj(Iobj).PSFData.Data = MeanPSF;
            Obj(Iobj).PSFData.Var  = VarPSF;
            
        end
        
        
    end
    
    
end
