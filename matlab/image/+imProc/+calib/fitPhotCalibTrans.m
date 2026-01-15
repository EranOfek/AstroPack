function [Result, PhotCalib] = fitPhotCalibTrans(Obj, Args)
    % Transmission-based absolute photometric calibration wrapper
    % Description: Wrapper function for PhotCalibTrans class that performs
    %              transmission-based photometric calibration on a vector of
    %              AstroImages or AstroCatalogs.
    % Input  : - Obj - AstroImage or AstroCatalog object (scalar or vector).
    %          * ...,key,val,...
    %            Calibrator selection:
    %            'SearchRadius' - Gaia matching radius [arcsec]. Default is 1.5.
    %            'MagRange' - Calibrator magnitude range [min max]. Default is [12 16].
    %            Transmission model:
    %            'FunListName' - Name of transmission function list. Default is 'DefaultLASTFunList'.
    %            'CustomFunList' - Custom function list (overrides FunListName). Default is [].
    %            'OptSeqName' - Name of optimization sequence. Default is 'DefaultLASTOptSeq'.
    %            'CustomOptSeq' - Custom optimization sequence (overrides OptSeqName). Default is [].
    %            'Tran2DType' - Position-dependent correction type. Default is 'cheby1_4_xt'.
    %            Catalog update:
    %            'AddMagAB' - Add calibrated AB magnitude columns to catalog. Default is true.
    %            'FluxColName' - Flux column for calibration fitting. Default is 'FLUX_APER_3'.
    %            'AddZP' - Add ZP column (position-dependent) to catalog. Default is false.
    %            Header update:
    %            'UpdateHeader' - Update AstroImage header with ZP. Default is true.
    %            General:
    %            'CreateNewObj' - Copy input object. Default is false.
    %            'Verbose' - Enable verbose output. Default is true.
    % Output : - Result - Input object, possibly with updated catalog and header.
    %          - PhotCalib - Array of PhotCalibTrans objects (one per input object).
    % Author : D. Kovaleva (Jan 2026)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: AI = io.files.load2('LAST_image.mat');
    %          [Result, PC] = imProc.calib.fitPhotCalibTrans(AI);
    %          % Check calibration success
    %          if PC.Success
    %              fprintf('Calibration successful! RMS = %.4f mag\n', PC.TransModel.RMS);
    %          end
    %          % Process multiple images
    %          [Result, PC] = imProc.calib.fitPhotCalibTrans(AI_vector, 'Verbose', false);

    arguments
        Obj  % AstroImage or AstroCatalog

        % Calibrator selection
        Args.SearchRadius = 1.5  % arcsec
        Args.MagRange = [12 16]

        % Transmission model
        Args.FunListName = 'DefaultLASTFunList'
        Args.CustomFunList = []
        Args.OptSeqName = 'DefaultLASTOptSeq'
        Args.CustomOptSeq = []
        Args.Tran2DType = 'cheby1_4_xt'

        % Catalog update
        Args.AddMagAB logical = true
        Args.FluxColName = 'FLUX_APER_3'
        Args.AddZP logical = false

        % Header update
        Args.UpdateHeader logical = true

        % General
        Args.CreateNewObj logical = false
        Args.Verbose logical = true
    end

    % ====================================================================
    % VALIDATE INPUT
    % ====================================================================
tic
    if isa(Obj, 'AstroImage')
        IsAstroImage = true;
    elseif isa(Obj, 'AstroCatalog')
        IsAstroImage = false;
    else
        error('Input must be AstroImage or AstroCatalog object');
    end

    % Copy object if requested
    if Args.CreateNewObj
        Result = Obj.copy();
    else
        Result = Obj;
    end

    Nobj = numel(Result);

    if Args.Verbose
        fprintf('\n=== TRANSMISSION-BASED PHOTOMETRIC CALIBRATION ===\n');
        fprintf('Processing %d object(s)\n', Nobj);
    end

    % Initialize output array of PhotCalibTrans objects
    PhotCalib = PhotCalibTrans.empty(0, Nobj);

    % ====================================================================
    % LOOP OVER OBJECTS
    % ====================================================================

    for Iobj = 1:Nobj
        if Args.Verbose
            fprintf('\n--- Object %d/%d ---\n', Iobj, Nobj);
        end

        % Create new PhotCalibTrans object for this image
        PC = PhotCalibTrans();

        % Build calibration arguments
        CalibArgs = {...
            'FunListName', Args.FunListName, ...
            'OptSeqName', Args.OptSeqName, ...
            'Tran2DType', Args.Tran2DType, ...
            'SearchRadius', Args.SearchRadius, ...
            'MagRange', Args.MagRange, ...
            'Verbose', Args.Verbose};

        % Add custom function list if provided
        if ~isempty(Args.CustomFunList)
            CalibArgs = [CalibArgs, {'CustomFunList', Args.CustomFunList}];
        end

        % Add custom optimization sequence if provided
        if ~isempty(Args.CustomOptSeq)
            CalibArgs = [CalibArgs, {'CustomOptSeq', Args.CustomOptSeq}];
        end

        % ----------------------------------------------------------------
        % Perform calibration
        % ----------------------------------------------------------------

        PC = PC.calibrate(Result(Iobj), CalibArgs{:});

        % ----------------------------------------------------------------
        % Post-calibration processing
        % ----------------------------------------------------------------

        if PC.Success
            % Add AB magnitude columns if requested
            if Args.AddMagAB
                if IsAstroImage
                    Result(Iobj).CatData = PC.addMagAB(Result(Iobj).CatData);
                else
                    Result(Iobj) = PC.addMagAB(Result(Iobj));
                end
            end

            % Add ZP column if requested
            if Args.AddZP
                if IsAstroImage
                    Result(Iobj).CatData = PC.addZP(Result(Iobj).CatData);
                else
                    Result(Iobj) = PC.addZP(Result(Iobj));
                end
            end

            % Update header if requested
            if Args.UpdateHeader
                if IsAstroImage
                    PC.writePhotCalibTrans(Result(Iobj).HeaderData);
                else
                    % For AstroCatalog, create new header (not stored)
                    PC.writePhotCalibTrans(AstroHeader());
                end
                if Args.Verbose
                    fprintf('  Header updated with calibration results\n');
                end
            end
        else
            if Args.Verbose
                fprintf('  Calibration unsuccessful - skipping post-processing\n');
            end
        end

        % Store calibration object
        PhotCalib(Iobj) = PC;
    end
toc
    % ====================================================================
    % SUMMARY
    % ====================================================================

    if Args.Verbose
        Nsuccess = sum([PhotCalib.Success]);
        fprintf('\n=== CALIBRATION COMPLETE ===\n');
        fprintf('Successful: %d/%d objects\n', Nsuccess, Nobj);
        if Nsuccess > 0
            RMSvals = arrayfun(@(x) x.TransModel.RMS, PhotCalib([PhotCalib.Success]));
            fprintf('RMS range: %.4f - %.4f mag\n', min(RMSvals), max(RMSvals));
        end
    end
end
