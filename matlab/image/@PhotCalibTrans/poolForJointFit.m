function JP = poolForJointFit(PC_array, Args)
    % Pool per-crop calibrator data across a PhotCalibTrans array into the
    % single-vector / single-matrix form CompositeFun.fitMultiStage and
    % CompositeFun.costFun expect.
    %
    % After selectCalibratorsJoint has populated PC(i).SourceData /
    % PC(i).SpecData per crop, this helper concatenates everything that
    % the regular per-crop CostArgs cell (see PhotCalibTrans.calibrate
    % ~line 1126) reads from one PC. The output struct can be passed
    % straight into a CostArgs cell that hands the joint pool to
    % fitMultiStage as if it were one big crop.
    % Input  : - PC_array - 1xN PhotCalibTrans array. Each PC needs
    %                       TransModel, SpecData (with .Spec, .SpecErr,
    %                       .SpecWvl), and SourceData (with Flux, XFULL,
    %                       YFULL, optionally MagErr, AIRMASS, and the
    %                       FluxErr column matching Args.FluxColName when
    %                       provided).
    %          * Args - struct or key/val with:
    %             .FluxColName     - default 'FLUX_APER_3'. Used to derive
    %                                FluxErr column name (FLUX→FLUXERR).
    %             .ComputeSpecFluxMatrix - logical, default true. Calls
    %                                PC(i).resampleCalibratorSpectra() per
    %                                crop and concatenates the resulting
    %                                interpolated spectra into one
    %                                [Nwvl_trans x Ntotal] matrix.
    %             .Verbose         - default false.
    % Output : - JP struct with:
    %             .Flux           - Ntotal x 1 pooled observed flux
    %             .FluxErr        - Ntotal x 1 pooled FluxErr (NaN where
    %                               the source crop didn't carry the column)
    %             .XFULL, .YFULL  - Ntotal x 1 pooled full-image pixel coords
    %             .CropID         - Ntotal x 1 origin crop index (for diags)
    %             .Spec           - Nwvl_spec x Ntotal pooled calibrator
    %                               spectra (matches Obj.SpecData.Spec' shape
    %                               expected by costFun WeightMatrix)
    %             .SpecErr        - Nwvl_spec x Ntotal pooled spectral errors
    %             .SpecWvl        - Nwvl_spec x 1 from PC(1).SpecData.SpecWvl
    %             .SpecFluxMatrix - Nwvl_trans x Ntotal (resampled onto
    %                               TransWvl; empty if ComputeSpecFluxMatrix=false)
    %             .MagErr         - Ntotal x 1 pooled per-source MagErr
    %                               (NaN where missing on a PC's SourceData)
    %             .Airmass        - Ntotal x 1 pooled AIRMASS (NaN if absent)
    %             .ExpTime_eff    - scalar from PC(1).ExpTime_eff
    %             .Aperture       - scalar from PC(1).Aperture
    %             .TransWvl       - from PC(1).TransWvl
    %             .Ntotal         - sum(Ncalib_i) for convenience
    % Author : D. Kovaleva (April 2026)
    % Example: JP = PhotCalibTrans.poolForJointFit(PC);

    arguments
        PC_array
        Args.FluxColName           = 'FLUX_APER_3'
        Args.ComputeSpecFluxMatrix logical = true
        Args.Verbose       logical = false
    end

    Ncrops = numel(PC_array);
    FluxErrColName = strrep(Args.FluxColName, 'FLUX', 'FLUXERR');

    FluxCell    = cell(Ncrops, 1);
    FluxErrCell = cell(Ncrops, 1);
    XfullCell   = cell(Ncrops, 1);
    YfullCell   = cell(Ncrops, 1);
    CropIDCell  = cell(Ncrops, 1);
    SpecCell    = cell(Ncrops, 1);
    SpecErrCell = cell(Ncrops, 1);
    SpecFluxCell= cell(Ncrops, 1);
    MagErrCell  = cell(Ncrops, 1);
    AirmassCell = cell(Ncrops, 1);

    SpecWvlRef = [];
    TransWvlRef = [];
    ExpTime_eff = NaN;
    Aperture    = NaN;

    for I = 1:Ncrops
        PC = PC_array(I);
        if isempty(PC.TransModel) || isempty(PC.SpecData) || isempty(PC.SourceData)
            continue;
        end
        SD = PC.SourceData;
        if height(SD.Table) == 0
            continue;
        end
        VarNames = SD.Table.Properties.VariableNames;
        N_i = height(SD.Table);

        FluxCell{I}   = SD.getCol('Flux');
        FluxErrCell{I} = nan(N_i, 1);
        if ismember(FluxErrColName, VarNames)
            FluxErrCell{I} = SD.getCol(FluxErrColName);
        elseif ismember('FluxErr', VarNames)
            FluxErrCell{I} = SD.getCol('FluxErr');
        end

        if ~ismember('XFULL', VarNames) || ~ismember('YFULL', VarNames)
            error('PhotCalibTrans:poolForJointFit:NoXYFULL', ...
                  'PC(%d).SourceData lacks XFULL/YFULL — run selectCalibratorsJoint first.', I);
        end
        XfullCell{I} = SD.getCol('XFULL');
        YfullCell{I} = SD.getCol('YFULL');
        CropIDCell{I}= repmat(double(I), N_i, 1);

        % Spectra (per-crop SpecData.Spec is [Ncalib x Nwvl]; we want
        % [Nwvl x Ntotal] in the pool, matching costFun's WeightMatrix shape)
        SpecCell{I}    = PC.SpecData.Spec';
        if isfield(PC.SpecData, 'SpecErr') && ~isempty(PC.SpecData.SpecErr)
            SpecErrCell{I} = PC.SpecData.SpecErr';
        end

        if Args.ComputeSpecFluxMatrix
            SpecFluxCell{I} = PC.resampleCalibratorSpectra();
        end

        if ismember('MagErr', VarNames)
            MagErrCell{I} = SD.getCol('MagErr');
        else
            MagErrCell{I} = nan(N_i, 1);
        end

        if ismember('AIRMASS', VarNames)
            AirmassCell{I} = SD.getCol('AIRMASS');
        else
            AirmassCell{I} = nan(N_i, 1);
        end

        if isempty(SpecWvlRef)
            SpecWvlRef  = PC.SpecData.SpecWvl;
            TransWvlRef = PC.TransWvl;
            ExpTime_eff = PC.ExpTime_eff;
            Aperture    = PC.Aperture;
        end
    end

    NonEmpty = ~cellfun(@isempty, FluxCell);
    if ~any(NonEmpty)
        error('PhotCalibTrans:poolForJointFit:NoData', ...
              'No PC produced any pooled calibrators.');
    end

    JP = struct();
    JP.Flux     = vertcat(FluxCell{NonEmpty});
    JP.FluxErr  = vertcat(FluxErrCell{NonEmpty});
    JP.XFULL    = vertcat(XfullCell{NonEmpty});
    JP.YFULL    = vertcat(YfullCell{NonEmpty});
    JP.CropID   = vertcat(CropIDCell{NonEmpty});
    JP.Spec     = horzcat(SpecCell{NonEmpty});
    if any(~cellfun(@isempty, SpecErrCell))
        JP.SpecErr = horzcat(SpecErrCell{NonEmpty});
    else
        JP.SpecErr = [];
    end
    if Args.ComputeSpecFluxMatrix && any(~cellfun(@isempty, SpecFluxCell))
        JP.SpecFluxMatrix = horzcat(SpecFluxCell{NonEmpty});
    else
        JP.SpecFluxMatrix = [];
    end
    JP.MagErr   = vertcat(MagErrCell{NonEmpty});
    JP.Airmass  = vertcat(AirmassCell{NonEmpty});
    JP.SpecWvl  = SpecWvlRef;
    JP.TransWvl = TransWvlRef;
    JP.ExpTime_eff = ExpTime_eff;
    JP.Aperture    = Aperture;
    JP.Ntotal      = numel(JP.Flux);

    if Args.Verbose
        fprintf('  Joint pool: %d calibrators across %d crops\n', ...
                JP.Ntotal, sum(NonEmpty));
    end
end
