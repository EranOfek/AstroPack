function V = resolvePCParam(PCobj, Name)
    % Resolve a named photometric-calibration quantity from a PhotCalibTrans
    %   Returns a scalar numeric value for any of the supported names.
    %   Returns NaN if the name is not recognized or the underlying field
    %   is missing/empty. Used by plotPhotParamScatter, plotPhotParamHist
    %   and any other diagnostic tool that needs to extract one named
    %   quantity per PC.
    %
    %   Resolution order:
    %     (a) fitted transmission parameter (TransModel.getAllFunPar.Name)
    %         e.g. 'TauAod500','PWV_cm','Center_Ang','Norm', ...
    %     (b) TransModel scalar fit-quality fields:
    %         'RMS','Chi2','DOF', plus derived 'Chi2_DOF' = Chi2/DOF.
    %         'DOF' returns TransModel.DOF as-is (currently the last-stage
    %         local DOF; will become the global DOF once PhotCalibTrans
    %         stores it directly).
    %     (b2) 'IntegralT' - integral transmission (mean fractional
    %          throughput including Norm), via
    %          PhotCalibTrans.integralTransmission.
    %     (c) PhotCalibTrans scalar properties (header-derived or metadata):
    %         'AirMass','Temp','Pressure','Humidity','ExpTime','NCoadd',
    %         'ARMS','DeltaZP_CB','Aperture'
    %     (d) per-crop SourceData summaries:
    %         'NSources'        - height of SourceData.Table
    %                             (initial calibrator pool BEFORE sigma clipping)
    %         'NCalib'          - calibrators retained AFTER sigma clipping
    %                             = sum(Used) [or NSources if Used absent]
    %         'NCalibAbs'       - synonym of 'NCalib' (absolute count of
    %                             used calibrators; companion to NCalibRetention).
    %         'NCalibRetention' - sum(Used) / NSources, in [0, 1]
    %         'MedResidual'     - median of Residuals over Used
    %         'StdResidual'     - std    of Residuals over Used
    %         'MedMagErr'       - median of MagErr over Table
    %
    % Input  : - Single PhotCalibTrans object.
    %          - Name (char) - quantity name (case-sensitive).
    % Output : - V - scalar numeric value, or NaN if unresolved.
    % Author : D. Kovaleva (Apr 2026)
    % Example: V = resolvePCParam(PC(5), 'Chi2_DOF');   % private helper

    arguments
        PCobj   PhotCalibTrans
        Name    char
    end

    V = NaN;

    % integral transmission (derived; mean fractional throughput incl. Norm)
    if strcmp(Name, 'IntegralT')
        try
            T = PCobj.integralTransmission();
            if isnumeric(T) && isscalar(T)
                V = T;
            end
        catch
        end
        return;
    end

    % (a) fitted transmission parameter
    if ~isempty(PCobj.TransModel)
        try
            P = PCobj.TransModel.getAllFunPar();
            Idx = find(strcmp(P.Name, Name), 1);
            if ~isempty(Idx)
                V = P.Val(Idx);
                return;
            end
        catch
        end

        % (b) TransModel scalar fit-quality fields
        if ismember(Name, {'RMS', 'Chi2', 'DOF'}) && isprop(PCobj.TransModel, Name)
            V = PCobj.TransModel.(Name);
            return;
        end
        if strcmp(Name, 'Chi2_DOF')
            Chi2 = PCobj.TransModel.Chi2;
            DOF  = PCobj.TransModel.DOF;
            if ~isnan(Chi2) && ~isnan(DOF) && DOF > 0
                V = Chi2 / DOF;
            end
            return;
        end
    end

    % (c) PhotCalibTrans scalar properties (header-derived or metadata)
    ScalarProps = {'AirMass','Temp','Pressure','Humidity','ExpTime', ...
                   'NCoadd','ARMS','DeltaZP_CB','Aperture'};
    if ismember(Name, ScalarProps) && isprop(PCobj, Name)
        Tmp = PCobj.(Name);
        if isnumeric(Tmp) && isscalar(Tmp)
            V = Tmp;
        end
        return;
    end

    % (d) per-crop SourceData summaries
    if ~isempty(PCobj.SourceData) && isprop(PCobj.SourceData, 'Catalog')
        Tab = PCobj.SourceData.Table;
        if ~isempty(Tab)
            Cols = Tab.Properties.VariableNames;
            switch Name
                case 'NSources'
                    V = height(Tab);
                case {'NCalib', 'NCalibAbs'}
                    % Absolute count of calibrators retained AFTER sigma
                    % clipping = sum(Used). 'NCalibAbs' is the absolute
                    % counterpart to the fraction 'NCalibRetention'.
                    if ismember('Used', Cols)
                        V = sum(logical(Tab.Used));
                    else
                        V = height(Tab);
                    end
                case 'NCalibRetention'
                    % Fraction retained after sigma clipping
                    %   = sum(Used) / height(SourceData.Table)
                    % (NCalib_used / NCalib_initial). In [0, 1].
                    if ismember('Used', Cols) && height(Tab) > 0
                        V = sum(logical(Tab.Used)) / height(Tab);
                    end
                case 'MedResidual'
                    if ismember('Residuals', Cols)
                        R = Tab.Residuals;
                        if ismember('Used', Cols); R = R(logical(Tab.Used)); end
                        V = median(R, 'omitnan');
                    end
                case 'StdResidual'
                    if ismember('Residuals', Cols)
                        R = Tab.Residuals;
                        if ismember('Used', Cols); R = R(logical(Tab.Used)); end
                        V = std(R, 0, 'omitnan');
                    end
                case 'MedMagErr'
                    if ismember('MagErr', Cols)
                        V = median(Tab.MagErr, 'omitnan');
                    end
            end
        end
    end
end
