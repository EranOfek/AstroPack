function [ResZP, MatchedS, ApplyToMagField] = relZPfit(MatchedS, Args)
    %  Perform and apply relative photometry to a MatchedSources object.
    % Input  : - A MatchedSources object.
    %          * ...,key,val,... 
    %            'RelPhotAlgo' - Relative photometry algorithm:
    %                   'meddiff' - call lcUtil.zp_meddiff
    %                   'lsq' - call lcUtil.zp_lsq
    %            'MagCalibColName' - A char array of column name by which to
    %                   calculate the relative photometric calibration.
    %                   Default is 'MAG_APER_3'.
    %            'MagCalibErrColName' - Error column name corresponding to
    %                   'MagCalibColName'. Default is 'MAGERR_APER_3'.
    %            --- specific algo arguments ---
    %            'zp_meddiffArgs' - A cell array of additional arguments to
    %                   pass to lcUtil.zp_meddiff.
    %                   Default is {}.
    %            'zp_lsqArgs' - A cell array of additional arguments to
    %                   pass to lcUtil.zp_lsq.
    %                   Default is {}.
    %            'ApplyToMagField' - If not empty, then will apply relative
    %                   zero points to magnitudes in the MatchedSources
    %                   object. A char or cell array.
    %                   If char, then will first search for all
    %                   field names in the first element of the
    %                   MatchedSources object that contains this
    %                   substring. All the releveant Ifields will be
    %                   put in a cell array. Will apply the ZP for
    %                   all fields in the cell array.
    %                   Default is 'MAG_'.
    %            'Operator' - A function handke with operator for
    %                   the ZP. E.g., NewMatrix = operator(Matrix, ZP)
    %                   Default is @minus.
    %            'UseMex' - A logical indicating if to use MEX when
    %                   possible. Default is false.
    %
    % Output : - A structure array (element per MatchedSources element),
    %            with the relative photometry results and zero points.
    %          - An updated MatchedSources object.
    %          - The magnitude fields for which the zero points were
    %            applied.
    % Author : Eran Ofek (2025 Oct) 
    % Example: 

    arguments
        MatchedS
        Args.RelPhotAlgo        = 'meddiff';
        Args.MagCalibColName    = 'MAG_APER_3';
        Args.MagCalibErrColName = 'MAGERR_APER_3';

        Args.zp_meddiffArgs     = {};
        Args.zp_lsqArgs         = {};

        Args.ApplyToMagField    = 'MAG_';
        Args.Operator           = @minus;

        Args.UseMex             = false;
    end

    ApplyToMagField = [];

    Nobj = numel(MatchedS);
    for Ifields=1:1:Nobj
        switch lower(Args.RelPhotAlgo)
            case 'meddiff'
                %error('meddff is not available yet');
                [ResZP(Ifields)] = lcUtil.zp_meddiff(MatchedS(Ifields),...
                                        Args.zp_meddiffArgs{:},...
                                        'MagField',Args.MagCalibColName,...
                                        'MagErrField',Args.MagCalibErrColName,...
                                        'UseMex',Args.UseMex);
            case 'lsq'
                warning('apply ZP in this case is partial in code');
                [ResZP(Ifields), MatchedS(Ifields)] = lcUtil.zp_lsq(MatchedS(Ifields),...
                                                                Args.zp_lsqArgs{:},...
                                                                'MagField',Args.MagCalibColName,...
                                                                'MagErrField',Args.MagCalibErrColName);
            otherwise
                error('Unknown RelPhotAlgo option');
        end
    
        if ~isempty(Args.ApplyToMagField)
            % apply ZP to all Magnitudes...
            [MatchedS(Ifields) ,ApplyToMagField] = applyZP(MatchedS(Ifields), ResZP(Ifields).FitZP,...
                'FieldZP','FitZP',...
                'ApplyToMagField','MAG_',...
                'Operator',Args.Operator);
            
        end
    end

end
