function [BestScale, BestShift, Matched] = matchLines(ObsLines, RefLines, Args)
    % Given a  reference and new line lists, find the best scale and shift and match the lines.
    %     This function calls: imUtil.spec.waveCalib.matchLines_Scale
    %     and imUtil.spec.waveCalib.matchLines_Shift and then use the best
    %     estimated scale and shift to matche the lines and return a list
    %     of matched lines.
    % Input  : - Observed lines [Position, [Intensity]].
    %            If empty, then run in simulation mode.
    %            Default is [].
    %          - Reference lines [Position, [Intenisty]].
    %          * ...,key,val,... 
    %            'ScaleShift' - An optional vector of [Scale Shift] to
    %                   apply to the observed line list prior to matching
    %                   lines. If empty, then will estimate the scale and
    %                   shift using:
    %                   imUtil.spec.waveCalib.matchLines_Scale and
    %                   imUtil.spec.waveCalib.matchLines_Shift
    %                   Use [1 1] to use the line lists as is.
    %                   Default is [].
    %            'StrongestN' - Selected N strongest lines in both lists.
    %                   Used only if the lines intensity is given.
    %                   Default is 30.
    %            'MaxScale' - Max scale to test. Default is 10.
    %            'StepScale' - Step size for scale testing.
    %                   Default is 0.0005.
    %            'Step' - Shift histogram step. Default is 1.
    %            'GaussFilter' - If not empty, then convolve the histograms
    %                   with a Gaussian prior to the cross-correlation.
    %                   The Gaussian sigma width is given by this argument.
    %                   Default is 2.
    %            'MaxDist' - Maximum distance for line matching (in units
    %                   of the reference line list).
    %                   Default is 5.
    % Output : - Best scale.
    %          - Best shift.
    %          - A structure array of the matched lines (element per match).
    %            Fields are:
    %            .Ref - Reference line position.
    %            .Obs - Observed line position.
    %            .ObsTran - Observed line position transformed to the
    %                   reference scale.
    %            .Diff - Diff between Ref and ObsTran.
    % Author : Eran Ofek (2023 Dec) 
    % Example: [BestScale, BestShift, Matched] = imUtil.spec.waveCalib.matchLines

    arguments
        ObsLines                  = [];
        RefLines                  = [];
        Args.ScaleShift           = [];
        Args.StrongestN           = 30;
        
        Args.MaxScale             = 10;
        Args.StepScale            = 0.0001;
        Args.Step                 = 1;
        Args.GaussFilter          = 2;
        
        Args.MaxDist              = 5; % in reference wavelength units
    end    
    
    if isempty(ObsLines)
        % simulation mode
    
        fprintf('Simulation mode\n');
        
        %
        Nl         = 55;
        Noverlap   = 45;
        Nnoise     = 10;
        ObsLines   = rand(Nl,1).*3000 + 3000;
        NoiseLines = rand(Nnoise,1).*3000 + 3000;
        
        Ir       = randi(Nl, Noverlap,1);
        RefLines = [ObsLines(Ir); NoiseLines].*3.27 + 1500;
        ObsLines = ObsLines + randn(size(ObsLines,1),1).*1;
        %
        
    end
    
    if isvector(ObsLines) 
        ObsLines = ObsLines(:);
        Nobs     = numel(ObsLines);
        ObsLines = [ObsLines, nan(Nobs,1)];
        SelectedObsLines = ObsLines;
    else
        Nobs = size(ObsLines,1);
        
        if Nobs>Args.StrongestN
            ObsLines = sortrows(ObsLines, 2, 'descend');
            SelectedObsLines = ObsLines(1:Args.StrongestN,:);
        else
            SelectedObsLines = ObsLines;
        end
    end
    ObsLines = sortrows(ObsLines,1);
    SelectedObsLines = sortrows(SelectedObsLines,1);
    
    if isvector(RefLines) 
        RefLines = RefLines(:);
        Nref     = numel(RefLines);
        RefLines = [RefLines, nan(Nref,1)];
        SelectedRefLines = RefLines;
    else
        Nref = size(RefLines,1);
        
        if Nref>Args.StrongestN
            RefLines = sortrows(RefLines, 2, 'descend');
            SelectedRefLines = RefLines(1:Args.StrongestN,:);
        else
            SelectedRefLines = RefLines;
        end
    end
    RefLines = sortrows(RefLines,1);
    SelectedRefLines = sortrows(SelectedRefLines,1);
    
    
    % find scale and shift:
    if isempty(Args.ScaleShift)
        [BestScale] = imUtil.spec.waveCalib.matchLines_Scale(SelectedObsLines(:,1), SelectedRefLines(:,1),...
                                    'MaxScale',Args.MaxScale, 'StepScale',Args.StepScale, 'GaussFilter',Args.GaussFilter);
        SelectedObsLinesScaled = SelectedObsLines(:,1).*BestScale;
        [BestShift] = imUtil.spec.waveCalib.matchLines_Shift(SelectedObsLinesScaled, SelectedRefLines(:,1), 'Step',Args.Step, 'GaussFilter',Args.GaussFilter);
    else
       BestScale = Args.ScaleShift(1);
       BestShift = Args.ScaleShift(2);
    end
    
    if nargout>2
        ObsLinesTrans = ObsLines.*BestScale + BestShift;

        % match the lines ObsLinesTrans vs. RefLines
        Matched = struct('Ref',[], 'Obs',[], 'ObsTran',[], 'Diff',[]); %, 'Iref',[], 'Inew',[]);
        K = 0;
        for Iref=1:1:Nref
            [MinDist, MinInd] = min(abs(RefLines(Iref,1) - ObsLinesTrans(:,1)));
            if MinDist<Args.MaxDist
                % match found
                K = K + 1;
                Matched(K).Ref     = RefLines(Iref);
                Matched(K).Obs     = ObsLines(MinInd);
                Matched(K).ObsTran = ObsLinesTrans(MinInd);
                Matched(K).Diff    = RefLines(Iref,1) - ObsLinesTrans(MinInd);
                %Matched(K).Iref    = Iref;
                %Matched(K).Inew    = MinInd;
            end
        end
    end
    
end
