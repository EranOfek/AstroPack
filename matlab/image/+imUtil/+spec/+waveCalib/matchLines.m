function [BestScale, BestShift, Matched] = matchLines(ObsLines, RefLines, Args)
    % Given a  refrence and new line lists, find the best scale and shift and match the lines.
    %     
    % Input  : - Observed lines [Position, [Intensity]].
    %            If empty, then run in simulation mode.
    %            Default is [].
    %          - Reference lines [Position, [Intenisty]].
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2023 Dec) 
    % Example: [BestScale, BestShift, Matched] = imUtil.spec.waveCalib.matchLines

    arguments
        ObsLines                  = [];
        RefLines                  = [];
        Args.StrongestN           = 30;
        
        Args.MaxScale             = 10;
        Args.StepScale            = 0.0001;
        Args.Step                 = 1;
        
        Args.MaxDist              = 10; % in reference wavelength units
    end    
    
    'sometime failed'
    'shift is not accurate enough - maybe edges?'
    
    if isempty(ObsLines)
        % simulation mode
    
        fprintf('Simulation mode\n');
        
        %%
        Nl         = 55;
        Noverlap   = 45;
        Nnoise     = 10;
        ObsLines   = rand(Nl,1).*3000 + 3000;
        NoiseLines = rand(Nnoise,1).*3000 + 3000;
        
        Ir       = randi(Nl, Noverlap,1);
        RefLines = [ObsLines(Ir); NoiseLines].*3.27 + 1500;
        ObsLines = ObsLines + randn(size(ObsLines,1),1);
        %%
        
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
    [BestScale] = imUtil.spec.waveCalib.matchLines_Scale(SelectedObsLines(:,1), SelectedRefLines(:,1), 'MaxScale',Args.MaxScale, 'StepScale',Args.StepScale);
    SelectedObsLinesScaled = SelectedObsLines(:,1).*BestScale;
    [BestShift] = imUtil.spec.waveCalib.matchLines_Shift(SelectedObsLinesScaled, SelectedRefLines(:,1), 'Step',Args.Step);
    
    
    if nargout>2
        ObsLinesTrans = ObsLines.*BestScale + BestShift;

        % match the lines ObsLinesTrans vs. RefLines
        Matched = struct('Ref',[], 'Obs',[], 'ObsTran',[], 'Diff',[], 'Iref',[], 'Inew',[]);
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
                Matched(K).Iref    = Iref;
                Matched(K).Inew    = MinInd;
            end
        end
    end
    
end
