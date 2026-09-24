function ModelPars = fitTransmissionPars(ModelPars, ObsData, StepNum, Args)
    % fit model parameters to data 
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : A. Krassilschikov, D. Kovaleva (Oct 2025) 
    % Example: 

    arguments
        ModelPars
        ObsData
        StepNum        
        Args.B    = [];
    end
    
    % 1. According to StepNum number, read from ModelPars.OptimizationScenario:
    % -- determine minimizer name (= type)
    % -- mark thawed parameters
    % -- type and number of clippings (NClip)
    
    MinimizerName   = ModelPars.OptimizationScenario{StepNum,42}; % minimizer function-to-be: tools.math.fit.lsqNonLinWithFixed
    MinimizerHandle = str2funct(MinimizerName);
    
    % 2. Call the minimizer    
    for Iter = 1:NClip
        ModelPars = MinimizerHandle(ModelPars, ObsData); 
    end
end

