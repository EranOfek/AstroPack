function ModelPars = fitTransmissionPars(ModelPars, ObsData, StepNum, Args)
    % fit model parameters parameters to data 
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Dana Kovaleva (2025 Sep) 
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
    
    MinimizerName   = ModelPars.OptimizationScenario{StepNum,42};
    MinimizerHandle = str2funct(MinimizerName);
    
    % 2. Call the minimizer    
    for Iter = 1:NClip
        ModelPars = MinimizerHandle(ModelPars, ObsData); 
    end
end

