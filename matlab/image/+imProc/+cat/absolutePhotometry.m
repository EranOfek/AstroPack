function [Result] = absolutePhotometry(AI, Args)
    % calculate and apply absolute photometry corrections 
    %     Optional detailed description
    % Input  : - an AstroImage array with catalogs and headers    
    %          * ...,key,val,... 
    %          'TransmissionFunctions'   - names of transmission functions (processes) to be applied 
    %          'TransmissionFunctionPar' - parameters of transmission functions: values, freeze/thaw and bounds 
    %          'OptimizationScenario'    - sequence of optimization steps and their parameters 
    % Output : - new columns in AI catalogs with updated ZPs (and magnitudes)  
    % Author : Dana Kovaleva (2025 Sep) 
    % Example: imProc.cat.absolutePhotometry(Coadd);
    %
    arguments
        AI
        Args.TransmissionFunctions    = {'name1','name2'};
        Args.TransmissionFunctionPar  = {'par1','par2'};        
        Args.OptimizationScenario     = {'step1','step2'};        
    end

    % 0. load common data and constants 
    
    ModelPars = Args;    
        
    % 1. find and select calibrators for field sources:
    %    read the source fluxes and image metadata from AI, 
    %    and selected calibrator fluxes from CatsHTM
    
%     ObsData   = 
        
    for Crop = 1:numel(AI)
        % 2. sequence of minimizer calls as determined by Args.OptimizationScenario
        for Step = 1:numel(Args.OptimizationScenario)
            % at each call the input of the minimizer consists of
            % model parameters: values, freeze/thaw and bounds
            % updated by the previous call (or set at the beginning)
            
            ModelPars = astro.transmission.fitTransmissionPars(ModelPars, ObsData, Step);
        end
        
        % 3. calculate final ZPs and mags from the optimized model parameters
        
        % 4. write of the new ZPs and mags into AI.CatData tables
        
    end
end
