
%--------------------------------------------------------------------------


classdef PipelineUltrasat < PipelineComponent
    % Properties
    properties (SetAccess = public)
        
        filename        
        configPath = "";
        data
        lines
        userData
        
        inputImagePath
        inputImageExt
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = PipelineUltrasat()
            Obj.setName('PipelineUltrasat');
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);            
        end
    end

    
    % 
    methods
        function Result = process()
            % ULTRASAT Pipeline Processing, from Pipeline_Data_flow_chart.pdf
            
            % Inputs: Raw image, Header, Telemetry
            
            % Prep raw image as fits
            % Update header


            % Partitioning w/ overlap (CCDSEC..)


            % Create mask
            % Set saturated pixels


            % Linearization
            % Gain->1


            % de Bias                      
            % de Dark

            
            % Cross-talk (?)

            % de Fringing (?)

            % de Flat

            % Super flat

            % Star flat


            % Calc Bck, VAR


            % Sources SNR > 20

            
            % PSF estimation
            

            % Source extraction + DaoPhot (?)

            % Catalogs

            % Astrometry

            % Phot calib.
            % Cat, header


            % Match image to Ref (?):
            % Phot, astrometry,
            % rotation (?)


            % Robust Coadd x3,...

            % Translate to ref grid (?)

            % Subtraction


            %  CR interpolated image (?) from scratch, but
            % add columns/rows


            % Cat of transients, CR and streaks


            % Forced photometry

            % Coadditon on subs x3,x..


            % Search transients


            % Select/class transients


            Result = true;
        end
    end    

    
    % Unit test
    methods(Static)   
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'FileProcessor test started\n');
            
            Proc = FileProcessor;
            Proc.inputLoop(100);
            
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'FileProcessor test passed')
            Result = true;            
        end
    end    
    
end


