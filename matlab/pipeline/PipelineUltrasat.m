
%--------------------------------------------------------------------------


classdef PipelineUltrasat < PipelineComponent
    % ULTRASAT Pipepline Processor - THE Class
    
    
    properties (SetAccess = public)     % General data
        
        filename        
        configPath = "";
        data
        lines
        userData
        
        inputImagePath
        inputImageExt
        
        CurProc = ''
    end
    
    
    properties (SetAccess = public)     % Image data
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
        
        function setProc(Obj, ProcName)
            % Set current process name
            Obj.CurProc = ProcName;
            Obj.msgLog(LogLevel.Info, 'Proc: %s', Obj.CurProc);
        end
        
        
        function Result = process(Obj)
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
        
        
        
        %==================================================================
        %                       Per-Step Functions
        %==================================================================
        
        function Result = proc_PreProcess(Obj)
            Result = false;
        end
        
   
        function Result = proc_PrepareInputData(Obj)
            % Prep raw image as fits
            Result = false;
        end        
                
        
        function Result = proc_UpdateHeader(Obj)
            % Update header
            Result = false;
        end             
        

        function Result = proc_Partitioning(Obj)
            % Partitioning w/ overlap (CCDSEC..)
            Result = false;
        end
            
                        
        function Result = proc_CreateMask(Obj)
            % Create mask
            Result = false;
        end

        
        function Result = proc_SetSaturatedPixels(Obj)            
            % Set saturated pixels
            Result = false;
        end            
        
           
        function Result = proc_Linearization(Obj)            
            % Linearization
            Result = false;
        end            

        
        function Result = proc_Gain1(Obj)            
            % Gain->1
            Result = false;
        end

                        
        function Result = proc_deBias(Obj)
            % de Bias                      
            Result = false;
        end
            
            
        function Result = proc_deDark(Obj)
            % de Dark
            Result = false;
        end

                        
        function Result = proc_CrossTalk(Obj)
            % Cross-talk (?)
            Result = false;
        end

            
        function Result = proc_deFringing(Obj)
            % de Fringing (?)
            Result = false;
        end
            
        
        function Result = proc_deFlat(Obj)
            % de Flat
            Result = false;
        end

        
        function Result = proc_SuperFlat(Obj)
            % Super flat            
            Result = false;
        end

            
        function Result = proc_StarFlat(Obj)
            % Star flat
            Result = false;
        end

            
        function Result = proc_CalcBack(Obj)
            % Calc Back
            Result = false;
        end
        
        
        function Result = proc_CalcVar(Obj)
            % Calc Var            
            Result = false;
        end
        

        function Result = proc_SourcesSNR(Obj)
            % Sources SNR > 20
            Result = false;
        end
        

        function Result = proc_PSF_Estimation(Obj)
            % PSF estimation
            Result = false;
        end

                   
        function Result = proc_SourceExtraction(Obj)
            % Source extraction + DaoPhot (?)
            Result = false;
        end

            
        function Result = proc_Catalogs(Obj)
            % Catalogs
            Result = false;
        end

        
        function Result = proc_Astrometry(Obj)
            % Astrometry            
            Result = false;
        end

            
        function Result = proc_PhotCalib(Obj)
            % Phot calib.
            Result = false;
        end
        

        function Result = proc_CatHeader(Obj)
            % Cat, header
            Result = false;
        end
        
            % Match image to Ref (?):
            % Phot, astrometry,
            % rotation (?)
        function Result = process(Obj)
            Result = false;
        end

            
        function Result = process(Obj)
            % Robust Coadd x3,...
            Result = false;
        end

            
        function Result = proc_TranslateToRefGrid(Obj)
            % Translate to ref grid (?)
            Result = false;
        end

            
        function Result = proc_Subtraction(Obj)
            % Subtraction
            Result = false;
        end


            %  CR interpolated image (?) from scratch, but
            % add columns/rows


            % Cat of transients, CR and streaks
        function Result = process(Obj)
            Result = false;
        end

        
        function Result = proc_ForcedPhotometry(Obj)
            % Forced photometry
            Result = false;
        end

                
        function Result = process(Obj)
                % Coadditon on subs x3,x..
            Result = false;
        end


        function Result = proc_SearchTransients(Obj)
            % Search transients            
            Result = false;
        end


        function Result = proc_SelectClassTransients(Obj)
            % Select/class transients            
            Result = false;
        end
                
    end    

    
    % Unit test
    methods(Static)   
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'PipelineUltrasat test started\n');
            
            Pipeline = PipelineUltrasat;
                        
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'PipelineUltrasat test passed')
            Result = true;            
        end
    end    
    
end
