
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
        
        % Input data
        ImageFileName
        HeaderFileName
        Image
        Header
        Telemetry
        
        % Output data
        Catalog
        
        FArgs
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

    
    %======================================================================
    methods
        
        
        function Result = processInputImage(Obj, ImageFileName, HeaderFileName, Args)
            % Pipeline entry point
            arguments
                Obj                
                ImageFileName
                HeaderFileName
                Args.TelemetryFileName
            end
            
            % Set data
            Obj.ImageFileName = ImageFileName;
            Obj.HeaderFileName = HeaderFileName;
            
            % Process
            try
                % Call the first process, everything will continue from
                % there
                Obj.proc_start();
            catch
                Obj.msgLog(LogLevel.Error, 'Exception');
            end
            
            % 
            %Obj.proc_end();
            
            Result = true;                
        end
        

        function Result = process(Obj, FuncName, FArgs)
            % Run pipeline function by its name
            
            % Prepare data
            Obj.FArgs = FArgs;            
            
            % Prepare function string            
            Func = ['Result = Obj.' FuncName '();']
            Obj.msgLog(LogLevel.Info, 'calling: %s', Func);
            eval(Func);
        end
        
    
        function Result = proc_start(Obj)
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
        
        
        function Result = proc_end(Obj)
        end
        
        %==================================================================
        %                       Per-Step Functions
        %==================================================================
        
        function Result = proc_PreProces(Obj)
            Obj.setProc('PreProces');
            Result = false;
        end
        
   
        function Result = proc_PrepareInputData(Obj)
            % Prep raw image as fits
            Obj.setProc('');
            Result = false;
        end        
                
        
        function Result = proc_UpdateHeader(Obj)
            % Update header
            Obj.setProc('');
            Result = false;
        end             
        

        function Result = proc_Partitioning(Obj)
            % Partitioning w/ overlap (CCDSEC..)
            Obj.setProc('');
            Result = false;
        end
            
                        
        function Result = proc_CreateMask(Obj)
            % Create mask
            Obj.setProc('');
            Result = false;
        end

        
        function Result = proc_SetSaturatedPixels(Obj)            
            % Set saturated pixels
            Obj.setProc('');
            Result = false;
        end            
        
           
        function Result = proc_Linearization(Obj)            
            % Linearization
            Obj.setProc('');
            Result = false;
        end            

        
        function Result = proc_Gain1(Obj)            
            % Gain->1
            Obj.setProc('');
            Result = false;
        end

                        
        function Result = proc_deBias(Obj)
            % de Bias                      
            Obj.setProc('');
            Result = false;
        end
            
            
        function Result = proc_deDark(Obj)
            % de Dark
            Obj.setProc('');
            Result = false;
        end

                        
        function Result = proc_CrossTalk(Obj)
            % Cross-talk (?)
            Obj.setProc('');
            Result = false;
        end

            
        function Result = proc_deFringing(Obj)
            % de Fringing (?)
            Obj.setProc('');
            Result = false;
        end
            
        
        function Result = proc_deFlat(Obj)            
            % de Flat
            Obj.setProc('');
            Result = false;
        end

        
        function Result = proc_SuperFlat(Obj)
            % Super flat            
            Obj.setProc('');
            Result = false;
        end

            
        function Result = proc_StarFlat(Obj)
            % Star flat
            Obj.setProc('');
            Result = false;
        end

            
        function Result = proc_CalcBack(Obj)
            % Calc Back
            Obj.setProc('');
            Result = false;
        end
        
        
        function Result = proc_CalcVar(Obj)
            % Calc Var            
            Obj.setProc('');
            Result = false;
        end
        

        function Result = proc_SourcesSNR(Obj)
            % Sources SNR > 20
            Obj.setProc('');
            Result = false;
        end
        

        function Result = proc_PSF_Estimation(Obj)
            % PSF estimation
            Obj.setProc('');
            Result = false;
        end

                   
        function Result = proc_SourceExtraction(Obj)
            % Source extraction + DaoPhot (?)
            Obj.setProc('');
            Result = false;
        end

            
        function Result = proc_Catalogs(Obj)
            % Catalogs
            Obj.setProc('');
            Result = false;
        end

        
        function Result = proc_Astrometry(Obj)
            % Astrometry            
            Obj.setProc('');
            Result = false;
        end

            
        function Result = proc_PhotCalib(Obj)
            % Phot calib.
            Obj.setProc('');
            Result = false;
        end
        

        function Result = proc_CatHeader(Obj)
            % Cat, header
            Obj.setProc('');
            Result = false;
        end
        
            % Match image to Ref (?):
            % Phot, astrometry,
            % rotation (?)
        function Result = proc_Match(Obj)
            Obj.setProc('');
            Result = false;            
        end

            
        function Result = proc_RobustCoadd(Obj)
            % Robust Coadd x3,...
            Obj.setProc('');
            Result = false;
        end

            
        function Result = proc_TranslateToRefGrid(Obj)
            % Translate to ref grid (?)
            Obj.setProc('');
            Result = false;
        end

            
        function Result = proc_Subtraction(Obj)
            % Subtraction
            Obj.setProc('');
            Result = false;
        end


            %  CR interpolated image (?) from scratch, but
            % add columns/rows


            % Cat of transients, CR and streaks
        function Result = processCat(Obj)
            Obj.setProc('');
            Result = false;
        end

        
        function Result = proc_ForcedPhotometry(Obj)
            % Forced photometry
            Obj.setProc('');
            Result = false;
        end

                
        function Result = processCoaddOnSubs(Obj)
            % Coadditon on subs x3,x..
            Obj.setProc('');
            Result = false;
        end


        function Result = proc_SearchTransients(Obj)
            % Search transients            
            Obj.setProc('');
            Result = false;
        end


        function Result = proc_SelectClassTransients(Obj)
            % Select/class transients            
            Obj.setProc('');
            Result = false;
        end
                
    end    

    %======================================================================
    methods % Helper functions
        function setProc(Obj, ProcName)
            % Set current process name and details
            Obj.CurProc = ProcName;
            Obj.msgLog(LogLevel.Info, 'Proc: %s', Obj.CurProc);
        end
    end


    % Unit test
    methods(Static)   
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'PipelineUltrasat test started\n');
            
            pipe = PipelineUltrasat;
            ImageFileName = '';
            HeaderFileName = '';
            
            pipe.processInputImage(Obj, ImageFileName, HeaderFileName);
                        
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'PipelineUltrasat test passed')
            Result = true;            
        end
    end    
    
end
