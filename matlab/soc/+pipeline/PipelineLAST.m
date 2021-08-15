

classdef PipelineLAST < PipelineComponent
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
        function Obj = PipelineLAST()
            Obj.setName('PipelineLAST');
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);                        
        end
     

    end

    
    %-------------------------------------------------------- 
    methods

        
        function Result = process(Obj)
            
            % Input is raw image
            if ImType == 'sci'
                
                Obj.processScienseImage()
                
            end
        end
        
        
        function Result = processScienseImage(Obj)
            
            % Input is raw image (image size ~240 MB)
            
            % SubtractDark
            Obj.process_SubtractDark();
            
            % MaskSaturatedPIxels           
            Obj.MaskSaturatedPIxels();
            
            % DivideByFlat
            Obj.process_DivideByFlat();
            
            % Sub images (54)
            Obj.process_SubImages();
            
            % Background estimation
            Obj.process_BackgroundEstimation();
            
            % Source finding
            Obj.process_SourceFinding();
            
            % AstrometrySubImages
            Obj.process_AstrometrySubImages();
            
            % Update WCS, Catalog
            Obj.process_UpdateWcsCatalog();
            
            % PhotometricZeroPoint
            Obj.process_PhotometricZeroPoint();
            
            % Update Catalog
            Obj.process_UpdateCatalog();
            
            % SaveProducts (Image, Catalog)
            Obj.process_saveProducts();
            
        end


        function Result = process_SubtractDark(Obj)            
            
            % SubtractDark
            
        end
        
        
        function Result = process_MaskSaturatedPixels(Obj)                       
            % MaskSaturatedPixels           
            
        end
        
        
        function Result = process_DivideByFlat(Obj)                        
            % DivideByFlat
            
        end
        
        
        function Result = process_SubImages(Obj)                        
            % Sub images (54)
        end
        
        
        function Result = process_BackgroundEstimation(Obj)                        
            % Background estimation
        
        end
        
        
        function Result = process_SourceFinding(Obj)                        
            % Source finding
        end            
        
        
        function Result = process_AstrometrySubImages(Obj)                        
            % AstrometrySubImages
        end
        
        
        function Result = process_UpdateWcsCatalog(Obj)                        
            % Update WCS, Catalog
        end
        
        
        function Result = process_PhotometricZeroPoint(Obj)                        
            % PhotometricZeroPoint
        end
        
        
        function Result = process_UpdateCatalog(Obj)                       
            % Update Catalog
        end            
        
        
        function Result = process_SaveProducts(Obj)                        
            % SaveProducts (Image, Catalog)
            
        end
        
    end
    
    
    %======================================================================
    methods
        function Result = processScienseImageSeries(Obj)
            
            % Registration
            
            % Coadd
            
            % Save Coadd --> Pipeline II
            
            % Analyze 20 cat per SubImage
            
            
        
        
        end
 
        
    end
    
    %======================================================================
    
    
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'PipelineLAST test started')
            
            
            io.msgStyle(LogLevel.Test, '@passed', 'PipelineLAST test passed')                       
            Result = true;
        end
    end    
        
end


