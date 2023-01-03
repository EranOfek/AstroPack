% Pipeline - 
% Properties :
%       
% Functionality :
%

classdef Pipeline < Component
    properties (SetAccess = public)
        CI CalibImages    
        StampSize         = [];
    end
    
    methods % Constructor
       
    end
    
    methods % Setters/Getters
    
    end
    
    methods (Static)  % static methods
        function [List, ImagePath, FullPathList] = prepImagesList(List, Args)
            % Prepare list of images to 
            
            arguments
                List                        = '*.fits';
                Args.ImagesPath             = @pipeline.last.constructCamDir;  % bias images are in this dir ('.'=current dir)
                Args.ImagePathArgs          = {1,'Node',1, 'SubDir','new', 'ProjNamebase','LAST'};
                Args.FileNameType           = 'sci';
                Args.UseFileNames logical   = true;
            end
            
            if isa(List,'AstroImage')
                % do nothing - List is an AstroImage
            else
                % identify bias/dark image by type
                if Args.UseFileNames
                    % use FileNames class
                    FN = FileNames.generateFromFileName(List);
                    [FN,Flag] = selectBy(FN, 'Type', Args.FileNameType, 'CreateNewObj',false);
                    List = FN.genFile;
                else
                    % select files
                    List = io.files.filelist(List);
                    % search for subs tring in file names
                    Flag = contains(List, Args.FileNameType);
                    List = List(Flag);
                end
                                                
            end
            
            if isa(Args.ImagesPath, 'function_handle')
                ImagePath = Args.ImagesPath(Args.ImagePathArgs{:});
            else
                ImagePath = Args.ImagesPath;
            end
            
            if nargout>2
                FullPathList = fullfile(ImagePath, List);
            end
            
        end
        
    end
    
    methods % Prepare pipeline for executation
        function Obj = loadCalibImages(Obj, Args)
            %
            
            arguments
                Obj
                Args.CalibDir = [];
            end
            
        end
        
    end
    
    methods % generic pipelines
        
        
        % prep dark/bias
        function Obj=createBias(Obj, List, Args)
            %
            
            arguments
                Obj
                List                          = '*.fits'; %[];  % pass a CalibImages object
                Args.ImagesPath             = @pipeline.last.constructCamDir;  % bias images are in this dir ('.'=current dir)
                Args.ImagePathArgs          = {1,'Node',1, 'SubDir','new', 'ProjNamebase','LAST'};
                Args.FileNameType           = 'dark';
                Args.UseFileNames logical   = true;
                Args.UseConfigArgs logical  = true;
                Args.ArgsCreateBias         = {};
            end
                        
            
            
            PWD = pwd;
            cd(ImagePath);
            % prep bias/dark

            Obj.CI = CalibImages;
            Obj.CI = createBias(Obj.CI, List, Args.ArgsCreateBias{:});
            
            cd(PWD);
        end
        
        % prep flat
        
        
        % getSources (bias, flat,..., back, sources, astrometry)
        
        % match sources
        
        % coadd
    end

    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    

end

           
