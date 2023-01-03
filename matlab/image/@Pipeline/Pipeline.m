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
        function [List, ImagePath, FullPathList, FN] = prepImagesList(List, Args)
            % Prepare list of images of some Type
            % Input  : - A cell array of images, or a char array with image
            %            name wild cards template.
            %            Default is '*.fits'.
            %          * ...,key,val,...
            %            'ImagesPath' - Either a char array with image path
            %                   name, or a function handle that generate the path.
            %                   Default is @pipeline.last.constructCamDir
            %            'ImagePathArgs' - A cell array of arguments to
            %                   pass to the ImagePath function.
            %                   Default is {1,'Node',1, 'SubDir','new',
            %                   'ProjNamebase','LAST'}.
            %            'FileNameType' - File type to select.
            %                   Default is 'sci'.
            %            'UseFileNames' - A logical indicating if to use
            %                   the FileNames class. Default is true.
            % Output : - A cell array of file names.
            %          - A char array with files path.
            %          - A cell array of full file names.
            %          - A FileNames object containing the files.
            % Author : Eran Ofek (Jan 2023)
            % Example: [List, ImagePath, FullPathList,FN] = Pipeline.prepImagesList('LAST*.fits');
            
            arguments
                List                        = '*.fits';
                Args.ImagesPath             = @pipeline.last.constructCamDir;  % bias images are in this dir ('.'=current dir)
                Args.ImagePathArgs          = {1,'Node',1, 'SubDir','new', 'ProjNamebase','LAST'};
                Args.FileNameType           = 'sci';
                Args.UseFileNames logical   = true;
            end
            
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

            
            if isa(Args.ImagesPath, 'function_handle')
                ImagePath = Args.ImagesPath(Args.ImagePathArgs{:});
            else
                ImagePath = Args.ImagesPath;
            end
            
            if nargout>2
                FullPathList = fullfile(ImagePath, List);
                
                if nargout>3 && ~Args.UseFileNames
                    FN = FileNames(List);
                end
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

           
