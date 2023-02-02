% Pipeline - 
%   The Pipeline class contains tools to perform blocks of image reduction
%   steps. The tools are excslusively works with FileNames, AstroImage, and
%   CalibImages objects.
% Properties :
%       
% Functionality :
%

% Wish list:
% readCalib
% prepCalib
% applyCalib (including calib selection)
% singleRaw2proc
% raw2proc
% multiRaw2procCoadd
% forcePhot (including moving)

classdef Pipeline < Component
    properties (SetAccess = public)
        CI      % either CalibImages object or directory path
        CroppedCI  % do we need this?
        Input   % AstroImage or list of image names
        Output  %
        CCDSEC    = [];  % [] for entire image
        
%%%% change name to PipelineOp

        % ???
        ImagesPath         = @pipeline.last.constructCamDir;  % bias images are in this dir ('.'=current dir)
        ArgsImagesPath     = {1,'Node',1, 'SubDir','new', 'ProjNamebase','LAST'};
        CalibPath          = @pipeline.last.constructCamDir;  % bias images are in this dir ('.'=current dir)
        ArgsCalibPath      = {1,'Node',1, 'SubDir','calib', 'ProjNamebase','LAST'};
        
        BasePath           
        
        InputArgs
        
        StampSize         = [];
    end
    
    methods % Constructor
       
    end
    
    methods % Setters/Getters
        % 
    end
    
    % object can be a vector! so running the pipelines will run on all???

    % methods:
    %   load
    %   save
    %   writeDB
    %   raw2proc
    %       dark, linearization, flat, fringe, gain, sources, astrometry+add2cat,
    %       matchExternalCal, matchSolarSystem
    %           Need: guess WCS parameter...
    %   coadd
    %   diff
    %   mergeTable
    %   mergeMatrix
    %   forcedPhot


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
            %            'ArgsImagePath' - A cell array of arguments to
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
                Args.ArgsImagePath          = {1,'Node',1, 'SubDir','new', 'ProjNamebase','LAST'};
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
                ImagePath = Args.ImagesPath(Args.ArgsImagePath{:});
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
            % Load the CalibImages object into the Pipeline object
            % Input  : - A Pipeline object.
            %          * ...,key,val,...
            %            'CalibDir' - Path containing the calibration
            %                   images. Default is {}.
            %            'loadFromDirArgs' - A cell array of additional key/val
            %                   arguments to pass to the CalibImages.loadFromDir
            %                   function. If empty, then attempt to
            %                   construct the dir using the CalibPath and
            %                   ArgsCalibPath object properties.
            %                   Default is {}.
            % Output : - A Pipeline object in which the CI property is
            %            populated with CalibImages object.
            % Author : Eran Ofek (Jan 2023)
            
            arguments
                Obj(1,1)
                Args.CalibDir        = [];
                Args.loadFromDirArgs = {};
            end
            
            if isempty(Args.CalibDir)
                if isa(Obj.CalibPath, 'function_handle')
                    Args.CalibDir = Obj.CalibPath(Obj.ArgsCalibPath{:});
                else
                    % CalibPath property contains a char array of path
                    Args.CalibDir = Obj.CalibPath;
                end
            end        
                
            Obj.CI = CalibImages.loadFromDir(Args.CalibDir, Args.loadFromDirArgs{:});
            
        end
        
        
        
    end
    
    methods % calibration prep
        
        
        % prep dark/bias
        function Obj=createBias(Obj, List, Args)
            % Pipeline: Create master bias images
            %   Search for bias images
            %   Create master bias image for each night/ExpTime/etc.
            %   Write the master bias images in output directory
            %   Store the master bias images in CI property.
            % Input  : - A Pipeline object.
            %          - List of images from which to generate master bias.
            %            This can be a cell array or char array, and the
            %            list is generated using: Pipeline.prepImagesList
            %            Default is '*.fits'.
            %          * ...,key,val,...
            %            'ImagesPath' - Either a char array with image path
            %                   name, or a function handle that generate the path.
            %                   Default is @pipeline.last.constructCamDir
            %            'ArgsImagePath' - A cell array of arguments to
            %                   pass to the ImagePath function.
            %                   Default is {1,'Node',1, 'SubDir','new',
            %                   'ProjNamebase','LAST'}.
            %            'FileNameType' - File type to select.
            %                   Default is 'dark'.
            %            'UseFileNames' - A logical indicating if to use
            %                   the FileNames class. Default is true.
            %            'BiasArgs' - A cell array of additional arguments
            %                   to pass to the imProc.dark.bias function.
            %                   Default is {}.
            %            'SaveProduct' - Cell array of oroducts to save
            %                   If empty, do not save.
            %                   Default is {'Image','Var','Mask'}.
            %            'FullPath' - Full path of images to save.
            %                   If empty, will be generated using the
            %                   BasePath and FileNames/genPath.
            %                   Default is ''.
            %            'UseSimpeFITS' - A logical indicating if to use 
            %                   FITS.writeSimpleFITS (true) or FITS.write
            %                   (false). Default is true.
            % Output : - A Pipeline object in which the CI property is
            %            populated with the master bias images.
            % Author : Eran Ofek (Jan 2023)
            % Example: 
            
            arguments
                Obj
                List                        = '*.fits'; %[];  % pass a CalibImages object
                Args.ImagesPath             = []; %@pipeline.last.constructCamDir;  % bias images are in this dir ('.'=current dir)
                Args.ArgsImagePath          = {}; % {1,'Node',1, 'SubDir','new', 'ProjNamebase','LAST'};
                Args.FileNameType           = 'dark';
                Args.UseFileNames logical   = true;
                Args.BiasArgs               = {};
                Args.SaveProduct            = {'Image','Var','Mask'};
                Args.FullPath               = '';
                Args.UseSimpleFITS logical  = true;
            end
            
            Nprod = numel(Args.SaveProduct);
            
            % Get default arguments from object property - if not supplied
            % by user
            Args = copyPropNotEmpty(Obj, Args, true);
                        
            [List, ImagePath, FullPathList,FN] = Pipeline.prepImagesList(List, 'ImagePath',Args.ImagePath,...
                                                                               'ArgsImagePath',Args.ArgsImagePath,...
                                                                               'FileNameType',Args.FileNameType);
                                                                             
            
            % seperate List by features
            FJD = floor(FN.julday);
            
            UniqueFloorJD = unique(FJD);
            Nufjd = numel(UniqueFloorJD);
            for Id=1:1:Nufjd
                % prep bias/dark
                Flag = FJD==UniqueFloorJD(Id);
                Obj.CI = CalibImages;
                Bias   = createBias(Obj.CI, List(Flag), 'BiasArgs',Args.BiasArgs);
                % replace bias
                Obj.CI = Bias;
                
                % generate file names
                FNd = reorderEntries(FN, Flag, 'CreateNewObj',true);
                FNd.Level   = 'proc';
                if ~isempty(Args.FullPath)
                    FNd.FullPath = Args.FullPath;
                end
                    
                for Iprod=1:1:Nprod
                    File = FNd.genFull(1, 'Product',Args.SaveProduct{Iprod}, 'Level','proc');
                    % save file
                    if Args.UseSimpleFITS
                        FITS.writeSimpleFITS(Obj.CI(Id).(Args.SaveProduct{Iprod}), 'Header',Obj.CI(Id).Header);
                    else
                        FITS.write1(Obj.CI(Id).(Args.SaveProduct{Iprod}), 'Header',Obj.CI(Id).Header);
                    end
                end
            end
            
        end
        
        % createFlat
    end
    
    methods % pipeline blocks
        % process
        
        % sources
        
        % forcePhot
        
        % astrometry
        
        % match to catalogs        
        
        % coadd
    end
    
    methods % generic pipelines
        % singleRaw
        
        % mutltiRaw
        
        % coaddMerge
        
        
    end

    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    

end

           
