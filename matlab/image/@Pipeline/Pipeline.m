% Pipeline - 
% Properties :
%       
% Functionality :
%

classdef Pipeline < Component
    properties (Dependent) % Access image data directly
        Data
        Var
    end
    
    properties (SetAccess = public)
        DataPSF           = [];   % The fun parameters, or an image
        DataVar           = [];
        Scale             = 1;    % used for consistency only
        FunPSF            = [];   % e.g., Map = Fun(Data, X,Y, Color, Flux)
        ArgVals cell      = {};
        ArgNames cell     = {'X','Y','Color','Flux'};
        StampSize         = [];
    end
    
    methods % Constructor
       
        function Obj = AstroPSF(FileName, Args)
            % AstroPSF constructor - read PSF images to AstroPSF object
            % Input  : - File names.
            %            Either an AstroImage, ImageComponent, SIM, imCl
            %            objects from which the image property will be
            %            stored in the DataPSF property of the AstroPSF
            %            object.
            %            Alternatively, a file name or a cell array of file
            %            names to read into the AstroPSF object.
            %            Or a matrix containing the PSF.
            %          * ...,key,val,...
            %            'HDU' - If file name is a FITS file, then this is
            %                   the HDU. Default is 1.
            %            'FileType' - File type for file names. Default is 'fits'.
            %            'UseRegExp' - Use regular expression for file
            %                   names. Default is false.
            %            'DataVar' - Same as file names, but for the
            %                   variance image of the PSF.
            %            'VarHDU' - Variance image HDU. Default is 1.
            % Output : - An AstroPSF object in which the PSF and variance
            %            are populated.
            % Author : Eran Ofek (May 2022)
            % Example: P=AstroPSF('ztf_20200207460174_000576_zg_c03_o_q3_diffimgpsf.fits')
            
            
            arguments
                FileName                  = [];
                Args.HDU                  = 1;
                Args.FileType             = 'fits';
                Args.UseRegExp logical    = false;
                Args.DataVar              = {};
                Args.VarHDU               = 1;
            end
            
            if ischar(FileName)
                FileName = {FileName};
            end
            Nf = numel(FileName);
            
            for Ifield=1:1:2
                if Ifield==2
                    File      = Args.DataVar;
                    HDU       = Args.VarHDU;
                    FieldName = 'DataVar';
                else
                    File      = FileName;
                    HDU       = Args.HDU;
                    FieldName = 'DataPSF';
                end
                    
                if isempty(File)
                    % define the object
                    Obj.(FieldName) = [];
                else
                    if isa(File,'ImageComponent') || isa(File,'AstroImage')
                        for If=1:1:Nf
                            Obj(If).(FieldName) = File{If}.Image;
                        end
                    elseif isa(File,'SIM') || isa(File,'imCL')
                        for If=1:1:Nf
                            Obj(If).(FieldName) = File{If}.Im;
                        end
                    elseif ischar(File)
                        ImIO = ImageIO(File, 'HDU',HDU,...
                                                 'FileType',Args.FileType,...
                                                 'IsTable',false,...
                                                 'UseRegExp',Args.UseRegExp);

                        Nobj = numel(ImIO);
                        for Iobj=1:1:Nobj
                            if ~isempty(ImIO(Iobj).Data)
                                Obj(Iobj).(FieldName) = ImIO(Iobj).Data;
                            end
                        end
                        Obj = reshape(Obj, size(ImIO));
                    else
                        % matrix format
                        Obj.(FieldName) = File;
                    end

                end % end if isempty...
            end % end for Ifield
            
        end

    end
    
    methods % Setters/Getters
    
    end
    
    methods (Static)  % static methods
       
            
        
    end
    
    methods % generic calibration
        % 
    end
    
    methods % generic pipelines
        % getSources (bias, flat,..., back, sources, astrometry)
        
        % match sources
        
        % coadd
    end

    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    

end

           
