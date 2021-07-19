% CalibImages class - A class for storing calibration image, and performs
%       the basic calibration steps on images.
% Description: Each element of this class may contain all the calibration
%   images for a single detector or a section of a detector.
%   This allows performing calibration for multiple detectors
%   simultaneously.

% use case:
%   1. create dark/bias/flat/fringe
%       bias
%       dark
%       flat
%       fringe
%   2. save to disk/db
%      writeImages
%      write2DB
%   3. upload from disk/db
%       readImages - read calibration images from disk
%       searchDB - search recent calibration files
%   4. apply calib on image
%           Each calib element corresond to one sub/image
%       debias
%       dedark
%       deflat
%       defringe
%       maskSaturated
%       calibrate (do all)


classdef CalibImages < Component
    properties
        Bias AstroImage(1,1)     
        Dark AstroImage   
        Flat AstroImage
        Fringe AstroImage(1,1)   
        
        SubtractOverScan(1,1) logical   = true;
        
        FlatFilter         = NaN;
        DarkExpTime        = NaN;
        DarkTemp           = NaN;
        FringeFilter       = NaN;
        FringeExpTime      = NaN;
    end
   
    methods  % constructor
        function Obj = CalibImages(Args)
            %
            
            arguments
                Args.Dark
                Args.Flat
                Args.FlatFilter     = [];
                Args.DarkExpTime    = [];
                Args.DarkTemp       = [];
            end
            
            Obj.Dark = Args.Dark;
            Obj.Flat = Args.Flat;
            
            
            
            if isempty(Args.FlatFilter)
                % attempt to read FlatFilter from Flat header
            end
            
            
        end
    end
    
    methods (Access=private)
        function [Nobj, Nim] = checkObjImageSize(Obj, Image)
            % Check the validity of the size of CalibImages object and input image
            %   This function will return an error if not one of the
            %   following: size(Image)==size(Obj) or (numel(Obj)==1 and numel(Image)>1)');
            % Input  : - A CalibImages object.
            %          - An AstroImage object
            % Output : - Number of elements in the CalibImages object.
            %          - Number of elements in the AstroImage object.
            % Author : Eran Ofek (Jul 2021)
            
            Nobj = numel(Obj);
            Nim  = numel(Image);
            % check size of Obj and Image
            if all(size(Obj)==size(Image))
                % Image and Obj has the same size - apply one to one calib
            else
                if Nobj==1 && Nim>1
                    % apply calibration to each input image
                else
                    error('Not valid Obj/Image size: Options are: size(Image)==size(Obj) or (numel(Obj)==1 and numel(Image)>1)');
                end
            end
        end
    end
    
    methods
        function Result = debias(Obj, Image, Args)
            % Subtract bias image from an image and update mask
            % Input  : - A CalibImages object.
            %            If this is a single-element object, then the bias
            %            image will subtrcated from all input images.
            %            If this is a multi-element object, then the input
            %            must have the same number of elements, ane the
            %            bias subtraction will be done element-by-element.
            %          - An AstroImage object containing the input image
            %            from which to subtract the bias image.
            %          * ...,key,val,...
            %            'CreateNewObj' - [], false, true. 
            %                   See Base.createNewObj for details.
            %                   This referes to creation of a new copy of
            %                   the input AstroImage (not the CalibImages
            %                   object). Default is [].
            % Output : - The AstroImage object, from which the bias was
            %            subtrcated.
            % See also: imProc.dark.debias
            % Author : Eran Ofek (Jul 2021)
            % Example: debias(Obj, Image)
            
            arguments
                Obj
                Image AstroImage
                Args.CreateNewObj     = [];   % refers to the Image and not the Obj!!!
            end
            
            % create new copy of Image object
            [Result, CreateNewObj] = createNewObj(Image, Args.CreateNewObj, nargout);
           
            [Nobj, Nim] = Obj.checkObjImageSize(Image);
            
            % select
            
            for Iim=1:1:Nobj
                Iobj = min(Iim, Nobj);
                % Note taht CreateNewObj was already done (if needed)
                Result(Iim) = imProc.dark.debias(Result(Iim), CalibImages(Iobj).Bias, 'CreateNewObj',false);
            end
                                                                                           
        end
    end
    
    
    
end