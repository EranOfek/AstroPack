% FastPipe - A fast real time pipeline for LAST ~1s exposure video mode
%
% Thoughts and comments:
% The methods should include the following components:
% 1. Loading the calibration object (dark+flat)
% 2. Full analsis of the first image in the seqence + writing data products
% 3. analyzing the cyclic buffer of images


classdef FastPipe < Component
    % 
            
    properties       
        %
        CI CalibImages   = CalibImages;    % CalibImages

        % These fields are the input parameters for getPath() and getFileName()
        ProjectName  = 'LAST';
        Node         = 1;
        DataDir      = 1;
        CamNumber    = [];
        HostName     = [];

        BasePath     = [];
   
        NewPath      = []; %'new';    % if start with '/' then abs path
        CalibPath    = []; %'calib';  % if start with '/' then abs path
        FailedPath   = []; %'failed'; % if start with '/' then abs path
        LogPath      = []; %'log';    % if start with '/' then abs path

        SciPath      = []; %'science';

        AutoPath     = [];  % 'LAST',...        

        ObsCoo       = [35.04073 30.05298 415];  % [deg deg m]
    end
    
    properties (Hidden)
        % Fields formatting
        %FormatFieldID   = '%06d';       % Used with FieldID
        FormatCounter   = '%03d';       % Used with Counter        
        FormatCCDID     = '%03d';       % Used with CCDID
        FormatCropID    = '%03d';       % Used with CropID
        FormatVersion   = '%03d';       % Used with Version
        

        
        DefNewPath      = 'new';    % if start with '/' then abs path
        DefCalibPath    = 'calib';  % if start with '/' then abs path
        DefFailedPath   = 'failed'; % if start with '/' then abs path
        DefLogPath      = 'log';    % if start with '/' then abs path
        DefRefPath      = 'data/references';   %/last01e/data/refreences'

        FieldList       = pipeline.DemonLAST.fieldsListLAST;
    end

    properties (Hidden, SetAccess=protected, GetAccess=public)
     
    end
    
    properties (Hidden, Constant)
        ListType        = { 'bias', 'dark', 'flat', 'domeflat', 'twflat', 'skyflat', 'fringe', 'focus', 'sci', 'wave', 'type' };
        ListLevel       = {'log', 'raw', 'proc', 'stack', 'ref', 'coadd', 'merged', 'calib', 'junk'};
        ListProduct     = { 'Image', 'Back', 'Var', 'Exp', 'Nim', 'PSF', 'Cat', 'Spec', 'Mask', 'Evt', 'MergedMat', 'Asteroids'};
    end
    
    
    methods % Constructor
       
        function Obj = DemonLAST(Args)
            % Constructor for DemonLAST

            
        end
        
    end
    

    


    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest()
    end
    
end
