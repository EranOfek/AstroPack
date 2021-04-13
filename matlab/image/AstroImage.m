% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef AstroImage < Component
    % Component should contain:
    % UserData
    % Config
    
    properties (Dependent) % Access image data directly        
        Image 
        Back 
        Var
        Mask 
        Header  % e.g., Header, Header('EXPTIME'), Header({'EXPTIME','IMTYPE'}), Header('IMTYPE',{additional args to keyVal})
        Key
        Cat     % e.g., Cat, Cat([1 3]), Cat('RA'), Cat({'RA','Dec'})
        PSF
        %WCS
    end
    
    properties (SetAccess = public)
        % Data
        %ImageData(1,1) NoisyImage
        
        ImageData(1,1) SciImage              %= SciImage;
        MaskData(1,1) MaskImage              %= MaskImage;
        BackData(1,1) BackImage              %= BackImage;
        VarData(1,1) VarImage                %= VarImage;
        
        HeaderData(1,1) AstroHeader          %= AstroHeader;
        CatData(1,1) AstroCatalog            %= AstroCatalog;
        PSFData(1,1) AstroPSF                %= AstroPSF;
        WCS   % not ready: AstroWCS
        
        PropagateErr(1,1) logical          = false;
    end
    
    properties (Hidden, Constant)
        % set the relation between the Dependent prop and the data prop
        Relations   = struct('Image','ImageData',...
                             'Back','BackData',...
                             'Var','VarData',...
                             'Mask','MaskData');
        
        
    end
    
    methods % Constructor
       
        function Obj = AstroImage(FileNames, Args)
            %
            
            % Example:
            %          AI = AstroImage([2 2]);
            %          AI = AstroImage(FileNames,'HDU',1);
            %          AI = AstroImage(FileNames,'HDU',1,'Back',FileNames,'BackHDU',2);
            %          AI = AstroImage(FileNames,'HDU',1,'Back',FileNamesBack,'BackHDU',1);
            
            arguments
                FileNames                     = [];
                Args.HDU                      = 1;
                Args.Scale                    = [];
                Args.ReadHeader(1,1) logical  = true;
                
                Args.Back                     = [];
                Args.BackHDU                  = [];
                Args.BackScale                = [];
                
                Args.Var                      = [];
                Args.VarHDU                   = [];
                Args.VarScale                 = [];
                
                Args.Mask                     = [];
                Args.MaskHDU                  = [];
                Args.MaskScale                = [];
                
                Args.FileType                 = [];
                Args.UseRegExp(1,1) logical   = false;
                
            end
            
            if isempty(FileNames)
                % create a single elemeny empty object
                % must initilaize all the internal objects
                Obj.ImageData   = SciImage;
                Obj.BackData    = BackImage;
                Obj.VarData     = VarImage;
                Obj.MaskData    = MaskImage;
                Obj.HeaderData  = AstroHeader;
                Obj.CatData     = AstroCatalog;
                Obj.CatData     = AstroCatalog;
                Obj.PSFData     = AstroPSF;
                Obj.WCS         = [];            % FFU: update when WCS class is ready
                
            else
                if isnumeric(FileNames)
                    Nobj = prod(FileNames);
                    for Iobj=1:1:Nobj
                        Obj(Iobj) = AstroImage([]);
                    end
                    Obj = reshape(Obj, FileNames);
                    
                else
                    if isa(FileNames,'AstroImage')
                        Obj = FileNames;
                    elseif isa(FileNames,'SIM')
                        % convert SIM to AstroImage
                        
                    elseif isa(FileNames,'imCl')
                        % convert imCl to AstroImage
                        
                    else
                        % ImageData
                        Obj = AstroImage.readImages2AstroImage(FileName,'HDU',Args.HDU,...
                                                                        'Obj',[],...
                                                                        'FileType',Args.FileType,...
                                                                        'UseRegExp',Args.UseRegExp,...
                                                                        'Scale',Args.Scale,...
                                                                        'ReadHeader',Args.ReadHeader,...
                                                                        'DataProp','ImageData');
                                                                        
                        % Other data properties
                        ListProp  = {'Back','Var','Mask'};
                        ListData  = {'BackData','VarData','MaskData'};
                        ListHDU   = {'BackHDU','VarHDU','MaskHDU'};
                        ListScale = {'BackScale','VarScale','MaskScale'};
                        
                        Nlist = numel(ListProp);
                        for Ilist=1:1:Nlist
                            if ~isempty(Args.(ListHDU{Ilist})) && isempty(Args.(ListProp{Ilist}))
                                % read the Back/Var/... images from the science images
                                % (FileNames), but from a different HDU.
                                Args.(ListProp{Ilist}) = FileNames;
                            end
                            if ~isempty(Args.(ListProp{Ilist}))
                                % do not read header
                                Obj = AstroImage.readImages2AstroImage(FileName,'HDU',Args.(ListHDU{Ilist}),...
                                                                            'Obj',Obj,...
                                                                            'FileType',Args.FileType,...
                                                                            'UseRegExp',Args.UseRegExp,...
                                                                            'Scale',Args.(ListScale{Ilist}),...
                                                                            'ReadHeader',false,...
                                                                            'DataProp',ListData{Ilist});
                            end
                        end
                        
                        
                                   
                            
                    end
                    
                end
            end
                    
                
            
            
            
            
%             % this is here for testing only
%             Obj.ImageData  = SciImage({300 + 10.*randn(100,100)});
%             Obj.VarData    = VarImage({10.*ones(100,100)});
%             Obj.BackData   = BackImage({300});
%             Obj.HeaderData.Data = {'EXPTIME',1,'';'FILTER','R',''}; 
%             
%             arguments
%                 AnotherObj            = [1 1];
%                 Args.
%             end
            
            
            
        end

    end

    methods (Static) % utilities
        function Obj = imageIO2AstroImage(ImIO, DataProp, Scale, CopyHeader, Obj)
            % Convert an ImageIO object into an AstroImage object
            % Input  : - An ImageIO object.
            %          - data property in which to store the image.
            %          - Scale of the image.
            %          - A logical indicating if to copy the header.
            %          - An AstroImage object in which to put the data.
            %            If empty, create a new object. Default is empty.
            % Output : - An AstroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI=AstroImage.imageIO2AstroImage(ImIO, 'ImageData', [], true)
       
            if nargin<5
                % create new Obj
                Obj = [];
            end
            
            if isempty(Obj)
                Obj  = AstroImage(size(ImIO));
            else
                % use supplied object
                if ~isa(Obj,'AstroImage')
                    error('Obj (argument at position 5) must be an AstroImage object');
                end
            end
            
            Nobj = numel(Obj);
            if Nobj~=numel(ImIO)
                error('Supplied AstroImage object and ImageIO object must have the same size');
            end
            
            for Iobj=1:1:Nobj
                Obj(Iobj).(DataProp).Data  = ImIO(Iobj).Data;
                Obj(Iobj).(DataProp).Scale = Scale;
                if CopyHeader
                    Obj(Iobj).HeaderData.Data = ImIO(Iobj).Header;
                end
            end
        end
        
        function Obj = readImages2AstroImage(FileName, Args)
            % Create AstroImage object and read images into a specific property.
            % Input  : - Either:
            %            [] - will return a single element empty object.
            %            [N, M,...] - will return an empty objects
            %                   which size is [N, M,...].
            %            A table to put in the Data property.
            %            A cell array of matrices to put in the Data
            %                   property.
            %            FileName with or without wild cards or regexp, 
            %                   or a cell of file names to read.
            %          * ...,key,val,...
            %            'Obj' - An AstroImage object in which to put the
            %                   data in. If empty create a new object.
            %                   Default is empty.
            %            'HDU' - HDU number or Dataset name from which to
            %                   read the images.
            %            'FileType' - See ImageIO. Default is [].
            %            'UseRegExp' - See ImageIO. Default is false.
            %            'Scale' - The scale of the image (see definition
            %                   in ImageComponent). Default is [].
            %            'DataProp' - AstroImage data property in wjich to
            %                   store the data. Default is 'ImageData'.
            %            'ReadHeader' - Default is true.
            % Outout : - An AstroImage object with the images stored in the
            %            requested field.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI=AstroImage.readImages2AstroImage('*.fits', 'DataProp', 'ImageData');
            %          AI=AstroImage.readImages2AstroImage([]);
            %          AI=AstroImage.readImages2AstroImage([1 2]);
            %          AI=AstroImage.readImages2AstroImage({rand(10,10), rand(5,5)});
            %          AI=AstroImage.readImages2AstroImage({rand(10,10), rand(5,5)},'DataProp','VarData');
            %          AI=AstroImage.readImages2AstroImage({rand(10,10), rand(20,20)},'DataProp','ImageData');
            %          AI=AstroImage.readImages2AstroImage({rand(10,10), rand(20,20)},'DataProp','BackData','Obj',AI);
            %          AI=AstroImage.readImages2AstroImage({rand(5,5), rand(10,10)},'DataProp','VarData','Obj',AI,'Scale',2);
            
            
            arguments
                FileName
                Args.Obj                    = [];
                Args.HDU                    = 1;
                Args.FileType               = [];
                Args.UseRegExp(1,1) logical = false;
                Args.Scale                  = [];
                Args.DataProp               = 'ImageData';
                Args.ReadHeader             = true;
            end
            
                
            switch lower(Args.DataProp)
                case {'imagedata','backdata','vardata','maskdata'}
                    ImIO = ImageIO(FileName, 'HDU',Args.HDU,...
                                             'FileType',Args.FileType,...
                                             'IsTable',false,...
                                             'ReadHeader',Args.ReadHeader,...
                                             'UseRegExp',Args.UseRegExp);
                case {'cat','catdata'}
                    ImIO = ImageIO(FileName, 'HDU',Args.HDU,...
                                             'FileType',Args.FileType,...
                                             'IsTable',true,...
                                             'ReadHeader',Args.ReadHeader,...
                                             'UseRegExp',Args.UseRegExp);
                otherwise
                    error('DataProp %s is not supported',Args.DataProp);
            end
            Obj = AstroImage.imageIO2AstroImage(ImIO, Args.DataProp, Args.Scale, Args.ReadHeader, Args.Obj);
            
            
        end
                                         
    end

 
    methods % Setters/Getters
        function Obj = set.Image(Obj, Data)
            % setter for Image - store image in ImageData property
            %Obj.(Relations.Image).Image = Data;  % can use this instead
            Obj.ImageData.Image = Data;
        end
        
        function Data = get.Image(Obj)
            % getter for Image - get image from ImageData property
            Data = Obj.ImageData.Image;
        end    
        
        function Obj = set.Back(Obj, Data)
            % setter for BackImage
            Obj.BackData.Image = Data;
        end
        
        function Data = get.Back(Obj)
            % getter for BackImage
            Data = Obj.BackData.Image;
        end
        
        function Obj = set.Var(Obj, Data)
            % setter for VarImage
            Obj.VarData.Image = Data;
        end
        
        function Data = get.Var(Obj)
            % getter for VarImage
            Data = Obj.VarData.Image;
        end
        
        function Obj = set.Mask(Obj, Data)
            % setter for MaskImage
            Obj.MaskData.Image = Data;
        end
        
        function Data = get.Mask(Obj)
            % getter for MaskImage
            Data = Obj.MaskData.Image;
        end
        
        function Data = get.Header(Obj)
            % getter for Header
            Data = Obj.HeaderData.Data;
        end
        
        function Data = get.Key(Obj)
            % getter for Header keys
            Data = Obj.HeaderData.Key;
        end
        
        
        
    end
    
    methods (Static)  % static methods
       
    end
    
    methods % translate Data property names
        function DataName = translateDataPropName(Obj, DataProp)
            % translate the Data propert names (e.g., 'Image' -> 'ImageData')
            % Output : - A cell array of Data property names.;
            % Example: AI.translateDataPropName('Var')
            %          AI.translateDataPropName({'Back','Var'})
            
            if ischar(DataProp)
                DataProp = {DataProp};
            end
            
            Nprop    = numel(DataProp);
            DataName = cell(1,Nprop);
            for Iprop=1:1:Nprop
                if isfield(Obj(1).Relations,DataProp{Iprop})
                    DataName{Iprop} = Obj(1).Relations.(DataProp{Iprop});
                else
                    % do not translate - return as is
                    DataName{Iprop} = DataProp{Iprop};
                    %error('Requested DataProp: %s - can not be translated into valid data property name',DataProp{Iprop});
                end
            end
            
        end
    end
    
    methods % empty and size
        function varargout = isemptyImage(Obj, Prop)
            % Check if data images in AstroImage object are empty
            % Input  : - An AstroImage object (multi elements supported).
            %          - A cell array of data properties for which to check
            %            if empty.
            %            Default is {'Image','Back','Var','Mask'}.
            % Output : * One output per requested data property. For each
            %            data property, this is an array of logical
            %            indicating if the data isempty.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI=AstroImage;
            %          [a,b]=AI.isemptyImage({'Image','Back'})
            
            arguments
                Obj
                Prop        = {'Image','Back','Var','Mask'};
            end
            
            if ischar(Prop)
                Prop = {Prop};
            end
            
            Nprop = numel(Prop);            
            Nobj = numel(Obj);
            varargout = cell(1,nargout);
            if nargout>Nprop
                error('Number of requested output (%d) must be equal or smaller then number of reqested data properties (%d)',nargout,Nprop);
            else
                Prop  = Prop(1:nargout);
                Nprop = nargout;
            end
            for Iprop=1:1:Nprop
                varargout{Iprop} = false(size(Obj));
                for Iobj=1:1:Nobj
                    [varargout{Iprop}(Iobj)] = isempty(Obj(Iobj).(Prop{Iprop}));
                end
            end
            
        end
        
        function [Nx, Ny] = sizeImage(Obj, Prop)
            % Return the size of images in AstroImage object
            % Input  : - An AstroImage object (multi elements supported).
            %          - A single propery name (char array)
            % Output : - Number of rows in each AstroImage element.
            %          - Number of columns in each AstroImage element.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI=AstroImage;
            %          [Ny, Nx] = AI.sizeImage
            %          [Ny, Nx] = AI.sizeImage('Back')
            
            arguments
                Obj
                Prop char       = 'Image';
            end
            
            Nobj = numel(Obj);
            Nx   = zeros(size(Obj));
            Ny   = zeros(size(Obj));
            for Iobj=1:1:Nobj
                [Ny, Nx] = size(Obj(Iobj).(Prop));
            end
            
        end
        
    end
    
    methods % basic functionality: funUnary, funUnaryScalar, funBinary, funStack, funTransform
        
        function Result = funUnary(Obj, Operator, Args)
            % Apply an unary function on AstroImage object.
            %       This include applying the function  on specific data
            %       fields, and or image sections (CCDSEC), and error
            %       propagation.
            %       Note that error propgation is activated using the
            %       AstroImage.PropagateErr property (default is false).
            % Input  : - An AstroImage object (multi elements supported)
            %          - Operator (function_handle) (e.g., @sin)
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of additional arguments to
            %                   pass to the operator. Default is {}.
            %            'CreateNewObj' - Logical indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false)
            %                   Default is false (i.e., input object will
            %                   be modified).
            %            'CCDSEC' - CCDSEC on which to operate:
            %                   [Xmin, Xmax, Ymin, Ymax].
            %                   Use [] for the entire image.
            %                   If not [], then DataPropIn/Out will be
            %                   modified to 'Image'.
            %            'OutOnlyCCDSEC' - A logical indicating if the
            %                   output include only the CCDSEC region, or
            %                   it is the full image (where the opeartor,
            %                   operated only on the CCDSEC region).
            %                   Default is true.
            %            'CalcImage' - A logical indicating if to apply the
            %                   operator to the Image field.
            %                   Default is true.
            %            'CalcVar' - A logical indicating if to apply the
            %                   operator to the Var field.
            %                   Default is true.
            %            'CalcBack' - A logical indicating if to apply the
            %                   operator to the Back field.
            %                   Default is true.
            %            'ReturnBack' - A logical indicating if to return
            %                   the background to the image before applying
            %                   the operator. Default is true.
            %            'ReRemoveBack' - A logical indicating if to remove
            %                   the background from the image after
            %                   applying the operator (relevant only if the
            %                   SciImage.IsBackSubtracted=true).
            %                   Default is true.
            %            'UpdateHeader' - A logical indicating if to update
            %                   the header of the output image.
            %                   Default is true.
            %            'AddHistory' - A logical indicating if to add an
            %                   HISTORY line in the header specifying the
            %                   operation. Default is true.
            %            'NewUnits' - If empty do nothing. If provided will
            %                   put this value in the header 'UNITS' key.
            %                   Default is [].
            %            'InsertKeys' - A cell array of 3 columns cell
            %                   array {key,val,comment}, to insert to the
            %                   header. Default is {}.
            %            'ReplaceKeys' - A cell array of keywords to
            %                   replace. Default is {}.
            %            'ReplaceVals' - A cell array of keywords values to
            %                   replace (corresponding top ReplaceKeys).
            %                   Default is {}.
            %            'replaceValArgs' - A cell array of additional
            %                   arguments to pass to the replaceVal function.
            %                   Default is {}.
            %            'insertKeyArgs' - A cell array of additional
            %                   arguments to pass to the insertKey function.
            %                   Default is {}.
            %            'DeleteCat' - A logical indicating if to delete
            %                   the catalog data. Default is false.
            %            'ImCompDataPropIn' - Data property in the
            %                   ImageComponent on which the operator
            %                   will be operated. Default is 'Data'.
            %            'ImCompDataPropOut' - Data property in the
            %                   ImageComponent in which the output
            %                   will be stored. Default is 'Data'.
            % Output : - An AstroImage object with the operator applied on
            %            the data.
            % Author : Eran Ofek (Apr 2021)
            % Example: 

           
            arguments
                Obj
                Operator function_handle
                Args.OpArgs cell                = {};
                Args.CreateNewObj(1,1) logical  = false;
                Args.CCDSEC                     = [];
                Args.OutOnlyCCDSEC(1,1) logical = true;
                
                Args.CalcImage(1,1) logical     = true;
                Args.CalcVar(1,1) logical       = true;
                Args.CalcBack(1,1) logical      = true;
                Args.ReturnBack(1,1) logical    = true;
                Args.ReRemoveBack(1,1) logical  = true;
                
                Args.UpdateHeader(1,1) logical  = true;
                Args.AddHistory(1,1) logical    = true;
                Args.NewUnits                   = []; % if empty don't change
                Args.InsertKeys                 = {};
                Args.ReplaceKeys                = {};
                Args.ReplaceVals                = {};
                Args.replaceValArgs             = {};
                Args.insertKeyArgs              = {};
               
                Args.DeleteCat(1,1) logical     = false;
                
%                 Args.DataPropIn                 = {'Image','Back','Var'};  % should not operate on Mask
%                 Args.DataPropOut                = {};
                Args.ImCompDataPropIn           = 'Data';
                Args.ImCompDataPropOut          = 'Data';
            end
        
%             if isempty(Args.DataPropOut)
%                 Args.DataPropOut = Args.DataPropIn;
%             end
%             % translate all the requested property names to "Data"
%             % properties
%             Args.DataPropIn  = translateDataPropName(Obj(1), Args.DataPropIn);
%             Args.DataPropOut = translateDataPropName(Obj(1), Args.DataPropOut);
            
            
            if Args.CreateNewObj
                Result = Obj.copyObject;
            else
                Result = Obj;
            end
            
            Nobj  = numel(Obj);
            for Iobj=1:1:Nobj
                if Obj(Iobj).PropagateErr && Args.CalcVar
                    % perform error propagation
                    VarMat = Obj(Iobj).VarData.(Args.ImCompDataPropIn);
                else
                    VarMat = [];
                end
                
                % return background to image if needed
                if Args.ReturnBack && Obj(Iobj).ImageData.IsBackSubtracted && ~isempty(Obj(Iobj).BackData.(Args.ImCompDataPropIn))
                    ImageMat = Obj(Iobj).ImageData.(Args.ImCompDataPropIn) + Obj(Iobj).BackData.(Args.ImCompDataPropIn);
                    RetBack  = true;
                else
                    ImageMat = Obj(Iobj).ImageData.(Args.ImCompDataPropIn);
                    RetBack  = false;
                end
                    
               [ResultFun,ResultVar,Flag,FunH] = imUtil.image.fun_unary_withVariance(Operator, ImageMat,...
                                                                                       VarMat,...
                                                                                       'OpArgs',Args.OpArgs,...
                                                                                       'CCDSEC',Args.CCDSEC,...
                                                                                       'OutOnlyCCDSEC',Args.OutOnlyCCDSEC);
                
                if Args.CalcImage                                                                 
                    if RetBack && Args.ReRemoveBack
                        % remove background from image
                        ResultFun = ResultFun - Obj(Iobj).BackData.(Args.ImCompDataPropIn);
                        Obj(Iobj).ImageData.IsBackSubtracted = true;
                    else
                        Obj(Iobj).ImageData.IsBackSubtracted = false;
                    end
                    Result(Iobj).ImageData.(Args.ImCompDataPropOut) = ResultFun;
                end
                
                if Args.CalcVar
                    Result(Iobj).VarData.(Args.ImCompDataPropOut) = ResultVar;
                end
                if Args.CalcBack && ~isempty(Result(Iobj).Back)
                    % use funUnary of ImageComponent
                    Result(Iobj).BackData = funUnary(Result(Iobj).BackData, Operator, 'OpArgs',Args.OpArgs,...
                                                                                      'CreateNewObj',Args.CreateNewObj,...
                                                                                      'CCDSEC',Args.CCDSEC,...
                                                                                      'OutOnlyCCDSEC',Args.OutOnlyCCDSEC,...
                                                                                      'DataPropIn',Args.ImCompDataPropIn,...
                                                                                      'DataPropOut',Args.ImCompDataPropOut);
                end
                
                % update Header
                Result(Iobj).HeaderData = funUnary(Result(Iobj).HeaderData, Operator, 'OpArgs',Args.OpArgs,...
                                                                                      'UpdateHeader',Args.UpdateHeader,...                                                                        
                                                                                      'AddHistory',Args.AddHistory,...
                                                                                      'NewUnits',Args.NewUnits,...
                                                                                      'InsertKeys',Args.InsertKeys,...
                                                                                      'ReplaceKeys',Args.ReplaceKeys,...
                                                                                      'ReplaceVals',Args.ReplaceVals,...
                                                                                      'CreateNewObj',false,...
                                                                                      'replaceValArgs',Args.replaceValArgs,...
                                                                                      'insertKeyArgs',Args.insertKeyArgs);
                                                                                  
                
                
                if Args.DeleteCat
                    Obj(Iobj).CatData.deleteCatalog;
                end
                
                % Do not modify: PSF, WCS
                
                
            end
            
        
        end
            
        
        function Result = fun__Unary(Obj, Operator, Args)
            % Apply an unary function on AstroImage data
            % Input  : - An AstroImage object
            %          - Operator (e.g., @tan, @std)
            %
            
            arguments
                Obj
                Operator function_handle
                Args.OpArgs cell                    = {}; % additional pars. to pass to the operator 
                Args.PropagateVar(1,1) logical      = false;
                Args.CCDSEC                         = [];   % empty for full image
                Args.CreateNewObj(1,1) logical      = false;
                Args.ReInterpOp(1,1) logical        = true;  % re-interpret the operator (e.g., in mask @plus -> @or
                % you should not use the following parameters unless you
                % know what you are doing!
                Args.DataPropIn                     = {'ImageData','HeaderData'}; % not including CatData, PSFData and WCS
                Args.DataPropOut                    = {};
                Args.Extra                          = {}; % extra par for special cases (e.g., header, cat).
            end
            
            % make sure DataPropIn/Out are cell/string arrays
            if ischar(Args.DataPropIn)
                Args.DataPropIn  = {Args.DataPropIn};
            end
            if ischar(Args.DataPropOut)
                Args.DataPropOut = {Args.DataPropOut};
            end
            % If DataPropOut is empty, make equal to DataPropIn
            if isempty(Args.DataPropOut)
                Args.DataPropOut = Args.DataPropIn;
            end
            
            
            Nprop = numel(Args.DataPropIn);
            if Nprop~=numel(Args.DataPropOut)
                error('Numeber of elements in DataPropIn and DataPropOut must be identical');
            end
            
            Nobj = numel(Obj);
            
            if Args.CreateNewObj
                Result = Obj.copyObject;
            else
                Result = Obj;
            end
            
            for Iobj=1:1:Nobj
                %Result(Iobj).(Args.DataPropOut{Iprop}) = Operator(Obj(Iobj).(Args.DataPropOut{Iprop}), Args.OpArgs{:});
                % DataPropIn and DataPropOut - use default
                Result(Iobj).(Args.DataPropOut{Iprop}) = fun_unary(Obj(Iobj).(Args.DataPropIn{Iprop}), Operator, ...
                                                                   'OpArgs',Args.OpArgs{:},...
                                                                   'PropagateVar',Args.PropagateVar,...
                                                                   'CCDSEC',Args.CCDSEC,...
                                                                   'CreateNewObj',Args.CreateNewObj,...
                                                                   'ReInterpOp',Args.ReInterpOp);
                                                               
            end
           
        end
        
        
        function varargout = object2array(Obj,DataProp)
            % Convert an AstroImage object that contains scalars into an array
            % Input  : - An AstroImage object.
            %          - A cell array of data properties to collect. Each
            %            property must contains a scalar.
            %            Default is {'Image'}.
            % Output : * Number of output arguments equal to the number of
            %            data properties.
            %            Each output argument is an array which size is
            %            equal to size(Obj), and contains all the scalars
            %            in the corresponding data property.
            % Author : - Eran Ofek (Apr 2021)
            % Example: 
            
            arguments
                Obj
                DataProp                    = {'Image','Back','Var'};
            end
            
            if ischar(DataProp)
                DataProp = {DataProp};
            end
            
            % select only the requested data properties
            DataProp = DataProp(1:1:nargout);
            
            Nprop = numel(DataProp);
            Nobj  = numel(Obj);
            
            varargout = cell(1,Nprop);
            for Iprop=1:1:Nprop
                varargout{Iprop} = nan(size(Obj));
                for Iobj=1:1:Nobj
                    varargout{Iprop}(Iobj) = Obj.(DataProp{Iprop});
                end
            end
            
        end
        
        function varargout = funUnary2scalar(Obj, Operator, Args)
            % Apply unary function using funUnary and convert to array of
            % scalars using object2array
            % The main difference between this function and funUnaryScalar
            % is that funUnaryScalar can be used to apply operators on
            % dependent properties like 'Image', while funUnary2scalar
            % apply it on the data properties (e.g., 'ImageData'). This
            % means that funUnary2scalar output may include error
            % propagation due to the operator, while funUnarayScalar
            % doesn't.
            % Input  : -
            % Output : -
            % Author : Eran Ofek (Apr 2021)
            % Example: 
            
            arguments
                Obj
                Operator function_handle
                Args.ReInterpOp(1,1) logical        = true;  % re-interpret the operator (e.g., in mask @plus -> @or
                Args.OpArgs cell                    = {}; % additional pars. to pass to the operator 
                Args.DataPropIn                     = {'ImageData'}; % DataProp on which to run the operator
                Args.DataPropOut                    = {'Image','Back','Var'};  % DataProp to retrieve
                Args.Extra                          = {}; % extra par for special cases (e.g., header, cat).
                Args.CCDSEC                         = [];
            end
            
            NewObj = funUnary(Obj, Operator, 'ReInterpOp',Args.ReInterpOp,...
                                             'OpArgs',Args.OpArgs,...
                                             'DataPropIn',Args.DataPropIn,...
                                             'DataPropOut',{},...
                                             'CreateNewObj',true,...
                                             'Extra',Args.Extra,...
                                             'CCDSEC',Args.CCDSEC);
            % convert into a matrix
            [varargout{1:1:nargout}] = object2array(NewObj, Args.DataPropOut);
            
        end
        
        function varargout = funUnaryScalar(Obj, Operator, Args)
            % Apply a unary function that return a scalar and return an
            % array of the results.
            % The main difference between this function and funUnary2scalar
            % is that funUnaryScalar can be used to apply operators on
            % dependent properties like 'Image', while funUnary2scalar
            % apply it on the data properties (e.g., 'ImageData'). This
            % means that funUnary2scalar output may include error
            % propagation due to the operator, while funUnarayScalar
            % doesn't.
            % Input  : -
            % Output : -
            % Author : Eran Ofek (Apr 2021)
            % Example: 
            
            
            arguments
                Obj
                Operator function_handle
                Args.OpArgs cell                    = {}; % additional pars. to pass to the operator 
                Args.DataProp                       = {'Image','Back','Var'};  % DataProp to retrieve
                Args.CCDSEC                         = [];
            end
            
            if ischar(Args.DataProp)
                Args.DataProp = {Args.DataProp};
            end
            
            % select only the requested data properties
            Args.DataProp = Args.DataProp(1:1:nargout);
            
            Nprop = numel(Args.DataProp);
            Nobj  = numel(Obj);
            
            varargout = cell(1,Nprop);
            for Iprop=1:1:Nprop
                varargout{Iprop} = nan(size(Obj));
                if isempty(Args.CCDSEC)
                    % operator on full image
                    for Iobj=1:1:Nobj
                        varargout{Iprop}(Iobj) = Operator(Obj.(Args.DataProp{Iprop}), Args.OpArgs{:});
                    end
                else
                    % operator on sub image
                    for Iobj=1:1:Nobj
                        varargout{Iprop}(Iobj) = Operator(Obj.(Args.DataProp{Iprop})(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2)), Args.OpArgs{:});
                    end
                end
            end
            
            
            
        end
        
            
        
        function Result = funBinary(Obj1, Obj2, Operator, Args)
            %
            
            arguments
                Obj1
                Obj2
                Operator function_handle
                Args.ReInterpOp(1,1) logical        = true;  % re-interpret the operator (e.g., in mask @plus -> @or
                Args.OpArgs cell                    = {}; % additional pars. to pass to the operator 
                Args.DataPropIn1 
                Args.DataPropIn2
                Args.DataPropOut
                Args.OutType
                Args.CreateNewObj(1,1) logical      = false;
                Args.Extra                          = {}; % extra par for special cases (e.g., header, cat).
                Args.CCDSEC                         = [];
            end
            
            Nobj = numel(Obj);
            
            
        end
        
        function Result = fun_stack(Obj, Operator, Args)
            %
            
            arguments
                Obj
                Operator
                Args.ReInterpOp(1,1) logical        = true;  % re-interpret the operator (e.g., in mask @plus -> @or
                Args.OpArgs cell                    = {}; % additional pars. to pass to the operator 
                Args.DataPropIn 
                Args.DataPropOut
                Args.IsOutObj(1,1) logical          = true;
                Args.CreateNewObj(1,1) logical      = false;
                Args.Extra                          = {}; % extra par for special cases (e.g., header, cat).
                Args.CCDSEC                         = [];
                Args.MemoryBlocks                   = [Inf Inf];  % do the stack onb sub images and merge
            end
            
            
            
            
            
            
            
        end
        
        function varargout = images2cube(Obj, Args)
            % Generate cubes of images from array of AstroImage objects
            % Input  : - An AstroImage array
            %          * ...,key,val,...
            %            'DataPropIn' - A cell array of data properties
            %                   from which to generate cubes.
            %                   Default is {'Image'}.
            %            'DimIndex' - Dimension of the image index.
            %                   Either 1 or 3. Default is 3.
            % Output : * Cube of images, per each element in DataProIn.
            % Author : Eran Ofek (Apr 2021)
            % Example: UNTESTED 
            
            arguments
                Obj
                Args.DataPropIn                    = {'Image'};
                Args.DimIndex                      = 3;
                Args.CCDSEC                        = [];  % empty for the entire image
                %Args.VerifySize(1,1) logical       = false;
            end
            
            if ischar(Args.DataPropIn)
                Args.DataPropIn = {Args.DataPropIn};
            end
            
            Nobj      = numel(Obj);
            Nprop     = numel(Args.DataPropIn);
            varargout = cell(1,Nprop);
            
            for Iprop=1:1:Nprop
                % size of images
                if isempty(Args.CCDSEC)
                    Size = size(Obj(1).(Args.DataPropIn{Iprop}));
                else
                    Size = [Args.CCDSEC(4)-Args.CCDSEC(3)+1, Args.CCDSEC(2)-Args.CCDSEC(1)+1];
                end
                % allocate cube
                varargout{Iprop} = zeros(Size(1), Size(2), Nobj);
                
                if isempty(Args.CCDSEC)
                    % generate cube from full images
                    for Iobj=1:1:Nobj
                        varargout{Iprop}(:,:,Iobj) = Obj(1).(Args.DataPropIn{Iprop});
                    end
                else
                    % generate cube from sub images (CCDSEC)
                    for Iobj=1:1:Nobj
                        varargout{Iprop}(:,:,Iobj) = Obj(1).(Args.DataPropIn{Iprop})(Args.CCDSEC(3):Args.CCDSEC(4), Args.CCDSEC(1):Args.CCDSEC(2));
                    end
                end
                
                % change dimension
                if Args.DimIndex==3
                    % do nothing
                elseif Args.DimIndex==1
                    varargout{Iprop} = permute(varargout{Iprop}, [3 1 2]);
                else
                    error('DimIndex must be 1 or 3');
                end
            end
            
            
            
        end
        
        
        function Result = funHeader(Obj, Operator, Args)
            % Apply function of Header properties in AstroImage array
            
            
        end
        
        function Result = funCat(Obj, Fun, Args)
            % Apply function of Cat properties in AstroImage array
            
        end
        
        function Result = funWCS(Obj, Fun, ArgsToFun)
            % Apply function of WCS properties in AstroImage array
        end
        
        function Result = funPSF(Obj, Fun, ArgsToFun)
            % Apply function of PSF properties in AstroImage array
        end
        
    end
    

    methods % specific functionality and overloads
        function conv(Obj, Args)
            % Convolve images with their PSF, or another PSF
            arguments
                Obj
                Args.PSF                         = [];
                Args.ArgsPSF                     = {};
                Args.DataPropIn                  = {'Image'}
                Args.DataPropOut                 = {'Image'};
                Args.CreateNewObj(1,1) logical   = true;
                Args.IsOutObj(1,1) logical       = true;
            end
            
            
            
        end
        
        function xcorr(Obj, Args)
            % cross correlate images with their PSF, or another PSF
            arguments
                Obj
                Args.PSF
            end
        end
        
        function subtractBack(Obj, Args)
            % subtract (and de-subtract) background from images
        end
        
        
        
        
        function coadd(Obj, Args)
            %
            
        end
        
        function background(Obj, Args)
            %
            
            arguments
                Obj
                Args.IsGlobal(1,1) logical            = false;
                
            end            
        end
        

        
        
        % ARE THESE FUNS PER IMAGE OR FVER MULTIPLE IMAGES????
        % possible solution: imFun.single.mean, imFun.stack.mean
        
        function NewObj = sum(Obj, Args)
            %
            
            arguments
                Obj
                Args.IsOutObj(1,1) logical        = false;
                Args.Dim                          = 'all';
                Args.DataPropIn                   = {'Image'};
                Args.DataPropOut                  = {'Image'};
                Args.CreateNewObj(1,1) logical    = true;
            end
            
        end
        
        function NewObj = mean(Obj)
            %
        end
        
        function NewObj = median(Obj)
            %
        end
        
        function NewObj = min(Obj)
            %
        end
        
        function NewObj = max(Obj)
            %
        end
        
        function NewObj = std(Obj)
            %
        end
        
        function NewObj = rstd(Obj)
            %
        end
        
        function NewObj = var(Obj)
            %
        end
        
        function NewObj = rvar(Obj)
            %
        end
        
        function NewObj = quantile(Obj)
            %
        end
        
        function NewObj = plus(Obj1, Obj2)
            %
        end
        
        function NewObj = minus(Obj1, Obj2)
            %
        end
        
        function NewObj = times(Obj1, Obj2)
            %
        end
        
        function NewObj = rdivide(Obj1, Obj2)
            %
        end
        
    end
    
    methods % Unit-Test
        function Result = unitTest()
            Astro = AstroImage;
            Result = true;
        end
    end
    

end

            
