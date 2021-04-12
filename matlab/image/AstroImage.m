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
        
        ImageData(1,1) SciImage
        MaskData(1,1) MaskImage
        BackData(1,1) BackImage
        VarData(1,1) VarImage
        
        HeaderData(1,1) AstroHeader
        CatData(1,1) AstroCatalog
        PSFData(1,1) AstroPSF
        WCS(1,1)   % not ready: AstroWCS
        
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
       
        function Obj = AstroImage(AnotherObj,Args)
            %
            
            % this is here for testing only
            Obj.ImageData  = SciImage({rand(100,100)});
            Obj.HeaderData.Data = {'EXPTIME',1,'';'FILTER','R',''}; 
            
%             arguments
%                 AnotherObj            = [1 1];
%                 Args.
%             end
            
            
            
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
        
        function funUnary(Obj, Operator, Args)
            %
            
            arguments
                Obj
                Operator function_handle
                Args.OpArgs cell                = {};
                Args.CreateNewObj(1,1) logical  = false;
                Args.CCDSEC                     = [];
                Args.OutOnlyCCDSEC(1,1) logical = false;
                Args.DataPropIn                 = {'Image','Back','Var'};  % should not operate on Mask
                Args.DataPropOut                = {};
                Args.ImCompDataPropIn           = 'Data';
                Args.ImCompDataPropOut          = 'Data';
            end
        
            if isempty(Args.DataPropOut)
                Args.DataPropOut = Args.DataPropIn;
            end
            % translate all the requested property names to "Data"
            % properties
            Args.DataPropIn  = translateDataPropName(Obj(1), Args.DataPropIn);
            Args.DataPropOut = translateDataPropName(Obj(1), Args.DataPropOut);
            
            
            Nobj  = numel(Obj);
            Nprop = numel(Args.DataPropIn);
            for Iobj=1:1:Nobj
                for Iprop=1:1:Nprop
                    % Note that operating unary fun on Mask should do nothing.
                    % but this is the user responsibilty not to include
                    % 'Mask' in the DataProp...
                    if ~isa(Obj(Iobj).(Args.DataPropIn{Iprop}), 'ImageComponent')
                        if Obj(Iobj).PropagateErr
                            % perform error propagation
                            
                            
                            
                            
                            
                            
                        else
                            funUnary(Obj(Iobj).(Args.DataPropIn{Iprop}), Operator, 'OpArgs',Args.OpArgs,...
                                                                               'CreateNewObj',Args.CreateNewObj,...
                                                                               'CCDSEC',Args.CCDSEC,...
                                                                               'OutOnlyCCDSEC',Args.OutOnlyCCDSEC,...
                                                                               'DataPropIn',Args.ImCompDataPropIn,...
                                                                               'DataPropOut',Args.ImCompDataPropOut);
                        end
                    else
                        error('funUnary must operate on an ImageComponent element');
                    end
                end
                
                % update Header
                
                
                % Do not modify: Cat, PSF, WCS
                
                
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

            
