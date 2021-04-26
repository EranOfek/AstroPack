
classdef Coadd < Component
    properties
        
        
    end
    
    methods % constructor
        function CObj = Coadd(Args)
            % Constructor for a Match object
            % Input  : * ...,key,val,...
            %            Can be any Match object property name followed by
            %            its new value.
            % Output : - A Match object
            % Author : Eran Ofek (Apr 2021)
            % Example: M = imProc.cat.Match('Radius',5,'RadiusUnits','arcsec');
           
            arguments
                Args.Cat              % catalaog
                Args.Ref              % ref catalog
                Args.Coo              % Search coordinates - used by coneSearch, match_catsHTM
                Args.CatName          % catsHTM name
                
                Args.CooUnits
                Args.Radius
                Args.RadiusUnits
                Args.Shape
                Args.AddDistCol
                Args.DistUnits
                Args.DistColName
                Args.DistColPos
                
                Args.OutIsSelfClass
                
                Args.CreateNewObj
            end
            
            FN = fieldnames(Args);
            for Ifn=1:1:numel(FN)
                Obj.(FN{Ifn}) = Args.(FN{Ifn});
            end
        end
    end
    
    methods % basic normalization function
        
        function Result = applayUnaryFun(Obj, ImObj, Offset, Operator, Args)
            % Applay scalar an unary function (e.g., constant) on AstroImage
            % Input  : - A Coadd object.
            %          - An AstroImage object.
            %          - An AstroImage object, or a cell array of matrices
            %            (images) or scalars, or a vector of scalars, or a
            %            function_handle.
            %            If an array, then will be converted to a cell of
            %            scalars using num2cell. Each cell element will be
            %            applied (using the operator) on the AstroImage. If one cell
            %            element then will be applied on all images.
            %            If function_handle (unary function) then it will
            %            be used to calculate the offset using this
            %            function.
            %          - Operator to use - e.g., subtracting the offset or
            %            dividing the factor. Default is @minus).
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of arguments to pass to
            %                   the Offset operator (if operator is provided).
            %            'PreDivide' - A logical indicating if to take
            %                   the inverse of the Offset prior to applying
            %                   the operator. This can be used to speed up
            %                   performences (in division). Default is false.
            %            'DataProp' - The data property in the AstroImage
            %                   (both input and offset) that contains the data.
            %                   Default is 'ImageData'.
            %            'DataPropIn' - The data property in the
            %                   ImageComponent that contains the image
            %                   data. Default is 'Data'.
            %            'CreateNewObj' - A logical indicating if the
            %                   output is a new object. If empty, then will
            %                   check the number of output argumnets. If
            %                   zero (e.g., C.subtractOffset(...)) then
            %                   will update the input image object. Else,
            %                   will create a new object.
            % Output : - An AstroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({ones(3,3), 3.*ones(4,4)});
            %          C  = imProc.image.Coadd;
            %          R  = C.applyUnaryFun(AI,1);
            %          R  = C.applayUnaryFun(AI,[1 2]);
            %          R  = C.applayUnaryFun(AI,{1 2}); % the same
            %          R  = C.applayUnaryFun(AI,AI); 
            %          R  = C.applayUnaryFun(AI,@mean,@minus,'OpArgs',{'all'});
            %          R  = C.applayUnaryFun(AI,@mean,@rdivide,'OpArgs',{'all'});
            
            
            arguments
                Obj(1,1) 
                ImObj
                Offset                        = [];   % AstroImage, cell of matrices, or array
                Operator function_handle      = @minus;
                Args.OpArgs cell              = {};
                Args.PreDivide(1,1) logical   = false;
                Args.DataProp char            = 'ImageData';
                Args.DataPropIn char          = 'Data';
                Args.CreateNewObj             = [];
            end
            
            if isempty(Args.CreateNewObj)
                if nargout==0
                    Args.CreateNewObj = false;
                else
                    Args.CreateNewObj = true;
                end
            end
                    
            if isa(Offset,'function_handle')
                % Offset is an operator
                Offset = funUnaryScalar(ImObj, Offset, 'OpArgs',Args.OpArgs, 'DataProp',{Args.DataProp}, 'DataPropIn',{Args.DataPropIn});
            end
            if isnumeric(Offset)
                Offset = num2cell(Offset);
            end
            
            if Args.CreateNewObj
                Result = ImObj.copyObject;
            else
                Result = ImObj;
            end
            Nobj = numel(ImObj);
            Noff = numel(Offset);
            for Iobj=1:1:Nobj
                Ioff = min(Iobj, Noff);
                if iscell(Offset)
                    Tmp = Offset{Ioff};
                else
                    Tmp = Offset(Ioff).(Args.DataProp).(Args.DataPropIn);
                end
                if ~isempty(Args.PreDivide)
                    Tmp = 1./Tmp;
                end
                
                Result(Iobj).(Args.DataProp).(Args.DataPropIn) = Operator(ImObj(Iobj).(Args.DataProp).(Args.DataPropIn), Tmp);
            end
        end
        
        function Result = subtractOffset(Obj, ImObj, Offset, Args)
            % Remove offset (constant) from AstroImage
            % Input  : - A Coadd object.
            %          - An AstroImage object.
            %          - An AstroImage object, or a cell array of matrices
            %            (images) or scalars, or a vector of scalars, or a
            %            function_handle.
            %            If an array, then will be converted to a cell of
            %            scalars using num2cell. Each cell element will be
            %            subtracted from the AstroImage. Of one cell
            %            element then will be subtracted from all images.
            %            If function_handle (unary function) then it will
            %            be used to calculate the offset using this
            %            function.
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of arguments to pass to
            %                   the Offset operator (if operator is provided).
            %            'DataProp' - The data property in the AstroImage
            %                   (both input and offset) that contains the data.
            %                   Default is 'ImageData'.
            %            'DataPropIn' - The data property in the
            %                   ImageComponent that contains the image
            %                   data. Default is 'Data'.
            %            'CreateNewObj' - A logical indicating if the
            %                   output is a new object. If empty, then will
            %                   check the number of output argumnets. If
            %                   zero (e.g., C.subtractOffset(...)) then
            %                   will update the input image object. Else,
            %                   will create a new object.
            % Output : - An AstroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({ones(3,3), 3.*ones(4,4)});
            %          C  = imProc.image.Coadd;
            %          R  = C.subtractOffset(AI,1);
            %          R  = C.subtractOffset(AI,[1 2]);
            %          R  = C.subtractOffset(AI,{1 2}); % the same
            %          R  = C.subtractOffset(AI,AI); 
            %          R  = C.subtractOffset(AI,@mean,'OpArgs',{'all'});
            
            arguments
                Obj(1,1) 
                ImObj
                Offset                    = [];   % AstroImage, cell of matrices, or array
                Args.OpArgs cell          = {};
                Args.DataProp char        = 'ImageData';
                Args.DataPropIn char      = 'Data';
                Args.CreateNewObj         = [];
            end
            
            if isempty(Args.CreateNewObj)
                if nargout==0
                    Args.CreateNewObj = false;
                else
                    Args.CreateNewObj = true;
                end
            end
            
            Result  = Obj.applayUnaryFun(ImObj, Offset, @minus, 'OpArgs',{'all'}, 'DataProp',Args.DataProp, 'DataPropIn',Args.DataPropIn, 'CreateNewObj',Args.CreateNewObj, 'PreDivide',false);
            
        end
        
        function Result = divideFactor(Obj, ImObj, Factor, Args)
            % Divide factor (constant) from AstroImage
            % Input  : - A Coadd object.
            %          - An AstroImage object.
            %          - An AstroImage object, or a cell array of matrices
            %            (images) or scalars, or a vector of scalars, or a
            %            function_handle.
            %            If an array, then will be converted to a cell of
            %            scalars using num2cell. Each cell element will be
            %            divided from the AstroImage. Of one cell
            %            element then will be subtracted from all images.
            %            If function_handle (unary function) then it will
            %            be used to calculate the factor using this
            %            function.
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of arguments to pass to
            %                   the Offset operator (if operator is provided).
            %            'DataProp' - The data property in the AstroImage
            %                   (both input and offset) that contains the data.
            %                   Default is 'ImageData'.
            %            'DataPropIn' - The data property in the
            %                   ImageComponent that contains the image
            %                   data. Default is 'Data'.
            %            'CreateNewObj' - A logical indicating if the
            %                   output is a new object. If empty, then will
            %                   check the number of output argumnets. If
            %                   zero (e.g., C.subtractOffset(...)) then
            %                   will update the input image object. Else,
            %                   will create a new object.
            % Output : - An AstroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({ones(3,3), 3.*ones(4,4)});
            %          C  = imProc.image.Coadd;
            %          R  = C.divideFactor(AI,1);
            %          R  = C.divideFactor(AI,[1 2]);
            %          R  = C.divideFactor(AI,{1 2}); % the same
            %          R  = C.divideFactor(AI,AI); 
            %          R  = C.divideFactor(AI,@mean,'OpArgs',{'all'});
            
            arguments
                Obj(1,1) 
                ImObj
                Factor                    = [];   % AstroImage, cell of matrices, or array
                Args.OpArgs cell          = {};
                Args.DataProp char        = 'ImageData';
                Args.DataPropIn char      = 'Data';
                Args.CreateNewObj         = [];
            end
            
            if isempty(Args.CreateNewObj)
                if nargout==0
                    Args.CreateNewObj = false;
                else
                    Args.CreateNewObj = true;
                end
            end
            
            Result  = Obj.applayUnaryFun(ImObj, Factor, @times, 'OpArgs',{'all'}, 'DataProp',Args.DataProp, 'DataPropIn',Args.DataPropIn, 'CreateNewObj',Args.CreateNewObj, 'PreDivide',true);
            
        end
        
    end
    
    methods % coaddition functions
        function varargout = funCube(Obj, ImObj, Args)
            % Applay function/s on a single cube
            % Input  : - A Coadd object.
            %          - An AstroImage object.
            %          * ...,key,val,...
            %            'CCDSEC' - [Xmin Xmax Ymin Ymax] to stack.
            %                       If empty, use entire image. Default is
            %                       [].
            %            'FunCube' - A function handle, or a cell array of
            %                   function handles to applay on cube.
            %                   Default is {@mean, @var}.
            %                   All the functions are applayed on the same
            %                   cube. Note that the number of functions
            %                   actually applied is min(nargout,numel(FunCube)).
            %                   Default is {@mean, @var}.
            %            'FunArgs' - If FunCube is a function handle then
            %                   this is a cell array of additional
            %                   arguments to pass to the FunCube function.
            %                   If FunCube is a cell array of function
            %                   handles, then this is a cell array of cell
            %                   arrays of additional arguments for each
            %                   function.
            %                   Default is {{3,'omitnan'}, {[],3,'omitnan'}}.
            %            'DataProp' - Data property in the AStroImage from
            %                   which to extract a cube.
            %                   Default is 'ImageData'.
            %            'DataPropIn' - Data property in the ImageComponent
            %                   from which to extract the image data.
            %                   Default is 'Data'.
            %            'SaveInProp' - A cell array of AstroImage
            %                   properties (one to one correspondence to
            %                   FunCube) in which to save the output
            %                   results.
            %                   If empty, then the output is matrices.
            %                   If provided, then the first output argument
            %                   is an AstroImage with the specific field
            %                   populated with the results of the
            %                   corresponding FunCube results.
            %                   Default is {'ImageData','VarData'}.
            %            'DimIndex' - Dimension along which the cube will
            %                   be constructed. Default is 3.
            % Output : * Arbitrary number of arguments.
            %            Each output contains a matrix of a coadd image that
            %            corresponds to one 'FunCube' function.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({rand(10,10), rand(10,10), rand(10,10)});
            %          C = imProc.image.Coadd;
            %          [Cube1, Cube2] = funCube(AI);
            %          [CAI] = C.funCube(AI,'SaveInProp',{'ImageData','VarData'});
            
            arguments
                Obj(1,1)
                ImObj AstroImage
                Args.CCDSEC                        = [];
                Args.FunCube                       = {@mean, @var};
                Args.FunArgs cell                  = {{3,'omitnan'}, {[],3,'omitnan'}};
                Args.DataProp char                 = 'ImageData'; %,'BackData', 'VarData', 'MaskData'};
                Args.DataPropIn char               = 'Data';
                Args.SaveInProp                    = {'ImageData','VarData'};
                Args.DimIndex                      = 3;
            end
           
            % convert AstroImage to cubes
            [Cube] = images2cube(ImObj, 'CCDSEC',Args.CCDSEC, 'DataPropIn',Args.DataPropIn, 'DataProp',{Args.DataProp}, 'DimIndex',Args.DimIndex);
            
            if ~iscell(Args.FunCube)
                Args.FunCube = {Args.FunCube};
                Args.FunArgs = {Args.FunArgs};
            end
            Nfun = numel(Args.FunCube);
            if isempty(Args.SaveInProp)
                Nfun = min(Nfun, nargout);
            else
                Nfun = min(Nfun, numel(Args.SaveInProp));
            end
            varargout = cell(1,Nfun);
            for Ifun=1:1:Nfun
                varargout{Ifun} = Args.FunCube{Ifun}(Cube, Args.FunArgs{Ifun}{:});
            end
            
            if ~isempty(Args.SaveInProp)
                Result = AstroImage;
                Nprop = numel(Args.SaveInProp);
                for Iprop=1:1:Nprop
                    Result.(Args.SaveInProp{Iprop}).(Args.DataPropIn) = varargout{Iprop};
                end
                varargout{1} = Result;
            end
        end
        
        function Result = stackCube(Obj, ImObj, Args)
            % Combine images in an AstroImage object
            %       Combine the ImageData, BackImage, and MaksImage
            %       The VarImage is either combined or calculated.
            
            arguments
                Obj
                ImObj
                Args.StackMethod                            = 'mean';
                Args.StackArgs cell                         = {};
                Args.EmpiricalVarFun function_handle        = @var;
                Args.EmpiricalVarFunArgs                    = {[],3,'omitnan'};
                Args.VarCube                                = [];
                Args.MedianVarCorrForEmpirical(1,1) logical = false;
                Args.DivideEmpiricalByN(1,1) logical        = false;
                Args.DivideVarByN(1,1) logical              = false;
                Args.CalcCoaddVarEmpirical(1,1) logical     = true;
                Args.CalcCoaddVar(1,1) logical              = true;
                Args.CalcCoaddN(1,1) logical                = true;
            end
            
            
            
            
        end
        
    end
    
end