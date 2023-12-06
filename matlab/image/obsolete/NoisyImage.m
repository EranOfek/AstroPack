% NoisyImage handle class 
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: 
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

% "Component" is in folder ../base/

classdef NoisyImage < Component

    properties (Dependent)
        Image
        Back
        Var
        Mask
    end
    
    properties (SetAccess = public)
        ImageData ImageComponent
        BackData ImageComponent
        VarData ImageComponent
        MaskData MaskImage
        
        IsBackSub(1,1) logical              = false;
        IsPropagateVar(1,1) logical         = false;
        %VarOriginMap
        %UseVarOrigin
        
        
    end
    
    
    methods % Constructor       
        function Obj = NoisyImage(FileNames, Args)
            % class constructor
            % 
            
            arguments
                FileNames                   = [];
                Args.Back                   = [];
                Args.Var                    = [];
                Args.Mask                   = [];
                Args.Scale                  = [];
                Args.HDU                    = 1;
                Args.UseRegExp(1,1) logical = false;
            end
            
            if isempty(FileNames)
                % construct a single element object
                Obj.ImageData.Data         = [];
                Obj.BackData.Data          = [];
                Obj.VarData.Data           = [];
                Obj.MaskData.MaskData.Data = [];
            else
                
                
                
                
                
            Nh = numel(List);
            for Ih=1:1:Nh
                Obj(Ih).File = List{Ih};
            end
            Obj = reshape(Obj,size(List));
            
            Nhdu = numel(HDU);
            % read files
            for Ih=1:1:Nh
                if ~isempty(Obj(Ih).File)
                    Ihdu = min(Ih,Nhdu);
                    
                    Obj(Ih).Data = FITS.read1(Obj(Ih).File,HDU(Ihdu));
                end
            end
               
            
        end
    end 
    
        
    methods % Setters/getters
        function Result = get.Image(Obj)
            % getter for Image data
            
            Result = Obj.ImageData.Image;
        end
        
        function Obj = set.Image(Obj, Val)
            % setter for Image data
            
            Obj.ImageData.Image = Val;
            Obj.ImageData.Scale = [];
        end
        
        function Result = get.Back(Obj)
            % getter for Back data
            
            Result = Obj.BackData.Image;
        end
        
        function Obj = set.Back(Obj, Val)
            % setter for Back data (full image or scalar only)
            
            Obj.BackData.Image = Val;
            Obj.BackData.Scale = [];
        end
            
        function Result = get.Var(Obj)
            % getter for Var data
            
            Result = Obj.VarData.Image;
        end
        
        function Obj = set.Var(Obj, Val)
            % setter for Var data (full image or scalar only)
            
            Obj.VarData.Image = Val;
            Obj.VarData.Scale = [];
        end
        
        function Result = get.Mask(Obj)
            % getter for Mask data
            
            Result = Obj.MaskData.Image;
        end
        
        function Obj = set.Mask(Obj, Val)
            % setter for Mask data
            
            Obj.MaskData.MaskData.Image = Val;
            Obj.MaskData.MaskData.Scale = [];
        end
        
    end
    
    methods (Static) % static methods
       
    end
    
    methods % basic fununctions 
        function Result = funUnary(Obj, Operator, Args)
            %
            
            arguments
                Obj
                Operator function_handle
                Args.OpArgs cell                    = {}; % additional pars. to pass to the operator 
                
                Args.ReturnBack                     = true;
                Args.OperateOnData                  = true;
                Args.ReInterpOp(1,1) logical        = true;  % re-interpret the operator (e.g., in mask @plus -> @or
                
                Args.DataPropIn                     = {'ImageData','BackData','VarData'}; % not including CatData, PSFData and WCS
                Args.DataPropOut                    = {};
                Args.DataPrppScalar                 = {'Image'}; % returned output if IsOutObj=false
                Args.CreateNewObj(1,1) logical      = false;
                Args.Extra                          = {}; % extra par for special cases (e.g., header, cat).
                Args.CCDSEC                         = [];
            end
            
            if Args.OperateOnData
                PropName = 'Data';
            else
                % operate on full image
                PropName = 'Image';
            end
            
            
            if Args.CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if Obj(Iobj).IsPropagateVar
                    
                    if Obj(Iobj).IsBackSub  && Obj(Iobj).ReturnBack
                        % return background before operation
                        Mat = Obj(Iobj).ImageData.(PropName) + Obj(Iobj).BackData.(PropName);
                        % Apply operator with error propagation
                        [Result,ResultVar,FlagBad,~] = imUtil.image.fun_unary_withVariance(Operator, Mat, Obj(Iobj).VarData.(PropName), Args.OpArg);
                        % re-subtract background
                        Result(Iobj).ImageData.(PropName) = Result - Obj(Iobj).BackData.(PropName);
                        Result(Iobj).VarData.(PropName)   = ResultVar;
                        
                    else
                        % Apply operator with error propagation
                        [Result,ResultVar,FlagBad,~] = imUtil.image.fun_unary_withVariance(Operator, Mat, Obj(Iobj).VarData.(PropName), Args.OpArg);
                        Result(Iobj).ImageData.(PropName) = Result;
                        Result(Iobj).VarData.(PropName)   = ResultVar;
                        
                    end
                    
                    % FFU: FlagBad
                    
                else
                    % no error propagation
                    Obj(Iobj).ImageData.Data = Operator(Obj(Iobj).ImageData.Data, Args.OpArgs{:});
                    % do not change Back / Var / Mask
                end
            end
                    
            
            
        end
        
        function Obj = fun_binary(Obj1, Obj2, Operator, OpArgs, Args)
            %
            
        end
    end
     
    
    methods (Static) % Unit-Test
        function Result = unitTest()
            % unitTest for NoistImage class
            
            N = NoisyImage;
            
            
            Result = true;
        end
    end
    
end



