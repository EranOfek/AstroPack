% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef AstroImage < ImageComponent
    % Component should contain:
    % UserData
    % Config
    
    properties (Dependent) % Access image data directly        
        Image 
        Mask 
        Back 
        Var
        Header  % e.g., Header, Header('EXPTIME'), Header({'EXPTIME','IMTYPE'}), Header('IMTYPE',{additional args to keyVal})
        Cat     % e.g., Cat, Cat([1 3]), Cat('RA'), Cat({'RA','Dec'})
        PSF
        %WCS
    end
    
    properties (SetAccess = public)
        % Data
        ImageData(1,1) BaseImage
        MaskData(1,1) MaskImage
        BackData(1,1) BackImage
        VarData(1,1) VarImage
        
        HeaderData(1,1) AstroHeader
        CatData(1,1) AstroCatalog
        PSFData(1,1) AstroPSF
        WCS(1,1) AstroWCS
        
    end
    
    methods % Constructor
       
        function Obj = AstroImage(AnotherObj,Args)
            %
            
%             arguments
%                 AnotherObj            = [1 1];
%                 Args.
%             end
            
            
            
        end

    end
 

 
    methods % Setters/Getters
        function Obj = set.Image(Obj, Data)
            % setter for Image - store image in ImageData property
            Obj.ImageData.Image = Data;
        end
        
        function Data = get.Image(Obj)
            % getter for Image - get image from ImageData property
            Data = Obj.ImageData.Image;
        end        
        
        
    end
    
    methods (Static)  % static methods
       
    end
    
    methods % general functionality
        function Result = fun_unary(Obj, Operator, Args)
            %
            
            arguments
                Obj
                Operator function_handle
                Args.ReInterpOp(1,1) logical        = true;  % re-interpret the operator (e.g., in mask @plus -> @or
                Args.OpArgs cell                    = {}; % additional pars. to pass to the operator 
                Args.DataPropIn 
                Args.DataPropOut
                Args.OutType
                Args.CreateNewObj(1,1) logical      = false;
                Args.Extra                          = {}; % extra par for special cases (e.g., header, cat).
            end
            
            Nobj = numel(Obj);
            
            
                     
            
        end
        
        function Result = fun_binary(Obj1, Obj2, Operator, Args)
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
                Args.OutType
                Args.CreateNewObj(1,1) logical      = false;
                Args.Extra                          = {}; % extra par for special cases (e.g., header, cat).
            end
            
            
            
            
            
        end
        
        
        function Result = funHeader(Obj, Fun, ArgsToFun)
            %
            
        end
        
        function Result = funCat(Obj, Fun, ArgsToFun)
            %
            
        end
        
        function Result = funWCS(Obj, Fun, ArgsToFun)
            %
        end
        
        function Result = funPSF(Obj, Fun, ArgsToFun)
            %
        end
        
    end
    

    methods % specific functionality and overloads
        function conv(Obj, Args)
            % Convolve images with their PSF, or another PSF
            arguments
                Obj
                Args.PSF
            end
        end
        
        function xcorr(Obj, Args)
            % cross correlate images with their PSF, or another PSF
            arguments
                Obj
                Args.PSF
            end
        end
        
        function subtract_back(Obj, Args)
            % subtract (and de-subtract) background from images
        end
        

        % ARE THESE FUNS PER IMAGE OR FVER MULTIPLE IMAGES????
        % possible solution: imFun.single.mean, imFun.stack.mean
        
        function NewObj = sum(Obj)
            %
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

            
