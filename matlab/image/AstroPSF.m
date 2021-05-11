% AstroPSF - A container class for PSFs
% Properties :
%       Data - A dependent property that generates the PSF stamp
%       Var -  A dependent property that generates the PSF variance stamp
%       DataPSF - A PSF data. Stamp, or function parameters
%       DataVar - A PSF variance data. Stamp, or function parameters
%       FunPSF - A PSF function handle
%       ArgVals - Arguments values.
%       ArgNames - The argument names.
%       StampSize - PSF stamp size.
% Functionality :
%       

classdef AstroPSF < Component
    properties (Dependent) % Access image data directly    
        Data
        Var
    end
    
    properties (SetAccess = public)
        DataPSF           = [];   % The fun parameters, or an image
        DataVar           = [];
        FunPSF            = [];   % e.g., Map = Fun(Data, X,Y, Color, Flux)
        ArgVals cell      = {};
        ArgNames cell     = {'X','Y','Color','Flux'};
        StampSize         = [];
    end
    
    methods % Constructor
       
        function Obj = AstroPSF
            
            
        end

    end
 

 
    methods % Setters/Getters
        function Result = get.Data(Obj)
            % getter for Dependent property Data
            Result = getPSF(Obj);
        end
        
        function Obj = set.Data(Obj, DataPSF)
            % setter for Dependent property Data
            Obj.DataPSF = DataPSF;
            % make fun empty
            Obj.FunPSF = [];
        end
        
        function Result = get.Var(Obj)
            % getter for Dependent property Var
            Result = DataVar;
        end
        
        function Obj = set.Var(Obj, VarPSF)
            % setter for Dependent property Var
            Obj.DataVar = VarPSF;
        end
    end
    
    methods (Static)  % static methods
        function multiGaussianPSF(DataPSF, X, Y, Color, Flux)
            %
            % I = G1(x,y, sigmaX(X,Y,Color,Flux), sigmaY(X,Y,Color,Flux), rho(X,Y,Color,Flux) )
            
            
            
            
        end
            
    end
    
    methods % generating PSF
        function Result = getPSF(Obj, DataPSF, FunPSF, StampSize, ArgVals, ArgNames)
            % get PSF from AstroPSF object
            % Input : - A single AstroPSF object.
            %         - DataPSF is empty, will take Obj.DataPSF.
            %           If not empty, will also populate Obj.DataPSF.
            %           Default is empty.
            %         - FunPSF function handle (like DataPSF). Default is [].
            %         - StampSize [I,J]. If empty, use default.
            %           Default is [].
            %         - ArgVals (like DataPSF). Default is [].
            %         - ArgNames (like DataPSF). Default is [].
            % Output : - A PSF stamp.
            % Author : Eran Ofek
            % Example: 
            
            arguments
                Obj(1,1)
                DataPSF   = [];
                FunPSF    = [];
                StampSize = [];
                ArgVals   = [];
                ArgNames  = [];
            end
            
            if isempty(DataPSF)
                DataPSF = Obj.DataPSF;
            else
                Obj.DataPSF = DataPSF;
            end
            if isempty(FunPSF)
                FunPSF  = Obj.FunPSF;
             else
                Obj.FunPSF = FunPSF;
            end
            if isempty(StampSize)
                StampSize = Obj.StampSize;
            else
                Obj.StampSize = StampSize;
            end
            if isempty(ArgVals)
                ArgVals  = Obj.ArgVals;
            else
                Obj.ArgVals = ArgVals;
            end
            if isempty(ArgNames)
                ArgNames  = Obj.ArgNames;
            else
                Obj.ArgNames = ArgNames;
            end
        
            if isempty(FunPSF)
                % PSF is an image stamp
                Result = Obj.DataPSF;
            else
                Result = Obj.FunPSF(Obj.DataPSF, Obj.ArgVals{:});
            end
            if ~isempty(StampSize)
                if ~all(size(Result)==StampSize)
                    % pad PSF
                    error('Pad PSF option is not yet available');
                end
            end
            
        end
            
    end
    
    methods % functionality
        function Result = fun_unary(Obj, OperatorOperatorArgs, OutType, DataProp, DataPropOut)
            %
           
            Nobj = numel(Obj)
            
            
        end
        
    end
    

    methods (Static) % UnitTest
        function Result = unitTest()
            % unitTest for AstroPSF
            % Example: Result = AstroPSF.unitTest
            
            AP = AstroPSF;
            P = imUtil.kernel2.gauss;
            AP.DataPSF = P;
            if ~all(AP.getPSF==P)
                error('Problem with set/get PSF');
            end
            
            Result = true;
        end
    end
    

end

            
