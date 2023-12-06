% AstroAngle class
%   A container for angles.
%
% Authors: Eran Ofek (Mar 2022)
%
% Functionality:
%


classdef AstroAngle < Base
    % Base class for all objects

    % Properties
    properties 
        Angle
        Units     = 'deg';   % 'deg' | 'rad' | 'arcmin' | 'arcsec' | 'mas' | 'microas' | 'frac' | 'hour'
    end
    
    %--------------------------------------------------------
    methods
        function Result = AstroAngle(Data, Units)
            % Constructor for AstroAmgle
            % Input  : - An array of angles, or a char array of
            %            sexagsimals, or a cell array of sexagesimal strings.
            %          - Units: 'rad' | 'deg' | 'arcmin' | 'arcsec' | 'mas' | 'microas' | 'hour' | 'frac'.
            %            If input is sexagesimal strings than this must be
            %            'h' | 'd', for 'hms', or 'dms', respectively.
            %            Default is 'rad'.
            % Output : - A single element AstroAngle object with the
            %            angular data stired in te Angle property.
            % Author : Eran Ofek (Mar 2022)
            % Example: A = AstroAngle(rand(10,10),'deg')
            %          A = AstroAngle('10:15:20.1','h')
            %          A = AstroAngle({'+10:15:20.1','-01:00:01.1'},'d')
            
            arguments
                Data
                Units    = 'rad';
            end
                        
            if iscell(Data) || ischar(Data)
                switch Units
                    case {'h','SH','hms'}
                        Result.Angle = celestial.coo.convertdms(Data, 'SH', 'r');
                        Result.Units = 'rad';
                    case {'d','SD','dms'}
                        Result.Angle = celestial.coo.convertdms(Data, 'SD', 'r');
                        Result.Units = 'rad';
                    otherwise
                        error('Unknown Units for char or cell input')
                end
            else
                switch Units
                    case {'deg','rad','arcmin','arcsec','mas','microas','hour','frac'}
                        % ok
                    otherwise
                        error('Unknown input units');
                end
                Result.Angle = Data;
                Result.Units = Units;
            end
            
        end
    end

    
    methods  % conversion
        function Result = convert(Obj, NewUnits)
            % Unit conversion for an AstroAngle object.
            % Input  : - An AstroAngle object.
            %          - New units: 'deg' | 'rad' | 'arcmin' | 'arcsec' | 'mas' | 'microas' | 'frac' | 'hour'
            %            Default is 'deg'.
            % Output : - The same (handle) AstroAngle object where the
            %            Angles are in the new units.
            % Author : Eran Ofek (Mar 2022)
            % Example: A = AstroAngle(rand(10,10),'arcsec');
            %          A.convert('deg')
            
            arguments
                Obj(1,1)
                NewUnits     = 'deg';
            end
           
            Result = Obj;
            Result.Angle = convert.angular(Obj.Units, NewUnits, Obj.Angle);
            Result.Units = NewUnits;
                
        end
        
        function Result = convert2array(Obj, NewUnits)
            % Unit conversion for an AstroAngle object into an array.
            %   will return an array rather than an AstroAngle object.
            % Input  : - An AstroAngle object.
            %          - New units: 'deg' | 'rad' | 'arcmin' | 'arcsec' | 'mas' | 'microas' | 'frac' | 'hour'
            %            Default is 'deg'.
            % Output : - An array with the converted angles.
            % Author : Eran Ofek (Mar 2022)
            % Example: A = AstroAngle(rand(10,10),'arcmin');
            %          D = A.convert2array('arcsec')
            
            arguments
                Obj(1,1)
                NewUnits     = 'deg';
            end
            
            Result = convert.angular(Obj.Units, NewUnits, Obj.Angle);
        end
        
        function Result = tpi(Obj)
            % Convert the angle to the range of 0 to 2*pi, in the current units
            % Input  : - An AstroAngle object.
            % Output : - The input AstroAngle object, but where all the
            %            angles are in the range of 0 to 2.*pi, in the
            %            original units.
            % Author : Eran Ofek (Mar 2022)
            % Example: A = AstroAngle(rand(10,10).*400,'deg');
            %          A.tpi;
           
            Result = Obj;
            TPI = convert.angular('rad', Obj.Units, 2.*pi);
            Result.Angle = mod(Obj.Angle, TPI);
            
        end
        
        function Result = mpi(Obj)
            % Convert the angle to the range of -pi to pi, in the current units
            % Input  : - An AstroAngle object.
            % Output : - The input AstroAngle object, but where all the
            %            angles are in the range of -pi to pi, in the
            %            original units.
            % Author : Eran Ofek (Mar 2022)
            % Example: A = AstroAngle(rand(10,10).*400,'deg');
            %          A.mpi;
           
            Result = Obj;
            TPI = convert.angular('rad', Obj.Units, 2.*pi);
            Result.Angle = mod(Obj.Angle, TPI);
            Flag = Result.Angle>(TPI.*0.5);
            Result.Angle(Flag) = Result.Angle(Flag) - TPI;
            
        end
        
        function Result = dsex(Obj)
            % Return a cell array of deg +dd:mm:ss.f sexagesimal coordinates.
            % Input  : - An AstroAngle object.
            % Output : - A cell array of degrees sexagesimal coordinates.
            % Author : Eran Ofek (Mar 2022)
            % Example: A = AstroAngle([10,-0.1],'deg');
            %          R = A.dsex;
            
            Data   = Obj.convert2array('rad');
            ND     = numel(Data);
            Result = cell(size(Data));
            for ID=1:1:ND
                Result{ID} = celestial.coo.convertdms(Data(ID),'r','SD');
            end 
        end
        
        function Result = hsex(Obj)
            % Return a cell array of hour hh:mm:ss.f sexagesimal coordinates.
            % Input  : - An AstroAngle object.
            % Output : - A cell array of hours sexagesimal coordinates.
            % Author : Eran Ofek (Mar 2022)
            % Example: A = AstroAngle([10,0.1],'deg');
            %          R = A.hsex;
            
            Data   = Obj.convert2array('rad');
            ND     = numel(Data);
            Result = cell(size(Data));
            for ID=1:1:ND
                Result{ID} = celestial.coo.convertdms(Data(ID),'r','SH');
            end 
        end
        
        function Result = dms(Obj)
            % Return a matrix of deg [Sign D M S] coordinates.
            % Input  : - An AstroAngle object.
            % Output : - A 4 column matrix of [Sign D M S] of all angles.
            % Author : Eran Ofek (Mar 2022)
            % Example: A = AstroAngle([10,-0.1],'deg');
            %          R = A.dms;
            
            Data   = Obj.convert2array('rad');
            ND     = numel(Data);
            Result = zeros(ND,3);
            for ID=1:1:ND
                Result(ID,1:4) = celestial.coo.convertdms(Data(ID),'r','D');
            end             
        end
        
        function Result = hms(Obj)
            % Return a matrix of hours [H M S] coordinates.
            % Input  : - An AstroAngle object.
            % Output : - A 3 column matrix of [H M S] of all angles.
            % Author : Eran Ofek (Mar 2022)
            % Example: A = AstroAngle([10,0.1],'deg');
            %          R = A.hms;
            
            Data   = Obj.convert2array('rad');
            ND     = numel(Data);
            Result = zeros(ND,3);
            for ID=1:1:ND
                Result(ID,1:4) = celestial.coo.convertdms(Data(ID),'r','D');
            end             
        end
        
        
    end
    
    methods % overload: sin, cos, ...
        function Result = fun(Obj, Fun, AddArgs)
            % Execute a function on angles (no unit conversion)
            % Input  : - An AstroAngle object.
            %          - A function handle.
            %          - A cell array of additional arguments to pass to
            %            the function. Default is {}.
            % Output : - An array of the function output on the Angle property
            %            (ignoring the units).
            % Author : Eran Ofek (Mar 2022)
            % Example: A = AstroAngle(rand(10,10),'deg');
            %          D = A.fun(@sind);
            
            arguments
                Obj(1,1)
                Fun
                AddArgs cell   = {};
            end
            
            Result = Fun(Obj.Angle, AddArgs{:});
        end
        
        function Result = sin(Obj)
            % sin function of angle data in an AstroAngle object (return an array).
            % Input  : - An AstroAngle object.
            % Output : - An array with the sin of the angles (taking into
            %            account the units).
            % Author : Eran Ofek (Mar 2022)
            % Example: A = AstroAngle(rand(10,10),'mas');
            %          D = A.sin;
            
            Result = Obj.convert2array('rad');
            Result = sin(Result);
        end
        
        function Result = cos(Obj)
            % cos function of angle data in an AstroAngle object (return an array).
            % Input  : - An AstroAngle object.
            % Output : - An array with the cos of the angles (taking into
            %            account the units).
            % Author : Eran Ofek (Mar 2022)
            % Example: A = AstroAngle(rand(10,10),'rad');
            %          D = A.cos;
            
            Result = Obj.convert2array('rad');
            Result = cos(Result);
        end
        
        function Result = tan(Obj)
            % tan function of angle data in an AstroAngle object (return an array).
            % Input  : - An AstroAngle object.
            % Output : - An array with the tan of the angles (taking into
            %            account the units).
            % Author : Eran Ofek (Mar 2022)
            % Example: A = AstroAngle(rand(10,10),'deg');
            %          D = A.tan;
            
            Result = Obj.convert2array('rad');
            Result = tan(Result);
        end
                
    end

    %----------------------------------------------------------------------
    methods(Static) % Unit test
        Result = unitTest()
            % unitTest for Base class
    end

end
