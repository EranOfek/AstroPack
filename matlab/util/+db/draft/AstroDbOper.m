% @Todo: Think if we need it to work with a queue from background thread.
% Discuess with @Eran
%

%#docgen
%
% Methods:
%    AstroDbOper - Obj.setName('AstroImageDb')
%
% Methods: Static
%    unitTest -
%
%#/docgen


classdef AstroDbOper < handle
    
    properties (Hidden, SetAccess = public)
        Oper            %
        TableName       % Table name
        Fields          %
        Uuid            %
        Query           %
        Data            % AstroHeader / AstroImage / AstroTable / AstroCatalog
    
    end
    

    methods % Constructor
        
        function Obj = AstroDbOper
            % Obj.setName('AstroImageDb')
            
        end

    end
    
    
    methods
  
    end
             
    %
    methods % (Static???)

    end
    
    %
    
    % setters/getters
    methods
                
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'AstroDbOper test started')
       
            io.msgStyle(LogLevel.Test, '@passed', 'AstroDbOper test passed')
            Result = true;
        end
    end
             

end

           
