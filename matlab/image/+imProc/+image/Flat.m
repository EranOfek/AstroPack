
classdef Flat < Component
    properties
        Flat AstroImage
    end
    
    methods  % Constructor
        function Obj = Flat(Args)
            % Constructor for a Flat object
            % Input  : * ...,key,val,...
            %            Can be any Flat object property name followed by
            %            its new value.
            % Output : - A Flat object
            % Author : Eran Ofek (Apr 2021)
            % Example: D = imProc.image.Flat
           
            arguments
                Args.StackMethod
                
            end
            
            FN = fieldnames(Args);
            for Ifn=1:1:numel(FN)
                Obj.(FN{Ifn}) = Args.(FN{Ifn});
            end
        end
    end
   
    methods % flat, deflat
        
        
    end
    
    methods % nonlinearity
    
        
    end
    
end