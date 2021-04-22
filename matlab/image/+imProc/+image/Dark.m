
classdef Dark < Component
    properties
        StackMethod 
        
    end
    
    methods  % Constructor
        function Obj = Dark(Args)
            % Constructor for a Dark object
            % Input  : * ...,key,val,...
            %            Can be any Match object property name followed by
            %            its new value.
            % Output : - A Dark object
            % Author : Eran Ofek (Apr 2021)
            % Example: D = imProc.image.Dark
           
            arguments
                Args.StackMethod
                
            end
            
            FN = fieldnames(Args);
            for Ifn=1:1:numel(FN)
                Obj.(FN{Ifn}) = Args.(FN{Ifn});
            end
        end
    end
    
    methods % identify images by type
        function isBias(Obj, AI, Args)
            %
            % Input  : - An AstroImage object.
            
            arguments
                Obj
                AI
                Args.ImTypeKeyName                                              = 'IMTYPE';
                
                Args.UseDict(1,1) logical                                       = true;
                Args.CaseSens(1,1) logical                                      = true;
                Args.SearchAlgo char  {mustBeMember(Args.SearchAlgo,{'strcmp','regexp'})} = 'strcmp'; 
                Args.IsInputAlt(1,1) logical                                    = true;
                Args.KeyDict                                                    = [];
            end
            
            
            % AI is now an AstroImage object
            Result = isImType(Obj, ImTypeVal, Args)
            
                
    
            
            
        end
        
        
        
    end
    
end
