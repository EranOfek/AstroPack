% A BitDictionary class
% 
% Author: Eran Ofek (March 2021)
% Example: BD=BitDictionary; D.unitTest

classdef BitDictionary < handle
    properties
        Name char            = '';   % BitDictionary name - e.g., 'HeaderKeySynonyms'
        Dic table            = table();    % Name, Description, BitInd
    end
   
    methods % constructor
        function Obj=Dictionary(DictionaryName)
            % Dictionary constructor
            % Input  : Dictionary file name to load. Default is ''.
            
            arguments
                DictionaryName char    = '';
            end
           
            Obj.Name = DictionaryName;
        end
    end
    
    methods
        function [BitName,BitDescription,BitInd]=bit_to_name(Obj,BitVal,ValType)
            % Convert an array of bit indices to bit names
            
            arguments
                Obj(1,1)
                BitVal
                ValType char    {mustBeMember(ValType,{'bit','dec','str'})} = 'bit';
            end
            
            Nbit           = numel(BitVal);
            BitName        = cell(size(BitVal));
            BitDescription = cell(size(BitVal));
            
            switch lower(ValType)
                case 'bit'
                    BitInd = zeros(size(BitVal));
                    for Ibit=1:1:Nbit
                        Flag = Obj.Dic.BitInd==BitVal(Ibit);
                        BitName{Ibit}        = Obj.Dic.BitName(Flag);
                        BitDescription{Ibit} = Obj.Dic.BitDescription(Flag);
                        BitInd(Ibit)         = Obj.Dic.BitInd(Flag);
                    end
                    
                    
                case 'dec'
                    
                case 'str'
                    
                otherwise
                    error('Unknown ValType option');
            end
            
            
        end
        
        function [BitInd,BitDec,BitDescription]=name_to_bit(Obj,Name)
        end
        
        
    end
    
    
    methods % unit Test
        function unitTest(Obj)
            %
        
            Tbl = cell2table({'Saturation', 'Saturated pixel', 1; ...
                              'DeadPix',    'Pixel level at Flat is low', 2});
                          
            Tbl.Properties.VariableNames = {'BitName','BitDescription','BitInd'};
            Obj.Dic = Tbl;
            
            [BitName,BitDescription,BitInd]=bit_to_name(Obj,1,'bit')
            
        end
    end
    
end
