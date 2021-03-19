% A BitDictionary class
% 
% Author: Eran Ofek (March 2021)
% Example: BD=BitDictionary; D.unitTest

classdef BitDictionary < handle
    properties
        Name char            = '';   % BitDictionary name - e.g., 'HeaderKeySynonyms'
        Dic table            = table();    % Name, Description, BitInd
    end
    properties (SetAccess=private)
        Nbit                 = 8; % setter by Dic
        Class                = 'uint8';
    end
    properties (Hidden, Constant)
        ColBitName        = 'BitName';
        ColBitDescription = 'BitDescription';
        ColBitInd         = 'BitInd';
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
    
    methods % setters/getters
        function set.Dic(Obj,BinDic)
            % setter for Dic property
            % BinDic can be: 'table' | 'cell'
            
            if istable(BinDic)
                Obj.Dic = BinDic;
                Obj.Dic.Properties.VariableNames = {Obj.ColBitName, Obj.ColBitDescription, Obj.ColBitInd};
            elseif iscell(BinDic)
                Obj.Dic = cell2table(BinDic);
            else
                error('Illegal BinDic class option');
            end
            
            % make sure the dictionary is formatted correctly
            MaxBitInd = max(Obj.Dic.(Obj.ColBitInd));
            Ndigits  = ceil(log(MaxBitInd)./log(2));  % number of binary digits required to represent the dictionary
            if MaxBitInd<=8
                Obj.Nbit  = 8;
                Obj.Class = 'uint8'; 
            else
                if MaxBitInd<=16
                    Obj.Nbit = 16;
                    Obj.Class = 'uint16'; 
                else
                    if MaxBitInd<=32
                        Obj.Nbit = 32;
                        Obj.Class = 'uint32'; 
                    else
                        if MaxBitInd<=64
                            Obj.Nbit = 64;
                            Obj.Class = 'uint64'; 
                        else
                            error('More than 64 bits in BitDictionary are not supported');
                        end
                    end
                end
            end
            % add missing entries with empty indices
            
            NewCell = cell(Obj.Nbit,3);
            VecBit  = (0:1:Obj.Nbit-1);
            NewCell(:,3) = num2cell(VecBit);
            NewTbl = cell2table(NewCell);
            for I=1:1:size(Obj.Dic,1)
                J = (Obj.Dic.(Obj.ColBitInd)(I)==VecBit);
                NewTbl(J,:) = Obj.Dic(I,:);
            end
            
            Obj.Dic = NewTbl;
            Obj.Dic.Properties.VariableNames = {Obj.ColBitName, Obj.ColBitDescription, Obj.ColBitInd};
            
        end
    end
    
    methods
        function [BitName,BitDescription,BitInd]=bitind_to_name(Obj,BitVal,Args)
            % Convert an array of bit indices to bit names
            % Input  : - A single element BitDictionary object
            %          - An array of bit indices (i.e., the running number of the
            %            bit in the dictionary).
            %          * ...,key,val,...
            %            'NotExist' - ['nan'] | 'fail'. Indicating what to
            %                   do if the bit index is out of range.
            %                   'nan' will return NaN, 'fail' will produce
            %                   an error.
            % Output : - A cell array f bit names (same size as BitVal).
            %          - A cell array of bit descriptions.
            %          - An array of the input bit values, but with NaN if
            %            the bit index is out of range.
            % Author : Eran Ofek (Mar 20201)
            % Example: [BitName,BitDescription,BitInd]=bitind_to_name(Obj,[1 4;7 12])
            
            arguments
                Obj(1,1)
                BitVal uint32
                Args.NotExist char   {mustBeMember(Args.NotExist,{'nan','fail'})} = 'nan';
            end
            
            Nbitval        = numel(BitVal);
            BitName        = cell(size(BitVal));
            BitDescription = cell(size(BitVal));
            
            BitInd = zeros(size(BitVal));
            for Ibit=1:1:Nbitval
                Flag = Obj.Dic.BitInd==BitVal(Ibit);
                if any(Flag)
                    BitName{Ibit}        = Obj.Dic.(Obj.ColBitName){Flag};
                    BitDescription{Ibit} = Obj.Dic.(Obj.ColBitDescription){Flag};
                    BitInd(Ibit)         = Obj.Dic.(Obj.ColBitInd)(Flag);
                else
                    switch Args.NotExist
                        case 'nan'
                            BitName{Ibit}        = '';
                            BitDescription{Ibit} = '';
                            BitInd(Ibit)         = NaN;
                        case 'fail'
                            error('BitInd not exist in dictionary %s',Obj.Name);
                    end
                end
            end
        end
       
        function [BitName,BitDescription,BitInd]=bitdec_to_name(Obj,DecVal)
            % Convert decimal numbers to bit names and indices
            % Input  : - A single element BitDictionary object.
            %          - An array of decimal values (DecVal).
            % Output : - A cell array of cell array of bit names.
            %            Each cell elements corresponds to an element in
            %            the input DecVal array.
            %          - A cell array of cell array of bit descritions.
            %          - A cell array of bit indices.
            % Example: [BitName,BitDesc,BitInd]=bitdec_to_name(Obj,[3,1,2^11+7; 1 1 1])
            
            arguments
                Obj(1,1)
                DecVal
            end
        
            
            BitName          = cell(size(DecVal));
            BitDescription   = cell(size(DecVal));
            BitInd           = cell(size(DecVal));
            Nbitval   = numel(DecVal);
            Flag = logical(de2bi(DecVal,Obj.Nbit)); % dec 2 bin and cast as logicals
            for Ibit=1:1:Nbitval
                BitName{Ibit}        = Obj.Dic.(Obj.ColBitName)(Flag(Ibit,:));
                BitDescription{Ibit} = Obj.Dic.(Obj.ColBitDescription)(Flag(Ibit,:));
                BitInd{Ibit}         = Obj.Dic.(Obj.ColBitInd)(Flag(Ibit,:));
            end

        end
        
        function [BitInd,BitDec,BitDescription]=name_to_bit(Obj,Name)
        end
        
        
    end
    
    
    methods % unit Test
        function unitTest(Obj)
            %
        
            Tbl = cell2table({'Saturation', 'Saturated pixel', 0; ...
                              'DeadPix',    'Pixel level at Flat is low', 1; ...
                              'NoisyPix',   'High noise in dark image', 3;...
                              'Spike',      'Stellar spike',            11});
                          
            Tbl.Properties.VariableNames = {'BitName','BitDescription','BitInd'};
            Obj.Dic = Tbl;
            
            [BitName,BitDescription,BitInd]=bitind_to_name(Obj,[0 1 17])
            [BitName,BitDesc,BitInd]=bitdec_to_name(Obj,[3,1,2^11+7; 1 1 1])
            
        end
    end
    
end
