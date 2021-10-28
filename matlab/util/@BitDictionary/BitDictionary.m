% A BitDictionary class
% Description: A class to support storage/read/write of bit dictionaries
%              including basic conversions between bit names, indices and
%              decimal representations.
% Author: Eran Ofek (March 2021)
% Example: BD=BitDictionary; D.unitTest

% #functions (autogen)
% BitDictionary - Dictionary constructor
% bitdec2name - Convert decimal numbers to bit names and indices
% bitind2name - Convert an array of bit indices to bit names
% name2bit - Convert bit names to bit indices and decimal representation
% read - read a dictionary from file/memnory FFU
% set.Dic - setter for Dic property BinDic can be: 'table' | 'cell'
% write - write a dictionary to file/memory FFU
% #/functions (autogen)
%

classdef BitDictionary < Component
    properties
        BitDictName char     = '';   % BitDictionary name - e.g., 'HeaderKeySynonyms'
        Dic table            = table();    % Name, Description, BitInd
    end
    properties (SetAccess=private)
        Nbit                 = 8; % setter by Dic
        Class                = @uint8;
    end
    properties (Hidden, Constant)
        ColBitName        = 'BitName';
        ColBitDescription = 'BitDescription';
        ColBitInd         = 'BitInd';
    end
   
    methods % constructor
        function Obj=BitDictionary(DictionaryName)
            % Dictionary constructor
            % Input  : Dictionary file name to load. Default is ''.
            % Example: BD=BitDictionary('BitMask.Image.Default')
            
            arguments
                DictionaryName char    = '';
            end
           
            Obj.Name = DictionaryName;
            if ~isempty(DictionaryName)
                St  = eval(sprintf('Obj.Config.Data.%s',DictionaryName));
                FN  = fieldnames(St);
                Nfn = numel(FN);
                Cell = cell(Nfn-1,3);
                for Ifn=1:1:Nfn
                    if iscell(St.(FN{Ifn}))
                        Cell{Ifn,1} = FN{Ifn};
                        Cell(Ifn,2) = St.(FN{Ifn})(2);
                        Cell(Ifn,3) = St.(FN{Ifn})(1);
                    end
                end
                Obj.Dic = cell2table(Cell);
                Obj.BitDictName = DictionaryName;
            end
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
                Obj.Dic.Properties.VariableNames = {Obj.ColBitName, Obj.ColBitDescription, Obj.ColBitInd};
            else
                error('Illegal BinDic class option');
            end
            
            % make sure the dictionary is formatted correctly
            MaxBitInd = max(Obj.Dic.(Obj.ColBitInd));
            Ndigits  = ceil(log(MaxBitInd)./log(2));  % number of binary digits required to represent the dictionary
            if MaxBitInd<=8
                Obj.Nbit  = 8;
                Obj.Class = @uint8;
            else
                if MaxBitInd<=16
                    Obj.Nbit = 16;
                    Obj.Class = @uint16;
                else
                    if MaxBitInd<=32
                        Obj.Nbit = 32;
                        Obj.Class = @uint32;
                    else
                        if MaxBitInd<=64
                            Obj.Nbit = 64;
                            Obj.Class = @uint64;
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
    
    methods % load/save
        function read
            % read a dictionary from file/memnory
            % FFU
        end
        
        function write
            % write a dictionary to file/memory
            % FFU
            
        end
    end
    
    methods % conversions
        function [BitName,BitDescription,BitInd]=bitind2name(Obj,BitVal,Args)
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
            % Example: [BitName,BitDescription,BitInd]=bitind2name(Obj,[1 4;7 12])
            
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
       
        function [BitName,BitDescription,BitInd]=bitdec2name(Obj,DecVal)
            % Convert decimal numbers to bit names and indices
            % Input  : - A single element BitDictionary object.
            %          - An array of decimal values (DecVal).
            % Output : - A cell array of cell array of bit names.
            %            Each cell elements corresponds to an element in
            %            the input DecVal array.
            %          - A cell array of cell array of bit descritions.
            %          - A cell array of bit indices.
            % Example: [BitName,BitDesc,BitInd]=bitdec2name(Obj,[3,1,2^11+7; 1 1 1])
            
            arguments
                Obj(1,1)
                DecVal
            end
        
            
            BitName          = cell(size(DecVal));
            BitDescription   = cell(size(DecVal));
            BitInd           = cell(size(DecVal));
            Nbitval   = numel(DecVal);
            
            % Note: de2bi() requires installation of Communications Toolbox
            % Consider rewriting this function for better performance
            Flag = logical(de2bi(DecVal,Obj.Nbit)); % dec 2 bin and cast as logicals
            for Ibit=1:1:Nbitval
                BitName{Ibit}        = Obj.Dic.(Obj.ColBitName)(Flag(Ibit,:));
                BitDescription{Ibit} = Obj.Dic.(Obj.ColBitDescription)(Flag(Ibit,:));
                BitInd{Ibit}         = Obj.Dic.(Obj.ColBitInd)(Flag(Ibit,:));
            end

        end
        
        function [BitInd,BitDec,SumBitDec,BitDescription]=name2bit(Obj,BitName)
            % Convert bit names to bit indices and decimal representation
            % Input  : - A single element BitDictionary object.
            %          - A string or a cell array of strings containing but
            %            names.
            % Output : - An array of bit indices (the same size as Bit
            %            names.
            %          - An array of bit decimal representations.
            %          - The sum of bits decimal representations.
            %          - A cell array of bit descriptions.
            % Author : Eran Ofek (Mar 20201)
            % Example: [BitInd,BitDec,SumBitDec,BitDescription]=name2bit(Obj,{'Spike','DeadPix'})
            
            arguments
                Obj
                BitName     {mustBeA(BitName,{'char','cell'})}
            end
            
            if isempty(Obj)
                error('BitDictionary must be populated');
            end
            if numel(Obj)>1
                error('BitDictionary must contain a single element');
            end
            
            if ~iscell(BitName)
                BitName = {BitName};
            end
            
            BitInd         = nan(size(BitName));
            BitDec         = nan(size(BitName));
            BitDescription = cell(size(BitName));
            Nbitname = numel(BitName);
            for Ibit=1:1:Nbitname
                Flag = strcmp(Obj.Dic.(Obj.ColBitName),BitName{Ibit});
                switch numel(find(Flag))
                    case 0
                        % not found
                        % default is set to NaN
                    case 1
                        % 1 found
                        BitInd(Ibit)         = Obj.Dic.(Obj.ColBitInd)(Flag);
                        BitDescription{Ibit} = Obj.Dic.(Obj.ColBitDescription)(Flag);
                    otherwise
                        % multiple found
                        error('Multiple bits with the same name exits');
                end
            end
            BitDec = 2.^BitInd;
            SumBitDec = nansum(BitDec,'all');
                
        end
        
    end
    
    
    
    methods (Static) % unit Test
        Result = unitTest(Obj)
            %

    end
    
end
