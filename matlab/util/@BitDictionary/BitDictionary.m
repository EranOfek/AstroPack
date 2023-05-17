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
        Dic struct           = struct('BitName',[],'BitDescription',[],'BitInd',[]);    % Name, Description, BitInd
    end
    properties (SetAccess=private)
        Nbit                 = 8; % setter by constructor
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
            % Input  : Dictionary file name to load.
            %          Default is 'BitMask.Image.Default'
            % Example: BD=BitDictionary('BitMask.Image.Default')
            %          BD=BitDictionary('BitMask.MergedCat.Default')
            % Author : Eran Ofek (May 2021)
            
            arguments
                DictionaryName char    = 'BitMask.Image.Default';
            end
           
            Obj.Name = DictionaryName;
            if ~isempty(DictionaryName)
                St  = eval(sprintf('Obj.Config.Data.%s',DictionaryName));
                FN  = fieldnames(St);
                Frem = strcmp(FN, 'FileName');
                FN   = FN(~Frem);
                Nfn = numel(FN);
                Obj.Dic.(Obj.ColBitName)        = cell(Nfn,1);
                Obj.Dic.(Obj.ColBitDescription) = cell(Nfn,1);
                Obj.Dic.(Obj.ColBitInd)         = nan(Nfn,1);
                for Ifn=1:1:Nfn
                    Obj.Dic.(Obj.ColBitName){Ifn}        = FN{Ifn};
                    Obj.Dic.(Obj.ColBitDescription){Ifn} = St.(FN{Ifn}){2};
                    Obj.Dic.(Obj.ColBitInd)(Ifn)         = St.(FN{Ifn}){1};
                end
                
                Options  = [8 16 32 64];
                Obj.Nbit = Options(find(ceil(Nfn./Options)==1,1,'first'));
                switch Obj.Nbit
                    case 8
                        Obj.Class = @uint8;
                    case 16
                        Obj.Class = @uint16;
                    case 32
                        Obj.Class = @uint32;
                    otherwise
                        error('Unknown Nbit option - only 8, 16, 32 are supported');
                end
                Obj.BitDictName = DictionaryName;
            end
        end
    end
    
    methods % setters/getters
%         function set.Dic(Obj,BinDic)
%             % setter for Dic property
%             % BinDic can be: 'table' | 'cell'
%             
%             if istable(BinDic)
%                 Obj.Dic.(Obj.ColBitName)        = table2array(BinDic(:,1));
%                 Obj.Dic.(Obj.ColBitDescription) = table2array(BinDic(:,2));
%                 Obj.Dic.(Obj.ColBitInd)         = table2array(BinDic(:,3));
%                 
%             elseif iscell(BinDic)
%                 Obj.Dic.(Obj.ColBitName)        = BinDic(:,1);
%                 Obj.Dic.(Obj.ColBitDescription) = BinDic(:,2);
%                 Obj.Dic.(Obj.ColBitInd)         = cell2array(BinDic(:,3));
%             else
%                 error('Illegal BinDic class option');
%             end
%             
%             % make sure the dictionary is formatted correctly
%             MaxBitInd = max(Obj.Dic.(Obj.ColBitInd));
%             Ndigits  = ceil(log(MaxBitInd)./log(2));  % number of binary digits required to represent the dictionary
%             if MaxBitInd<=8
%                 Obj.Nbit  = 8;
%                 Obj.Class = @uint8;
%             else
%                 if MaxBitInd<=16
%                     Obj.Nbit = 16;
%                     Obj.Class = @uint16;
%                 else
%                     if MaxBitInd<=32
%                         Obj.Nbit = 32;
%                         Obj.Class = @uint32;
%                     else
%                         if MaxBitInd<=64
%                             Obj.Nbit = 64;
%                             Obj.Class = @uint64;
%                         else
%                             error('More than 64 bits in BitDictionary are not supported');
%                         end
%                     end
%                 end
%             end
%             % add missing entries with empty indices
%             
%             NewCell = cell(Obj.Nbit,3);
%             VecBit  = (0:1:Obj.Nbit-1);
%             NewCell(:,3) = num2cell(VecBit);
%             for I=1:1:size(Obj.Dic,1)
%                 J = (Obj.Dic.(Obj.ColBitInd)(I)==VecBit);
%                 NewCell(J,:) = Obj.Dic(I,:);
%             end
%             
%             Obj.Dic = NewTbl;
%             Obj.Dic.Properties.VariableNames = {Obj.ColBitName, Obj.ColBitDescription, Obj.ColBitInd};
%             
%         end
        
    end
    
    methods % utilities
        function Result = isemptyBitDict(Obj)
            % Return true if a BitDictionary contains no info
            % Example: Result = isemptyBitDict(BitDictionary)
            
            Nobj = numel(Obj);
            Result = true(size(Obj));
            for Iobj = 1:1:Nobj
                Result(Iobj) = isempty(Obj.Dic.BitName);
            end
            
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
            % Author : Eran Ofek (Mar 2021)
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
                Flag = Obj.Dic.(Obj.ColBitInd)==BitVal(Ibit);
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
            % Use int2bit instead
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
            % Author : Eran Ofek (Mar 2021)
            % Example: [BitInd,BitDec,SumBitDec,BitDescription]=name2bit(Obj,{'Spike','DeadPix'})
            
            arguments
                Obj
                BitName       % char or cell
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
            SumBitDec = sum(BitDec,'all','omitnan');
                
        end
        
    end
    
    methods % search
        function [Flag] = findBit(Obj, DecFlags, BitNames, Args)
            % Search decimal flags array for entries in which some bits are open 
            % Input  : - A single element BitDictionary object.
            %          - An array of decimal flags in which to search for
            %            the open bits.
            %          - A cell array of bit names, or a decimal number
            %            representing several bits to search in the array
            %            of decimal flags.
            %          * ...,key,val,...
            %            'Method' - Indicating if to look for entries in
            %                   which all the requested bits are on
            %                   ('all'), or one or more of the requested
            %                   bits are on ('any').
            %                   Default is 'any'.
            % Output : - An array of logicals indicating if each entry in
            %            the array of decimal flags contains one/all the requested
            %            bits.
            % Author : Eran Ofek (Nov 2021)
            % Example:
            
            
            arguments
                Obj(1,1)
                DecFlags
                BitNames
                Args.Method           = 'any';     % 'all' | 'any'
            end
            
            if ischar(BitNames)
                BitNames = {BitNames};
            end
            
            if isempty(BitNames)
                Flag = false(size(DecFlags));
            else
                if iscell(BitNames) || isstring(BitNames)
                    [~,~,SumBitDec,~] = name2bit(Obj, BitNames);
                else
                    SumBitDec = BitNames;
                end

                switch lower(Args.Method)
                    case 'all'
                        Flag = bitand(DecFlags, SumBitDec) == SumBitDec;

                    case 'any'
                        Flag = bitand(DecFlags, SumBitDec) > 0;

                    otherwise
                        error('Unknown Method option');
                end
            end
            
        end
        
        
    
    end
    
    
    methods (Static) % unit Test
        Result = unitTest(Obj)
            %

    end
    
end
