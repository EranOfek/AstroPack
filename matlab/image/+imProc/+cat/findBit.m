function [Result] = findBit(Obj, Bits, Col, BD, Args)
    % Execute a logical findBit or a string-like query on bit names and return the logical result.
    %     The input may be an AstroCatalog, table, or a matrix.
    % Input  : - An AstroCatalog object, or a table, or a matrix.
    %          - Either a bit index (starting with 1), or a cell array of
    %            bits name, or a decimal number
    %            representing several bits to search in the array
    %            of decimal flags,
    %            or a string of bits constraints
    %            (e.g., '(~Saturated & ~Negative) | NearEdge')
    %          - Column name, or column index of the bit mask column.
    %            If input object is a matrix, then this must be a number,
    %            or empty. If empty, then will query the entire array (and
    %            not only one column).
    %            Default is 'FLAGS'.
    %          - Either a BitDictionary object, or a BitDictionary Name.
    %            Alternatively this can be one of the following strings:
    %            'Image'|'MergedCat'.
    %            Default is 'BitMask.Image.Default'.
    %            In this case, the BitDictionary Name will be constructed
    %            from: 'BitMask.<String>.Default'.
    %          * ...,key,val,... 
    %            'Method' - Indicating if to look for entries in
    %                   which all the requested bits are on
    %                   ('all'), or one or more of the requested
    %                   bits are on ('any'). This is ignored ig the second
    %                   argument is a string.
    %                   Default is 'any'.
    % Output : - An array of logical results indicating if the query/bits
    %            are satisfied for each input value.
    % Author : Eran Ofek (2025 Mar) 
    % Example: R=imProc.cat.findBit(T, {'Saturated', 'NearEdge'}, 'FLAGS', 'Image')
    %          R=imProc.cat.findBit(T, {'Saturated'}, 'FLAGS', BitDictionary)
    %          R=imProc.cat.findBit(T, 'GAIADR3 & NVSS', 'FLAGS', 'BitMask.MergedCat.Default')
    %          R=imProc.cat.findBit([1 20], '~Saturated & ~NearEdge', [], 'BitMask.Image.Default')

    arguments
        Obj
        Bits
        Col                    = 'FLAGS';
        BD                     = 'BitMask.Image.Default'
        Args.Method            = 'any';
    end
    
    % Populating the BD with a BitDictionary object
    if ischar(BD) || isstring(BD)
        if ~contains(BD, '.')
            switch lower(BD)
                case 'image'
                    BD = 'BitMask.Image.Default';
                case 'mergedcat'
                    BD = 'BitMask.MergedCat.Default';
                otherwise
                    error('Unknown BitDictionary option');
            end
        end
        BD = BitDictionary(BD);
    end
    
    % exctract ColData from Obj
    if isa(Obj, 'AstroCatalog')
        ColData = Obj.getCol(Col);
    elseif istable(Obj)
        if isnumeric(Col)
            ColData = table2array(Obj(:,Col));
        else
            ColData = Obj.(Col);
        end
    elseif isnumeric(Obj)
        if isempty(Col)
            ColData = Obj;
        else
            ColData = Obj(:,Col);
        end
    else
        error('Unknown 1st input argument type');
    end
    
    %
    if iscell(Bits) || isnumeric(Bits)
        Result = BD.findBit(ColData, Bits, 'Method', Args.Method);
    else
        Result = BD.query(ColData, Bits);
    end
        
    
    
    

end
