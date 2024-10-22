function [Obj, ID] = generateImageID(Obj, Args)
    % Generate image 'unique' ID and store it in the header.
    %       The ID is constructed from multiple pats each containing a pre
    %       defined number of bits and information from a specific header keyword.
    %       The value of each of these header keywords is encoded in the
    %       sub array of bits.
    % Input  : - An AstroImage object.
    %          * ...,key,val,... 
    %            'FormatSt' - A structure array containing information on how
    %                   to construct the ID. The following fields should be
    %                   provided:
    %                   .Key - Header keyword name from which to retrieve
    %                       the ID that will be stored in a sub array of
    %                       bits corresponding to this key.
    %                   .BitNum - The number of bits in the sub array.
    %                   .Fun - A function handle to apply to the keyword
    %                       value in order to get the value to enode in the
    %                       sub array of bits.
    %               Default: see code.
    %            'KeyID' - If not empty, then this is the header keyword
    %                   name in which the generated ID will be written into.
    %                   Default is 'ID_PROC'.
    %            'ErrorOnNaN' - A logical indicating if to generate error
    %                   if keyword value is NaN. If false, will insert 0.
    %                   Default is true.
    % Output : - An AstroImage object with the updated header.
    %          - A vector of IDs (one per image).
    % Author : Eran Ofek (2024 Oct) 
    % Example: [A,ID]=imProc.db.generateImageID(A)

    arguments
        Obj

        Args.FormatSt          = struct("Key",{'IMTYPE','LEVEL','NODENUMB','MOUNTNUM','CAMNUM','CROPID','JD'},...
                                        "BitNum", {4, 5, 5, 5, 3, 6, 36},...
                                        "Fun", {@(x) find(strcmp(x, AstroFileName.ListType)),...
                                                @(x) find(strcmp(x, AstroFileName.ListLevel)),...
                                                @(x) x,...
                                                @(x) x,...
                                                @(x) x,...
                                                @(x) tools.array.replace(x,NaN,0),...
                                                @(jd) uint64((jd-2451545.5).*86400.*10)});

        Args.KeyID             = 'ID_PROC';
        Args.ErrorOnNaN logical = true;
    end

    Nobj = numel(Obj);
    Nsub = numel(Args.FormatSt);
    ID   = zeros(Nobj,1, "uint64");
    for Iobj=1:1:Nobj
        BitNum = zeros(1,Nsub);
        BitVal = zeros(1,Nsub);
        for Isub=1:1:Nsub
            BitNum(Isub) = Args.FormatSt(Isub).BitNum;
            if BitNum(Isub)>0
                % include in keyword value in ID
                TmpVal       = Obj(Iobj).HeaderData.getVal( Args.FormatSt(Isub).Key );
                BitVal(Isub) = Args.FormatSt(Isub).Fun(TmpVal);
                if isnan(BitVal(Isub)) && Args.ErrorOnNaN
                    error('Keyword %s value is NaN', Args.FormatSt(Isub).Key);
                elseif isnan(BitVal(Isub)) && ~Args.ErrorOnNaN
                    BitVal(Isub) = 0;
                else
                    % do nothing
                end
            end
        end

        ID(Iobj) = tools.bit.bitEncode(BitNum, BitVal);
        if ~isempty(Args.KeyID)
            Obj(Iobj).HeaderData.replaceVal(char(Args.KeyID), ID(Iobj));
        end
    end

end
