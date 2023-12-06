function St = reshapeFields(St, NewSize, Behav)
    % Reshape all fields which have a consistent size
    %   Given a NewSize vector, the program will search fields in which
    %   their first dimension length is equal to prod(NewSize), and will
    %   reshape these fields intwo [NewSize, additionl_dim]
    % Input  : - A structure array
    %          - A vector of new size. (vector, or multiple arguments).
    %          - Behavior:
    %               'first' - compare only the first dim of the field to
    %                       prod(NewSize). Default.
    %               'all' - compare the numel of the field to
    %                       prod(NewSize).
    % Output : - If a field contains a vector which number of elements is
    %            consistent with the new size, then the fields will be
    %            reshaped with this new size.
    % Author : Eran Ofek (Aug 2021)
    % Example: St.A = ones(20,1); St(1).B = 1; St(2).A=ones(20,1); St(2).B=ones(20,1);
    %          St = tools.struct.reshapeFields(St, [5, 4]);
   
    arguments
        St struct
        NewSize(1,:)
        Behav char     = 'first';
    end
    
    FieldNames = fieldnames(St);
    Nfn        = numel(FieldNames);
    N = numel(St);
    for I=1:1:N
        for Ifn=1:1:Nfn
            switch lower(Behav)
                case 'all'
                    if numel(St(I).(FieldNames{Ifn})) == prod(NewSize)
                        % field is reshapeable
                        % reshape
                        St(I).(FieldNames{Ifn}) = reshape(St(I).(FieldNames{Ifn}), NewSize);
                    end
                case 'first'
                    if size(St(I).(FieldNames{Ifn}),1) == prod(NewSize)
                        % reshape only first dim
                        SizeSt = size(St(I).(FieldNames{Ifn}));
                        St(I).(FieldNames{Ifn}) = reshape(St(I).(FieldNames{Ifn}), [NewSize, SizeSt(2:end)]);
                    end
                otherwise
                    error('Unknown Behav option');
            end
        end
    end
        
end

