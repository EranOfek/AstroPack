function [Result] = read_pkl(FileName, Args)
    % Read python pkl files into matlab
    % Input  : - File name.
    %          * ...,key,val,...
    %            'Convert2double' - A logical indicating if to convert to
    %                   double. Default is true.
    % Output : - Data in matlab format.
    % Author : Eran Ofek (2024 Nov) 
    % Required: pip install pandas
    % Example: Data=io.files.read_pkl('test_data.pkl');

    arguments
        FileName
        Args.Convert2double logical   = true;
    end

    % Initialize Python
    py.importlib.import_module('pickle');
    py.importlib.import_module('pandas');
    
    % Open the .pkl file
    FID = py.open(FileName, 'rb');
    Data = py.pickle.load(FID);
    FID.close();
    
    % If the data is a pandas DataFrame, convert it to a MATLAB struct
    if isa(Data, 'py.pandas.core.frame.DataFrame')
        %Result = struct(py.pandas.DataFrame.to_dict(Data, pyargs('orient', 'list')));
        % Convert the DataFrame to a dictionary
        DictData = Data.to_dict(pyargs('orient', 'list'));
        % Convert the Python dictionary to a MATLAB struct
        Result = struct(DictData);
        if Args.Convert2double
            FN = fieldnames(Result);
            Nfn = numel(FN);
            for Ifn=1:1:Nfn
                Result.(FN{Ifn}) = double(Result.(FN{Ifn}));
            end
        end
    else
        if Args.Convert2double
            Result = double(Data); % Example for numerical data
        else
            Result = Data;
        end
    end
    
end
