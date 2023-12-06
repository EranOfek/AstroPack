function Val=get_env(VarName,Numeric)
% attempt to read environment variable and print error if failes
% Package: @imUtil.util
% Input  : - Envirinment variable name.
%          - Logical flag indicating if to attempt to convert variable to
%            numeric. Default is true.
% Output : - Value of environment variable.
% Example: imUtil.util.get_env('imClass.specCl.get_Pickles.DataDir')


if nargin<2
    Numeric = true;
end

Val = getenv(VarName);
if ~isempty(Val)
    if Numeric
        NumVal = str2double(Val);
        if ~isnan(NumVal)
            % a numeric
            Val = NumVal;
        end
    end
else
    % didn't find env var
   
    fprintf('Envrinment variable: %s   : was not found\n',VarName);
    fprintf('In order to fix problem:\n');
    fprintf('  Add setenv(''%s'',ParValue) to your startup.m file\n',VarName);
    error('Envrinment variable was not found');
    
    
end

        
    