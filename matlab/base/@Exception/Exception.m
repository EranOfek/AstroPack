% DO NOT USE THIS CLASS! - Use MException instead, we just need Exception.unitTest()
%
% Exception class (derived from MATLAB's MException class)
%
% Author: Chen Tishler (Dec 2021)
%
% When your program detects a fault that will keep it from completing as expected
% or will generate erroneous results, you should halt further execution and report
% the error by throwing an exception. The basic steps to take are:
%
%    Detect the error. This is often done with some type of conditional statement,
%    such as an if or try/catch statement that checks the output of the current operation.
%
%    Construct an MException object to represent the error. Add an error identifier and
%    error message to the object when calling the constructor.
%
%    If there are other exceptions that may have contributed to the current error, you
%    can store the MException object for each in the cause field of a single MException
%    that you intend to throw. Use the addCause function for this.
%
%    If there is fix that can be suggested for the current error, you can add it to
%    the Correction field of the MException that you intend to throw.
%    Use the addCorrection function for this.
%
%    Use the throw or throwAsCaller function to have MATLAB® issue the exception.
%    At this point, MATLAB stores call stack information in the stack field of the 
% 	 MException, exits the currently running function, and returns control to either 
% 	 the keyboard or an enclosing catch block in a calling function.
%
%
% References:
%   https://www.mathworks.com/help/matlab/matlab_prog/throw-an-exception.html
%   https://www.mathworks.com/help/matlab/matlab_prog/error-reporting-in-a-matlab-application.html
%   https://www.mathworks.com/help/matlab/matlab_prog/respond-to-an-exception.html
%   https://www.mathworks.com/help/matlab/ref/mexception.html
%   https://www.mathworks.com/matlabcentral/answers/41399-subclassing-mexception
%   https://www.mathworks.com/matlabcentral/answers/370457-error-management-in-oop-framework
%
%   https://www.mathworks.com/help/matlab/matlab_prog/throw-an-exception.html
%

classdef Exception < MException
        
    properties % Properties derived from MException    

        % identifier -  Unique identifier of error,  character vector, read-only.
        %               Character vector that uniquely identifies the error, 
        %               specified as a character vector by the errID input argument.
        %               Example: 'MATLAB:test'


        % message    -  Error message, character vector, read-only.
        %               Character vector that contains the error message that is 
        %               displayed when MATLAB throws the exception, specified by 
        %               the msgtext and A1,...,An input arguments.
        %               Example: 'Variable x not found'


        % stack      -  Stack trace information, structure array, read-only.
        %               Structure array that contains stack trace information 
        %               including the file name (file), function name (name), 
        %               and line number (line) where MATLAB throws the exception. 
        %               If the error occurs in a called function, the stack property 
        %               also contains the file name, function name, and line number 
        %               for each of the called functions. MATLAB generates the stack 
        %               only when it throws the exception.
        %               stack is an N-by-1 struct array, where N represents the depth 
        %               of the call stack.
        
        % cause      -  Cause of exception, cell array of MException objects, read-only.
        %               Cell array of MException objects that caused MATLAB to create 
        %               the exception. Use the addCause method to add an exception to 
        %               the cause property.


        % Correction -  Suggested fix for exception, read-only.
        %               Suggested fix for the exception, specified as a matlab.lang.correction.AppendArgumentsCorrection, matlab.lang.correction.ConvertToFunctionNotationCorrection, or matlab.lang.correction.ReplaceIdentifierCorrection object. When an exception is thrown and not caught, MATLAB uses the Correction property to suggest a fix for the exception.

    end
    

    %
    properties
        SourceObj       %
    end
    
    
    methods % Constructor
        
        function Obj = Exception(errID, msgtext, varargin)
            % Constructor - see MException help
            % Input:
            %   errID — Identifier for error, character vector | string scalar
            % Identifier for the error, specified as a character vector or string scalar. Use the error identifier with exception handling to better identify the source of the error or to control a selected subset of the exceptions in your program.
            % 
            % The error identifier includes one or more component fields and a mnemonic field. Fields must be separated with colon. For example, an error identifier with a component field component and a mnemonic field mnemonic is specified as 'component:mnemonic'.
            % 
            %     A component field typically specifies the product or functionality under which various errors can be generated. For example, the error identifier 'MATLAB:TooManyInputs' has a component field MATLAB, which means that the exception is thrown in MATLAB. You can reuse the same mnemonic TooManyInputs as long as you precede it with different components. For example, if you want to throw an exception in your toolbox whenever a function is called with too many inputs, you can use 'MyToolbox:TooManyInputs'.
            % 
            %     The mnemonic field of an error identifier is typically a tag specific to the error issue. For example, when reporting an error resulting from the use of ambiguous syntax in MATLAB, you can specify the error identifier as 'MATLAB:ambiguousSyntax'.

            % The component and mnemonic fields must each begin with a letter. The remaining characters can be alphanumerics (A–Z, a–z, 0–9) and underscores. No white space characters can appear in errID.

            % Example: 'MyComponent:noSuchVariable'

            % Example: 'Simulink:Signals:InvalidNumberOfPorts'


            % MsgText — Information about cause of error, character vector | string scalar
            % Information about the cause of the error and how you might correct it, specified as a character vector or string scalar. To format the text, use escape sequences, such as \t or \n. You also can use any format specifiers supported by the sprintf function, such as %s or %d. Specify values for the conversion specifiers using the A1,...,An input arguments.
            % 
            % Example: 'Error opening file.'
            % Example: 'Error on line %d.'

            % varargin - character vector | string scalar | numeric scalar
            % Values that replace the conversion specifiers in msgtext, each specified as a character vector, string scalar, or numeric scalar.

            Obj = Obj@MException(errID, msgtext, varargin{:});
        end
        
        
        %function Result = getReport(Obj, varargin)
        %    Result = sprintf('Object Error on object %s\n%s', Obj.SourceObj.name, ...
        %        getReport@MException(Obj, varargin{:}));
        %end
    end


    methods %  Object Functions
        % throw()           - Throw exception
        % MException.last()	- Return last uncaught exception
        % rethrow()         - Rethrow previously caught exception
        % throwAsCaller()	- Throw exception as if occurs within calling function
        % addCause()        - Record additional causes of exception
        % addCorrection()	- Provide suggested fix for exception
        % getReport()       - Get error message for exception
    end
    
    
    methods(Static) % Unit test
        Result = unitTest()
            % Unit test
    end
    
end
