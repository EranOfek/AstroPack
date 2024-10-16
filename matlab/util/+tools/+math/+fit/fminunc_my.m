function [varargout]=fminunc_my(Fun,varargin)
% fminunc.m version in which it is possible to pass additional parameters to the function
% Description: A version of the built in fminunc.m function in which it
%              is possible to pass additional parameters to the function,
%              with no need for nested functions.
% Input  : - A function handle to minimize, Y=Fun(X).
%            Alternatively, a cell vector in which the first
%            element is the function handle, and the rest of the array
%            contains additional parameters to pass to Fun
%            {@Fun,Par1,Par2,...}.
%            In this case Y=Fun(X,Par1,Par2,...).
%          * Additional parameters as defined in fminsearch.m
%            See fminsaerch.m for details.
%            For example, the first parameter is the guess parameter.
% Output : * The output parameters returned by fminsearch.m 
% Tested : Matlab 2011b
%     By : Eran O. Ofek                    Apr 2013
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Fun = @(X,A)(X-A).^2
%          [X,Fval]=Util.fit.fminunc_my({Fun,2},0.7);
%          [X,Fv,E,O]=Util.fit.fminunc_my(@sin,0.7);
% Reliable: 2
%------------------------------------------------------------------------------

if (~iscell(Fun))
   Fun = {Fun};
end

CallFun = Fun{1};
FunPar  = Fun(2:end);
 
varargout = cell(nargout,1);

[varargout{:}] = fminunc(@call_fun,varargin{:});


   function Y=call_fun(X)
        Y = feval(CallFun,X,FunPar{:});
   end

end
