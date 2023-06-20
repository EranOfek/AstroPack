% PasswordsManager class for managing passwords
%
% Author: Eran Ofek (Apr 2021)
%
% Functionality:
%
% Examples: 
%   Add your passwords data to the configuration directory.
%   Important - do this in the config/local directory, this directory is
%   private and not distributed in github.
%   The format for adding password data:
%   e.g.,  
%       PTF : ['eran', 'palomar', 'eran.ofek@weizmann.ac.il', 'VO.PTF.wget_corrim']
%   For Project name, user, password, e-mail, functions
%   Next,
%   If needed reload the configuration:
%   C=Configuration;
%   C.reloadSysConfig
%
%   PM = PasswordsManager;
%   [Result]=PM.search('ptf') 
%   [Result]=PM.search('VO.PTF.wget_corrim','fun',false)

classdef PasswordsManager < Component
    % Base class for all objects

    % Properties
    properties (SetAccess = public)
        Passwords                  % ProjectName: [User, Pass, E-Mail, FunNames...]
        PassFile   = 'Passwords';
    end

    %--------------------------------------------------------
    methods
        function Obj = PasswordsManager(PassFile)
            % Constructor for PasswordsManager / populated Passwords
            
            arguments
                PassFile    = [];
            end
            
            if isempty(PassFile)
                PassFile = Obj.PassFile;
            end
            
            Obj.Passwords = tools.struct.string2fields(Obj.Config.Data, PassFile);
            if isstruct(Obj.Passwords)
                Obj.Passwords = rmfield(Obj.Passwords, 'FileName');
            end
        end
    end

    methods % search/get passwords from Config
        function [Result]=search(Obj, ProjectName, Type, IsExact)
            % Search user/pass data in passwords configuration object
            %   Search for password data in the config/local directory.
            % Input  : - A PasswordsManager object.
            %          - String of project name, user, password, email, or
            %            function name.
            %          - Search type: 'proj'|'user'|'pass'|'email'|'fun'.
            %            Default is 'proj'.
            %          - A logical indicating if to use exact (strcmpi)
            %            search (true), or (contains) search.
            %            Default is true.
            % Output : - A structure with the following fields:
            %            .Proj
            %            .User
            %            .Pass
            %            .Email
            %            .Funs - A cell array of functions.
            % Author : Eran Ofek (Jun 2023)
            % Example: PM = PasswordsManager;
            %          [Result]=PM.search('ptf')
            %          [Result]=PM.search('VO.PTF.wget_corrim','fun',false)
            
            arguments
                Obj
                ProjectName
                Type            = 'proj';  % 'proj'|'user'|'pass'|'email'|'fun'
                IsExact logical = true;
            end
            
            if isempty(Obj.Passwords)
                error('Passwords configuration file %s was not found',Obj.PassFile);
            else
                if ~isstruct(Obj.Passwords)
                    error('Passwords property must contain a structure');
                end
            end

            FieldsName = fieldnames(Obj.Passwords);
            Nfn        = numel(FieldsName);
            switch lower(Type)
                case 'proj'
                    % search by project name
                    FN = FieldsName;
                    
                case 'user'
                    FN = cell(Nfn,1);
                    for Ifn=1:1:Nfn
                        if numel(Obj.Passwords.(FieldsName{Ifn}))>0
                            FN{Ifn} = Obj.Passwords.(FieldsName{Ifn}){1};
                        else
                            FN{Ifn} = 'null';
                        end
                    end
                case 'pass'
                    FN = cell(Nfn,1);
                    for Ifn=1:1:Nfn
                        if numel(Obj.Passwords.(FieldsName{Ifn}))>1
                            FN{Ifn} = Obj.Passwords.(FieldsName{Ifn}){2};
                        else
                            FN{Ifn} = 'null';
                        end
                    end
                case 'email'
                    FN = cell(Nfn,1);
                    for Ifn=1:1:Nfn
                        if numel(Obj.Passwords.(FieldsName{Ifn}))>2
                            FN{Ifn} = Obj.Passwords.(FieldsName{Ifn}){3};
                        else
                            FN{Ifn} = 'null';
                        end
                    end
                case 'fun'
                    FN = cell(Nfn,1);
                    for Ifn=1:1:Nfn
                        if numel(Obj.Passwords.(FieldsName{Ifn}))>3
                            FN{Ifn} = Obj.Passwords.(FieldsName{Ifn}){4};
                        else
                            FN{Ifn} = 'null';
                        end
                    end
                otherwise
                    error('Unknown Type option');
            end
                    
            if IsExact
                Flag = strcmpi(FN, ProjectName);
            else
                Flag = contains(FN, ProjectName);
            end

            if sum(Flag)>1
                error('More than one password was found');
            end
            
            if sum(Flag)==0
                error('Password not found');
            end
            
            Ind = find(Flag);
            
            Result.Proj = Obj.Passwords.(FieldsName{Ind});
            Result.User = Obj.Passwords.(FieldsName{Ind}){1};
            Result.Pass = Obj.Passwords.(FieldsName{Ind}){2};
            if numel(Obj.Passwords.(FieldsName{Ind}))>2
                Result.Email = Obj.Passwords.(FieldsName{Ind}){3};
            end
            if numel(Obj.Passwords.(FieldsName{Ind}))>3
                Result.Funs = Obj.Passwords.(FieldsName{Ind})(4:end);
            end
            
            
        end
    end
    
    
    %----------------------------------------------------------------------
    methods(Static) % Unit test
        Result = unitTest()
            % unitTest for Base class
    end

end
