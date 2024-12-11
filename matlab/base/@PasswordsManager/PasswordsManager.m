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
%   File name should be Passwords.yml
%   
%   File content:
%   e.g.,  
%       PTF : ['eran', 'palomar', 'eran.ofek@weizmann.ac.il', 'VO.PTF.wget_corrim', 'http://']
%   For Project name, user, password, e-mail, functions, URL
%   Next,
%   If needed reload the configuration:
%   C=Configuration;
%   C.reloadSysConfig
%
%   PM = PasswordsManager;
%   PM.Config.reloadSysConfig
%   [Result]=PM.search('ptf') 
%   [Result]=PM.search('VO.PTF.wget_corrim','fun',false)
%

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
        function Result = getAllData(Obj)
            % Get the content of the Passwords.yml file
            % Input  : - self.
            % Output : - A structure with the content of the Passwords.yml file
            % Author : Eran Ofek (Dec 2024)
            % Example: PM=PasswordsManager; R = PM.getAllData

            Result = Obj.Config.Data.(Obj.PassFile);

        end

        function [Result]=getProjects(Obj)
            % Get the list of all project names stored in the Passwords.yml file
            % Input  : - self.
            % Output : - A cell array of all the project names stored in the Passwords.yml file
            % Author : Eran Ofek (Dec 2024)
            % Example: PM=PasswordsManager; R = PM.getProjects
            
            All   = getAllData(Obj);
            Result = fieldnames(All);

        end

        function Result = getAllKey(Obj, Key)
            % Get specific Key (e.g., user or password) for all projects
            % Input  : - self.
            %          - Key: options are:
            %               'user'|'pass'|'email'|'functions'|'url'
            %               Default is 'user'.
            % Output : - A cell array of key in all projects (e.g., all
            %            users).
            % Author : Eran Ofek (Dec 2024)
            % Example: getAllKey(PM, 'User')

            arguments
                Obj
                Key   = 'user';
            end

            switch lower(Key)
                case 'user'
                    KeyInd = 1;
                case 'pass'
                    KeyInd = 2;
                case 'email'
                    KeyInd = 3;
                case 'functions'
                    KeyInd = 4;
                case 'url'
                    KeyInd = 5;
                otherwise
                    error('Unknown Key option');
            end

            All    = getAllData(Obj);
            FN     = fieldnames(All);
            % -1 to remove FileName
            Nfn    = numel(FN) - 1;
            Result = cell(Nfn,1);
            for Ifn=1:1:Nfn
                
                ProjCell = All.(FN{Ifn});
                if KeyInd<=numel(ProjCell)
                    Result{Ifn} = ProjCell{KeyInd};
                end
            end
        end

        function Result = toStruct(Obj)
            % Convert the Passwords.yml to a structure array of passwords.
            % Input  : - self.
            % Output : - A structure array with element per project, and
            %            the following fields:
            %            .Project
            %            .User
            %            .Pass
            %            .EMail
            %            .Functions
            %            .URL
            % Author : Eran Ofek (Dec 2024)
            % Example: S=PM.toStruct;

            All    = getAllData(Obj);
            FN     = fieldnames(All);
            % -1 to remove FileName
            Nfn    = numel(FN) - 1;
            Result = struct('Project',cell(Nfn,1), 'User', cell(Nfn,1), 'Pass',cell(Nfn,1), 'EMail',cell(Nfn,1), 'Functions',cell(Nfn,1), 'URL',cell(Nfn,1));

            for Ifn=1:1:Nfn
                Result(Ifn).Project = FN{Ifn};
                Result(Ifn).User      = All.(FN{Ifn}){1};
                Result(Ifn).Pass      = All.(FN{Ifn}){2};
                Result(Ifn).EMail     = All.(FN{Ifn}){3};
                Result(Ifn).Functions = All.(FN{Ifn}){4};
                Result(Ifn).URL       = All.(FN{Ifn}){5};
            end

        end


        function [User, Pass, EMail, Fun, Url] = getUserPassword(Obj, Project, User)
            % Get User/Pass for specific project and user.
            % Input  : - self.
            %          - Project name.
            %          - Optional user name. If empty, all users.
            %            Default is [].
            % Output : - User
            %          - Pass
            %          - EMail
            %          - Fun
            %          - URL
            % Author : Eran Ofek (Dec 2024)
            % Example: [User,Pass]=PM.getUserPassword('LASTDB_Root')

            arguments
                Obj
                Project
                User   = [];
            end

            St = Obj.toStruct;
            AllProjects = {St.Project};
            AllUsers    = {St.User};
            
            if isempty(User)
                Ind = find(strcmp(AllProjects, Project));
            else
                Ind = find(strcmp(AllProjects, Project) & strcmp(AllUsers, User));
            end

            switch numel(Ind)
                case 0
            
                    % no match
                    User  = [];
                    Pass  = [];
                    EMail = [];
                    Fun   = [];
                    Url   = [];
                    error('No project/user match')
                case 1
                    % single match
                    User      = AllUsers{Ind};
                    AllPass   = {St.Pass};
                    Pass      = AllPass{Ind};
                    AllEMail  = {St.EMail};
                    EMail     = AllEMail{Ind};
                    AllFun    = {St.Functions};
                    Fun       = AllFun{Ind};
                    AllURL    = {St.URL};
                    Url       = AllURL{Ind};
                otherwise
                    % multiple matches
                    User  = [];
                    Pass  = [];
                    EMail = [];
                    Fun   = [];
                    Url   = [];
                    error('Multiple project/user match')
            end

        end



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
