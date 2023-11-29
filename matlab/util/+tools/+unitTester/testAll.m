function [Result, Failed] = testAll(Args)
    % Search and execute all the unitTest functions in AstroPack
    % Input  : * ...,key,val,... 
    %            See code for details.
    % Output : - A strucure array with the results of running each of the
    %            unitTest in AstroPack.
    %            Including the following fields:
    %            .FH - unitTest function handle.
    %            .Result - A logical result for the unitTest.
    %            .FunPath - Function name and path.
    % Author : Eran Ofek (2023 Nov) 
    % Example: R=tools.unitTester.testAll()

    arguments
        Args.UnitTestFunName   = 'unitTest';  % unitTest function name
        Args.SkipClass         = {'FileProcessor'};
        Args.SkipPackage       = {};
    end

    % for all Classes
    ListC = tools.code.listClasses;
    Nl    = numel(ListC);
    I     = 0;
    for Il=1:1:Nl
        if ~any(strcmp(ListC{Il}, Args.SkipClass))
            FunName  = sprintf('%s.%s',ListC{Il}, Args.UnitTestFunName);
            ResWhich = which(FunName);
            if ~isempty(ResWhich)
                I = I + 1;
                % unitTest exist
                FH       = str2func(FunName);
                fprintf('Execute: %s\n',FunName);
                try
                    ResF  = FH();
                    %ResF = true;
                catch
                    ResF = false;
                end
                Result(I).FunH    = FH;
                Result(I).FunName = func2str(FH);
                Result(I).Result  = ResF;
                Result(I).FunPath = ResWhich;
            end
        end
    end
    
    % for all Packages
    ListP = tools.code.listPackages;
    Np    = numel(ListP);
    for Ip=1:1:Np
        if ~any(strcmp(ListP{Ip}, Args.SkipPackage))
            FunName  = sprintf('%s.%s',ListP{Ip}, Args.UnitTestFunName);
            ResWhich = which(FunName);
            if ~isempty(ResWhich)
                I = I + 1;
                % unitTest exist
                FH       = str2func(FunName);
                fprintf('Execute: %s\n',FunName);
                try
                    ResF  = FH();
                    %ResF = true;
                catch
                    ResF = false;
                end
                Result(I).FunH    = FH;
                Result(I).FunName = func2str(FH);
                Result(I).Result  = ResF;
                Result(I).FunPath = ResWhich;
            end
        end
    end
    
    % List of failed unitTest
    Failed = Result(~[Result.Result]);
    
end
