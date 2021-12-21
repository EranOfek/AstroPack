% Unit Tester shortcuts, calls actual functions in UnitTester.m
%
% Author: Chen Tishler (June 2021)
%
% Usage:
%   ut.bpush() - Runs all Unit-Tests before git push
%                If errors found, you should NOT push your changes to the main 
%                working branch (dev1/master/etc.)
%

% #functions (autogen)
% bpush - Call to perform tests before git push - PUSH ONLY IF ALL TESTS PASS
% diff - Call to perform tests before git push - PUSH ONLY IF ALL TESTS PASS
% test - Run all unit tests
% unitTest -
% #/functions (autogen)
%

classdef ut < handle

    methods(Static)

        function Result = test()
            % Run all unit tests
            % Output:  true on sucess
            % Example: ut.test()
            Tester = UnitTester;
            Result = Tester.doTest();
        end

        
        function Result = bpush()
            % Call to perform tests before git push - PUSH ONLY IF ALL TESTS PASS
            % Output:  true on sucess
            % Example: ut.bpush()            
            Result = UnitTester.beforePush();
        end

        
        function Result = diff()
            % Call to perform tests before git push - PUSH ONLY IF ALL TESTS PASS
            % Output:  true on sucess
            % Example: ut.diff()            
            persistent Tester
            if isempty(Tester)
                Tester = UnitTester;
            end
           
            Result = Tester.doBeforePush();
        end

        
        function Result = unitTest()
            % Dummy function to avoid warnings about missing unitTest()
            Result = true;
        end
        
    end
end
