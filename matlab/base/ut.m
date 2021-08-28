% Unit Tester shortcuts, calls actual functions in UnitTester.m
% 
%   ut.bpush() - Runs all Unit-Tests before git push
%

classdef ut < handle    
    
    methods(Static)        
        
        function Result = test()
            % Run all unit tests
            Tester = UnitTester;
            Result = Tester.doTest();
        end
                
        function Result = bpush()
            % Call to perform tests before git push - PUSH ONLY IF ALL TESTS PASS
            Result = UnitTester.beforePush();
        end
    end       
end
