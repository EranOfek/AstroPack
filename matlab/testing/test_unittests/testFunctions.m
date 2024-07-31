
classdef testFunctions < matlab.unittest.TestCase
    methods (Test)
        function testFunction1(testCase)
            actualOutput = function1(2, 3);
            expectedOutput = 5;
            testCase.verifyEqual(actualOutput, expectedOutput);
        end
        
        function testFunction2(testCase)
            actualOutput = function2([1, 2, 3]);
            expectedOutput = [3, 2, 1];
            testCase.verifyEqual(actualOutput, expectedOutput);
        end
    end
end

