
classdef MyClassTest < matlab.unittest.TestCase
    properties
        MyClassInstance
    end
    
    methods (TestMethodSetup)
        function createObject(testCase)
            testCase.MyClassInstance = MyClass;
        end
    end
    
    methods (Test)
        function testAdd(testCase)
            actualOutput = testCase.MyClassInstance.add(2, 3);
            expectedOutput = 5;
            testCase.verifyEqual(actualOutput, expectedOutput);
        end
        
        function testSub(testCase)
            actualOutput = testCase.MyClassInstance.sub(5, 3);
            expectedOutput = 2;
            testCase.verifyEqual(actualOutput, expectedOutput);
        end
        
        function testMul(testCase)
            actualOutput = testCase.MyClassInstance.mul(2, 3);
            expectedOutput = 6;
            testCase.verifyEqual(actualOutput, expectedOutput);
        end
    end
end

