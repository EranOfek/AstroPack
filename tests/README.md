# MATLAB Unit-Tests Folder

Chen link: https://chatgpt.com/c/a3cbb7bb-fb14-4d79-bfe8-dfd600a7a67b


Organize this folder in parallel to matlab/ folder, with the same tree structure.

https://www.mathworks.com/matlabcentral/answers/1605290-creating-matlab-tests-in-separate-folder-module


MATLAB supports several ways to write unit-tests:

https://www.mathworks.com/help/matlab/matlab_prog/ways-to-write-unit-tests.html

We use "Function-based unit tests"

https://www.mathworks.com/help/matlab/matlab_prog/write-function-based-unit-tests.html


## ChatGPT Prompt

Use ChatGPT Pro (paid version), Model '4o'

	Write unit test for this using matlab 'Function-Based Unit Tests'
	Add embedded comments for easy understanding, mark places that need 
	later human modifications with '@TODO - ...'. If there are complex 
	objects (e.g., classes) as inputs and outputs, mark these with 
	'@Object - ...' and include a note about it. 
	Test such objects for expected results where applicable.

## runTestsFromFolder.m

runTestsFromFolder.m need some debugging and fixes.



## Example: Test File: TestStruct2KeyVal.m

function tests = TestStruct2KeyVal
    % TestStruct2KeyVal
    % Function-based unit tests for the struct2keyval function.
    % This function returns a test suite containing multiple test functions,
    % each of which tests a different aspect of the struct2keyval function.
    %
    % Author: Chen Tishler
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testBasicConversion(testCase)
    % Test the basic conversion of a structure to a key-value cell array
    St.A = 1;
    St.B = 2;
    St.C = 3;
    
    % Expected result
    expectedCell = {'A', 1, 'B', 2, 'C', 3};
    
    % Run the function
    resultCell = tools.struct.struct2keyval(St);
    
    % Verify the result
    verifyEqual(testCase, resultCell, expectedCell, 'Basic structure to key-value conversion failed.');
end

function testTwoColFormat(testCase)
    % Test conversion to two-column format
    St.A = 1;
    St.B = 2;
    St.C = 3;
    
    % Expected result
    expectedCell = {'A', 1; 'B', 2; 'C', 3};
    
    % Run the function with TwoCol = true
    resultCell = tools.struct.struct2keyval(St, true);
    
    % Verify the result
    verifyEqual(testCase, resultCell, expectedCell, 'Two-column format conversion failed.');
end

function testEmptyStructure(testCase)
    % Test conversion of an empty structure
    St = struct();
    
    % Expected result
    expectedCell = {};
    
    % Run the function
    resultCell = tools.struct.struct2keyval(St);
    
    % Verify the result
    verifyEqual(testCase, resultCell, expectedCell, 'Empty structure conversion failed.');
end

function testSingleFieldStructure(testCase)
    % Test conversion of a structure with a single field
    St.A = 42;
    
    % Expected result
    expectedCell = {'A', 42};
    
    % Run the function
    resultCell = tools.struct.struct2keyval(St);
    
    % Verify the result
    verifyEqual(testCase, resultCell, expectedCell, 'Single field structure conversion failed.');
end

function testMixedFieldTypes(testCase)
    % Test conversion of a structure with mixed field types
    St.A = 1;
    St.B = 'text';
    St.C = [1, 2, 3];
    
    % Expected result
    expectedCell = {'A', 1, 'B', 'text', 'C', [1, 2, 3]};
    
    % Run the function
    resultCell = tools.struct.struct2keyval(St);
    
    % Verify the result
    verifyEqual(testCase, resultCell, expectedCell, 'Mixed field types conversion failed.');
end

function testNestedStructure(testCase)
    % Test conversion of a nested structure (note: struct2keyval does not flatten nested structures)
    St.A = 1;
    St.B = struct('C', 2, 'D', 3);
    
    % Expected result
    expectedCell = {'A', 1, 'B', St.B};  % Struct should not be flattened
    
    % Run the function
    resultCell = tools.struct.struct2keyval(St);
    
    % Verify the result
    verifyEqual(testCase, resultCell, expectedCell, 'Nested structure conversion failed.');
end

