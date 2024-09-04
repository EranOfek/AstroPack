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


