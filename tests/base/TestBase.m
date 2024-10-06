function tests = TestBase
    % TestBase
    % Function-based unit tests for the Base class.
    % This function returns a test suite containing multiple test functions,
    % each of which tests a different aspect of the Base class.
    %
    % Author: Chen Tishler & Eran Ofek (Apr 2021)
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testConstructor(testCase)
    % Test the Base class constructor
    Obj = Base();  % Create an instance of the Base class
    verifyTrue(testCase, isa(Obj, 'Base'), 'Base class constructor failed.');
end

function testCopyProp(testCase)
    % Test the copyProp method
    Obj1 = Base();  % Source object
    Obj1.UserData = 'TestData';  % Set a property
    
    Obj2 = Base();  % Target object
    copyProp(Obj1, Obj2, 'UserData');  % Copy property from Obj1 to Obj2
    
    % Verify the property was copied correctly
    verifyEqual(testCase, Obj2.UserData, 'TestData', 'copyProp method failed to copy the UserData property.');
end

function testCopyPropNotEmpty(testCase)
    % Test the copyPropNotEmpty method
    Obj1 = Base();  % Source object
    Obj1.UserData = 'TestData';  % Set a property
    
    Obj2 = Base();  % Target object
    Obj2.UserData = [];  % Ensure the target property is empty
    
    copyPropNotEmpty(Obj1, Obj2);  % Copy property only if empty
    
    % Verify the property was copied correctly
    verifyEqual(testCase, Obj2.UserData, 'TestData', 'copyPropNotEmpty method failed to copy the UserData property when empty.');
    
    % Now test with a non-empty target
    Obj2.UserData = 'NotEmpty';
    copyPropNotEmpty(Obj1, Obj2);  % Should not overwrite non-empty property
    
    % Verify the property was not overwritten
    verifyEqual(testCase, Obj2.UserData, 'NotEmpty', 'copyPropNotEmpty method incorrectly overwrote a non-empty property.');
end

function testCreateNewObj(testCase)
    % Test the createNewObj method
    Obj = Base();  % Original object
    Obj.UserData = 'TestData';
    
    % Test with CreateNewObj = true
    [ResultObj, CreateNewObjFlag] = createNewObj(Obj, true, 1);
    verifyTrue(testCase, CreateNewObjFlag, 'createNewObj did not set CreateNewObj flag to true as expected.');
    verifyNotEqual(testCase, ResultObj, Obj, 'createNewObj did not create a new object as expected.');
    verifyEqual(testCase, ResultObj.UserData, 'TestData', 'createNewObj did not correctly copy UserData.');

    % Test with CreateNewObj = false
    [ResultObj, CreateNewObjFlag] = createNewObj(Obj, false, 0);
    verifyFalse(testCase, CreateNewObjFlag, 'createNewObj incorrectly set CreateNewObj flag to true.');
    verifyEqual(testCase, ResultObj, Obj, 'createNewObj did not return the original object as expected.');
end

function testSetProps(testCase)
    % Test the setProps method
    Obj = Base();
    
    Args.UserData = 'NewData';
    Result = Obj.setProps(Args);
    
    % Verify the property was set correctly
    verifyEqual(testCase, Obj.UserData, 'NewData', 'setProps method failed to set the UserData property.');
    verifyEqual(testCase, Result, 1, 'setProps did not return the correct number of fields copied.');
end

function testIsEmptyProperty(testCase)
    % Test the isemptyProperty method
    Obj(1) = Base();
    Obj(2) = Base();
    Obj(2).UserData = 'NotEmpty';
    
    % Check which properties are empty
    Result = Obj.isemptyProperty('UserData');
    
    % Verify the result
    verifyEqual(testCase, Result, [true; false], 'isemptyProperty method did not correctly identify empty properties.');
end

function testCopyElement(testCase)
    % Test the copyElement method (protected method)
    Obj = Base();
    Obj.UserData = 'DeepCopyTest';
    
    % Use the public copy method that calls copyElement internally
    NewObj = Obj.copy();
    
    % Verify that a deep copy was made
    verifyNotEqual(testCase, NewObj, Obj, 'copy method did not create a new object as expected.');
    verifyEqual(testCase, NewObj.UserData, 'DeepCopyTest', 'copy method did not correctly copy UserData.');
end

