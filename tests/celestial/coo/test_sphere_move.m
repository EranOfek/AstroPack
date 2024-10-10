function tests = test_sphere_move
    % Unit tests for the sphere_move function, which applies offsets to RA and Dec coordinates.
    %
    %  The function itself is scriptless - beside one row >>> reckon.
    % reckon do not get ant arguments in this funciton (resulting in an
    % error) - Understand whats going on.
    % Author: Yarin Shani
    %

    tests = functiontests(localfunctions);
end

%% Test Functions

function testZeroOffset(testCase)
    % Test if the function returns the same RA and Dec when no offset is applied.
    RA = pi / 4;  % RA in radians
    Dec = pi / 6; % Dec in radians
    OffsetRA = 0;
    OffsetDec = 0;
    Units = 'rad';
    
    % Call sphere_move function
    [newRA, newDec] = reckon(RA, Dec, OffsetRA, OffsetDec, Units);
    
    % Verify that the RA and Dec remain the same
    verifyEqual(testCase, newRA, RA, 'AbsTol', 1e-9, 'RA should not change with zero offset.');
    verifyEqual(testCase, newDec, Dec, 'AbsTol', 1e-9, 'Dec should not change with zero offset.');
end

function testSmallOffset(testCase)
    % Test if the function correctly applies small offsets to RA and Dec.
    RA = pi / 4;  % RA in radians
    Dec = pi / 6; % Dec in radians
    OffsetRA = 1e-3; % Small offset in radians
    OffsetDec = -1e-3;
    Units = 'rad';
    
    % Call sphere_move function
    [newRA, newDec] = reckon(RA, Dec, OffsetRA, OffsetDec, Units);
    
    % Verify the new RA and Dec are correctly offset
    verifyEqual(testCase, newRA, RA + OffsetRA, 'AbsTol', 1e-9, 'Incorrect RA after applying small offset.');
    verifyEqual(testCase, newDec, Dec + OffsetDec, 'AbsTol', 1e-3, 'Incorrect Dec after applying small offset.');
end
