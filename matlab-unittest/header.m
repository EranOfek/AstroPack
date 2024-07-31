% AstroPack Unit-Test
% Target Function: util/+tools/+struct/isfield_check.m
%
% Brief Description:
% This unit test verifies the functionality of the isfield_check function.
%
% Detailed Description:
% The isfield_check function checks if a specified field exists in a given 
% structure. If the field exists, it applies a specified function (default 
% is @all) to the field's content and returns the function's output. 
% Otherwise, it returns false.
% 
% This unit test covers the following scenarios:
% - When the field exists and the default function (@all) is used.
% - When the field exists and a custom function (@any) is used.
% - When the field does not exist.
% - When the structure is empty.
% - When the field exists but is empty.
%
% Created: 17/06/2024
% Author:  Chen Tishler
%--------------------------------------------------------------------------
