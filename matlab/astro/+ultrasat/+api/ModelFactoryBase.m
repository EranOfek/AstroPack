%==========================================================================
% ULTRASAT 
%
% File:   ModelFactoryBase.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 11/02/2025
%
%==========================================================================

classdef ModelFactoryBase < handle
    % ModelFactoryBase - Base class for factories that create model instances.
    %
    % This class provides a factory pattern for creating structured data 
    % models that align with the Python FastAPI backend. It serves as an 
    % abstraction layer for generating MATLAB structs that match Pydantic 
    % models used in FastAPI.
    %
    % Key Features:
    % - Defines a standard structure for model creation.
    % - Converts MATLAB structs to JSON format.
    % - Used in conjunction with `ModelBase` to ensure compatibility with 
    %   the backend API
    %
    % Typical Usage:
    % - Subclasses of `ModelFactoryBase` define specific models.
    
    methods (Static)

    end    
end
