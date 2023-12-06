%--------------------------------------------------------------------------
% HEAD class
% Description: A class of for an image header (Header).
%              This class can be use to store header meta data information
%              associated with images or spectra, and to access and search
%              this information.
%              An header information consist of multiple keyword names.
%              Each keyword have a value and an optional comment.
%              The Header information can be stored in a 
%              three columns cell array {Keyword,Value,Comment}.
% Input  : null
% Output : null
% Tested : Matlab R2014a
%     By : Eran O. Ofek                    Nov 2014
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Reliable: 2
%--------------------------------------------------------------------------

classdef HEAD < WorldCooSys
    properties (SetAccess = public)
        Header
    end
    
    % consider add hidden properties:
    % isaligned
    
    methods
       
        function Obj = HEAD(varargin)
            % constructor
            Obj(1).Header = cell(0,3);
            Obj(1).UserData = [];
            %obj = struct_def({'Header','UserData'},varargin{:});
            
        end
        
    end
    

    methods (Static)
        
        function Result = ishead(Obj)
            % Return true if object is HEAD
            % Description: Check if object is of HEAD class.
            % Input  : - Object
            % Output : - {true|false}.
            %     By : Eran O. Ofek                    Oct 2015
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Example: S=1; HEAD.ishead(S);
            % Reliable: 2
            Result = isa(Obj,'HEAD');
        end
        
        
        function Result = HeaderField
            % Description: Return the header field name in HEAD
            % Input  : null
            % Output : Header field name
            Result = 'Header';
        end        
    end
    
    
    methods 
        
        function Result = isheader(Head)
            % Description: check if object is an Header class
            % Input  : - An object.
            % Output : - True if Header class, othewise false.
            Result = true;
        end
        
        
        %Structre functions ---
        function Result = isfield(Head,Field)
            % isfield 
            Result = any(strcmp(fieldnames(Head),Field));
        end

        function Result = isstruct(Head)
            % isstruct
            Result = true;  %isstruct(Sim) || isa(Sim,'SIM');
        end
        
        
%         function [Gain,Sim]=gain(Sim,varargin)
%             % Description: Get CCD gain from image header and multiply
%             %              each image by its gain.
%             % Input  : - SIM array.
%             %          * Additional arguments to pass to sim_gain.m
%             % Output : - Vector of gain factors for each image in the
%             %            SIM array.
%             %          - SIM array multiplied by the gain.
%             % Example: [Gain, Sim]=Sim.gain;
%             
%             [Gain,Sim]=sim_gain(Sim,varargin{:});
%             
%         end
            
    end
end

            