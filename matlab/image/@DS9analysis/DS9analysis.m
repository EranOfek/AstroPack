% A static class to control and manipulate ds9
% Description: A static class for intearction with the ds9 display.
%              This include functions to load images, change their
%              properties, create and plot region files, printing, image
%              examination, interaction with SIM content and more.
%              Type "ds9." followed by <tab> to see the full list of
%              functions.
%              Full manual is available in manual_ds9.pdf
% Input  : null
% Output : null
% Tested : Matlab R2014a
%     By : Eran O. Ofek                    Jul 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Reliable: 2
%--------------------------------------------------------------------------


classdef DS9analysis < handle

    properties
        Current           = 1; % Index or 'prev'/...
        Images            % AstroImage | FileNames | cell


    end

    % A static class
    
    
    methods % Constructor method
        function Obj = DS9analysis(Image,varargin)
            %

            % open ds9 if doesn't exist

        end

    end

    methods % setters/getters
        function set.Current(Obj, Input)
            % setter for Current
            % Input  : - self.
            %          - One of the following:
            %            A vector of indices of elements in the Images
            %               property to load to the Current property.
            %            'prev'|'next'|'first'|'last' - In the current frame
            %               load previous/... image from Images.
           
            if isnumeric(Input)
                % do nothing
            elseif ischar(Input)
                % change only current frame
                CurrentFrame = ds9.frame;
                Nim = numel(Obj.Images);
                switch lower(Input)
                    case 'first'
                        Obj.Current(CurrentFrame) = 1;
                    case 'last'
                        Obj.Current(CurrentFrame) = Nim;
                    case 'prev'
                        Ind = Obj.Current(CurrentFrame);
                        Ind = Ind - 1;
                        Ind = mod(Ind, Nim) + 1;
                        Obj.Current(CurrentFrame) = Ind;
                    case 'next'
                        Ind = Obj.Current(CurrentFrame);
                        Ind = Ind + 1;
                        Ind = mod(Ind, Nim) + 1;
                        Obj.Current(CurrentFrame) = Ind;
                    otherwise
                        error('Unknown Input option');
                end
            else
                error('Unsupported Input class');
            end
            
            Obj.Input = Input;
            
            % check consistency
            
            if max(Input)>numel(Obj.Images)
                error('Maximum index specified (%d) is larger than the number of elements in Images (%d)', max(Input), numel(Obj.Images));
            end

            Nim = numel(Obj.Input);
            for Iim=1:1:Nim
                Ind = Obj.Input(Iim);
                ds9.disp(Obj.Images(Ind), Iim);
            end
                
        end

        function set.Images(Obj, Input)
            % setter for Images
            % Input  : - An AstroImage | FileNames | cell object

            switch class(Obj)
                case 'AstroImage'
                    Obj.Images = Input;

                    Obj.Current = Obj.Images(1);
                case 'FileNames'

                case 'cell'

                otherwise
                    error('Images property must be of AStroImages or FileNames class');
            end

        end
    end

    methods % display
        % frame - go to frame
        % 
    end

    methods  % switch images in frame
        % sortByTime
        % goto: next | prev | first | last | ind
        
    end



    methods (Static) % Unit-Test
        Result = unitTest()
            % unitTest for ds9

    end
    
    
end % end class
            
