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
        Current           % AstroImage | if Index, then load image number Ind from Images / per frame
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
            % Input  : - 

            if isa(Input, 'AstroImage')
                % do nothing
                Obj.Current = Input;
                
            elseif isnumeric(Input)
                % Vector of indeces (index per frame), with NaNs for no change
                Nframe = numel(Input);
                for Iframe=1:1:Nframe
                    if isnan(Input(Iframe))
                        % do not change AstroImage content
                    else
                        if Iframe>numel(Obj.Images)
                            error('Number of indeces (%d) must corresponds to the numer of el;ements in Images (%d)',Nframe,numel(Obj.Images));
                        end
                        Obj.Current(Iframe) = Obj.Images(Iframe);
                    end
                end
            elseif ischar(Input)
                % change only current frame
                switch lower(Input)
                    case 'prev'

                    otherwise
                        error('Unknown Input option');
                end
            else
                error('Unsupported Input class');
            end


                Nim = numel(Input);
                for Iim=1:1:Nim
                    ds9.disp(Input(Iim), Iim);
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
            
