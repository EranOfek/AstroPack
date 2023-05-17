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
        Images            % AstroImage | FileNames | cell
        Frame2Ind    = [1];

    end

    
    properties (Hidden)
    end
    
    methods % Constructor method
        function Obj = DS9analysis(Image,varargin)
            %

            % open ds9 if doesn't exist

        end

    end

    methods % setters/getters
        function set.Frame2Ind(Obj, Input)
            % Setter for Frame2Ind (which image index to displa in each frame).
            % Input  : - self.
            %          - A vector of image indices per each frame.
            %            if image index is NaN then skip frame.
            
            
            Nframe = ds9.frame;
            Ninput = numel(Input);
            Nim    = numel(Obj.Images);
            if max(Input)<Nim
                error('Max. image index (%d) must be smaller or equal to the numbre of Images (%d)', max(Input), Nim);
            end
            
            for Iinput=1:1:Ninput
                Iim = Input(Iinput);
                if isnan(Iim)
                    % do not display an image in frame
                else
                    ds9.disp(Obj.Images(Iim), Iinput);
                end
            end
            % delete extra frames
            for I=Nframe:-1:Ninput+1
                ds9.delete_frame;
            end
        end
            
        
    end

    methods % display
        % frame
        
        function disp(Ind, AI)
            %
           
            if isnumeric(Ind)
                
            end
            
        end
        
        % 
    end

    methods  % switch images in frame
        function AI = getImage(Obj, Ind)
            % Get image by index from Images property
            % Input  : - self.
            %          - Index of image in the AstroImage|FileNames|cell
            % Output : - A single element AstroImage object.
            
            switch class(Obj.Images)
                case 'AstroImage'
                    AI = Obj.Images(Ind);
                case 'FileNames'
                    FN = reorderEntries(Obj.Images, Ind, 'CreateNewObj',true);
                    AI = AstroImages.readFromFileNamesObj(FN);
                case 'cell'
                    AI = AstroImages(Obj.Images{Ind});
                otherwise
                    error('Unknown Images class');
            end
            
        end
        
        function Obj=sortByJD(Obj)
            % Sor the Images property by images JD
            % Input  : - self.
            % Output : - self in which the Images are sorted by JD.
            % Author : Eran Ofek (May 2023)
            
            switch class(Obj.Images)
                case 'AstroImage'
                    JD = Obj.Images.julday;
                    [~,SI] = sort(JD(:));
                    Obj.Images = Obj.Images(SI);
                case 'FileNames'
                    Obj.Images.sortByJD;
                case 'cell'
                    error('Can not sort by time an Images cell property of class cell');
            end
        end
        
        % goto: next | prev | first | last | ind
        
    end
    
    methods  % tools
        % [XY, RADec] = moments
        % getMask
        % getBack
        % getVar
        % plot
        % plotAll  % in all frames
        % nearest
        % near(Radius)
        % nearestAll
        % XY=getXY
        % RADec=getCoo
        % forcedPhot
    end



    methods (Static) % Unit-Test
        Result = unitTest()
            % unitTest for ds9

    end
    
    
end % end class
            
