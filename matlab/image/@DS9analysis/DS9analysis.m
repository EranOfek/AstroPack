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
        Images                % AstroImage | FileNames | cell
        Frame2Ind    = [1];

        MapInd       = [];
    end

    
    %properties (Hidden)
    %end
    
    methods % Constructor method
        function Obj = DS9analysis(Image, DispInd)
            %

            arguments
                Image   = [];
                DispInd = 1;
            end

            if ~isempty(Image)
                Obj.Images = Image;


            end

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
        
        % function disp(Ind, AI)
        %     %
        % 
        %     if isnumeric(Ind)
        % 
        %     end
        % 
        % end
        
        % 
    end

    methods  % switch images in frame
        function AI = getImage(Obj, Ind)
            % Get image by index from Images property
            % Input  : - self.
            %          - Index of image in the AstroImage|FileNames|cell
            % Output : - A single element AstroImage object.
            
            if isempty(Obj.Images)
                error('Images is not populated');
            end

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
        % 
        % goto: next | prev | first | last | ind
        
    end
    
    methods  % basic utilities
        function [X, Y, Val, AI] = getXY(Obj, Coo, Args)
            % Get X/Y position for user clicked/specified position
            % Input  : - self.
            %          - If empty, then prompt the user to click the ds9
            %            window in a give position.
            %            Alterantively, a vector of [RA, Dec] in decimal or
            %            radians.
            %            Or, a cell of sexagesimal coordinates {RA, Dec}.
            %          * ...,key,val,...
            %            'CooSys' - Coordinate system of user specified
            %                   coordinates: 'sphere'|'pix'. Default is 'sphere.
            %            'CooUnits' - Coordinates units. Default is 'deg'.
            % Output : - X position.
            %          - Y position.
            %          - Image value at position.
            %          - AstroImage at current frame for which
            %            positions/values where obtained.
            % Author : Eran Ofek (May 2023)

            arguments
                Obj
                Coo    = [];
                Args.CooSys   = 'sphere';
                Args.CooUnits = 'deg';
            end

            Frame = str2double(ds9.frame);
            Ind   = Obj.MapInd(Frame);
            AI    = Obj.getImage(Ind);


            if isempty(Coo)
                [X, Y, PixVal] = ds9.getpos(1);
            else
                if iscell(Coo)
                    % assume Coo in sexagesimal coordinates
                    [X, Y] = AI.WCS.sky2xy(Coo{1}, Coo{2});
                else

                    switch lower(CooSys)
                        case 'sphere'
                            [X, Y] = AI.WCS.sky2xy(Coo(1), Coo(2), 'InUnits',Args.CooUnits);
                        case 'pix'
                            X = Coo(1);
                            Y = Coo(2);
                        otherwise
                            error('Unknown CooSys option');
                    end
                end
            end

            if nargout>2
                Xpix      = round(X);
                Ypix      = round(Y);
                Val       = AI.Image(Ypix, Xpix);
            end

        end

    end

    methods  % tools
        function [M1, M2, Aper, RADec, AI] = moments(Obj, Coo, Args)
            % Measure 1st, 2nd moments, and aper phot at user click or specified position.
            % Input  : - self.
            %          - If empty, then prompt the user to click the ds9
            %            window in a give position.
            %            Alterantively, a vector of [RA, Dec] in decimal or
            %            radians.
            %            Or, a cell of sexagesimal coordinates {RA, Dec}.
            %          * ...,key,val,...
            %            'CooSys' - Coordinate system of user specified
            %                   coordinates: 'sphere'|'pix'. Default is 'sphere.
            %            'CooUnits' - Coordinates units. Default is 'deg'.
            %
            %            'HalfSize' - Moments stamp half size.
            %                   Default is 7.
            %            'MaxIter' - Maximum number of moment estimation
            %                   iterations. Default is 10.
            %            'MaxStep' -  Maximum step size (pixels) in X and Y shifts
            %                   allowd in each iteration. Default is 0.1.
            %            'AperRadius' - Vector of aperture radii, in which to calculate
            %                   aperture photometry.
            %                   Default is [2 4 6].
            %            'Annulus' - Vector of inner and outer radius of background
            %                   annulus. Default is [10 14].
            %            'OutUnits' - Units of output RA, Dec position
            %                   based on 1st moments. Default is 'deg'.
            % Output : - A structure with 1st moment information:
            %            .RoundX - Vector of roundex X position
            %            .RoundY - Vector of roundex Y position
            %            .DeltaLastX - Vector of the X shifts in the last position
            %                   iteration.
            %            .DeltaLastY - Vector of the Y shifts in the last position
            %                   iteration.
            %            .Iter - Number of position iterations.
            %            .X    - 1st moment X position
            %            .Y    - 1st moment Y position.
            %            .Xstart - Starting X position,
            %            .Ystart - Starting Y position.
            %           - A second momement information.
            %             A structure with the following fields.
            %             .X2 - X^2 2nd moment.
            %             .Y2 - Y.^2 2nd moment.
            %             .XY - X*Y 2nd moment.
            %           - Photometry information. A structure with the following fields.
            %             .AperRadius - Vector of apertures radius.
            %             .AperPhot - Matrix of aperture photometry. Column per
            %                         aperture.
            %             .AperArea - Matrix of apertures area. Column per aperture.
            %             .BoxPhot  - Vector of the full box photometry (if requested)
            %             .AnnulusBack - Annulus background.
            %             .AnnulusStd - Annulus StD.
            %             .WeightedAper - Weighted photometry. Weighted by the user
            %                           specified weight function.
            %           - [RA, Dec] of of 1st moemnt position.
            %           - The AstroImage object from which the information
            %             was extracted.
            % Author : Eran Ofek (May 2023)
            
            arguments
                Obj
                Coo              = [];
                Args.CooSys      = 'sphere';
                Args.CooUnits    = 'deg';
                
                Args.HalfSize    = 7;
                Args.MaxIter     = 10;
                Args.MaxStep     = 0.1;
                Args.AperRadius  = [2 4 6];
                Args.Annulus     = [10 14];
                Args.OutUnits    = 'deg';
            end

            [X, Y, Val, AI] = getXY(Obj, Coo, 'CooSys',Args.CooSys, 'CooUnits',Args.CooUnits);
            
            
            [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(AI.Image, X, Y, Args.HalfSize);
            [M1, M2, Aper]               = imUtil.image.moment2(Cube, X, Y, 'SubBack',true,...
                                                                            'MaxIter',Args.MaxIter,...
                                                                            'MaxStep',Args.MaxStep,...
                                                                            'AperRadius',Args.AperRadius,...
                                                                            'Annulus',Args.Annulus);
            if nargout>3
                [RA, Dec] = AI.WCS.xy2sky(M1.X, M1.Y, 'OutUnits',Args.OutUnits);
                RADec = [RA, Dec];
            end
                
        end
        
        % forcedPhot
        
        function [MaskName, MaskVal]=getMask(Obj, Coo, Args)
            % Get Mask bit values/names at user clicked/specified position
            % Input  : - self.
            %          - If empty, then prompt the user to click the ds9
            %            window in a give position.
            %            Alterantively, a vector of [RA, Dec] in decimal or
            %            radians.
            %            Or, a cell of sexagesimal coordinates {RA, Dec}.
            %          * ...,key,val,...
            %            'CooSys' - Coordinate system of user specified
            %                   coordinates: 'sphere'|'pix'. Default is 'sphere.
            %            'CooUnits' - Coordinates units. Default is 'deg'.
            % Output : - A cell array of bit mask names at the specified
            %            position. If empty, then bit mask is 0.
            %          - Bit mask decimal value.
            % Author : Eran Ofek (May 2023)

            arguments
                Obj
                Coo    = [];
                Args.CooSys   = 'sphere';
                Args.CooUnits = 'deg';
            end

            [X, Y, Val, AI] = getXY(Obj, Coo, 'CooSys',Args.CooSys, 'CooUnits',Args.CooUnits);
            
            Xpix      = round(X);
            Ypix      = round(Y);
            if isempty(AI.Mask)
                error('No Mask image');
            end
            MaskVal   = AI.Mask(Ypix,Xpix);
            MaskName  = AI.MaskData.Dict.bitdec2name(MaskVal);
            MaskName  = MaskName{1};

        end

        function [Back, Var, X, Y, AI] = getBack(Obj, Coo, Args)
            % Get Back/Var values at user clicked/specified position
            % Input  : - self.
            %          - If empty, then prompt the user to click the ds9
            %            window in a give position.
            %            Alterantively, a vector of [RA, Dec] in decimal or
            %            radians.
            %            Or, a cell of sexagesimal coordinates {RA, Dec}.
            %          * ...,key,val,...
            %            'CooSys' - Coordinate system of user specified
            %                   coordinates: 'sphere'|'pix'. Default is 'sphere.
            %            'CooUnits' - Coordinates units. Default is 'deg'.
            % Output : - Background value.
            %          - Variance value.
            %          - Clicked X position.
            %          - Clicked Y position.
            %          - AstroImage on which the operation was performed.
            % Author : Eran Ofek (May 2023)
            
            arguments
                Obj
                Coo    = [];
                Args.CooSys   = 'sphere';
                Args.CooUnits = 'deg';
            end

            [X, Y, Val, AI] = getXY(Obj, Coo, 'CooSys',Args.CooSys, 'CooUnits',Args.CooUnits);
            
            Xpix      = round(X);
            Ypix      = round(Y);
            if isempty(AI.Back) || isempty(AI.Var)
                error('No Back or Var image');
            end
            Back = AI.Back(Ypix, Xpix);
            Var  = AI.Var(Ypix, Xpix);            
            
        end
            
        % plot
        % plotAll  % in all frames
        
        function [Result,Dist,CatInd]=near(Obj, Coo, Radius, Args)
            % Get sources in AstroImage catalog within search radius from clicked/specified position.
            % Input  : - self.
            %          - If empty, then prompt the user to click the ds9
            %            window in a give position.
            %            Alterantively, a vector of [RA, Dec] in decimal or
            %            radians.
            %            Or, a cell of sexagesimal coordinates {RA, Dec}.
            %          * ...,key,val,...
            %            'CooSys' - Coordinate system of user specified
            %                   coordinates: 'sphere'|'pix'. Default is 'sphere.
            %            'CooUnits' - Coordinates units. Default is 'deg'.
            %            'OutType' - Type of output catalog:
            %                   'table' | 'astrocatalog'.
            %                   Default is 'table'.
            % Output : - Catalog of of sources within search radius,
            %            obtained from the CatData property in the
            %            AstroImage object associated with the current
            %            image.
            %          - Vector of distances [pix] from the clicked search
            %            position, for each source in the output catalog.
            %          - Vector of indices of the returned sources in the
            %            original AstroCatalog in the AstroImage.
            % Author : Eran Ofek (May 2023)
            
            arguments
                Obj
                Coo    = [];
                Radius = 10;  % pix
                Args.CooSys   = 'sphere';
                Args.CooUnits = 'deg';
                Args.OutType  = 'table';  % 'table'|'astrocatalog'
            end
            
            [X, Y, Val, AI] = getXY(Obj, Coo, 'CooSys',Args.CooSys, 'CooUnits',Args.CooUnits);

            if AI.CatData.sizeCatalog==0
                error('Source catalog in AstroImage is empty');
            end

            CatXY = AI.CatData.getXY;

            Dist  = sqrt( (CatXY(:,1)-X).^2 + (CatXY(:,2)-Y).^2);

            Flag = Dist<Radius;
            CatInd = find(Flag);
            Dist   = Dist(CatInd);
            switch lower(Args.OutType)
                case 'table'
                    Result = AI.CatData.toTable;
                    Result = Result(CatInd,:);
                case 'astrocatalog'
                    Result = AI.CatData.copy;
                    Result.Catalog = Result.Catalog(CatInd,:);
                otherwise
                    error('Unknown OutType option');
            end


        end

        % nearestAll
        % RADec=getCoo
    end



    methods (Static) % Unit-Test
        Result = unitTest()
            % unitTest for ds9

    end
    
    
end % end class
            