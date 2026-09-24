function [CornersRA, CornersDec] = getCooCorners(Obj, Args)
    % Get coordinates of corners of images stored in AstroImage object.
    % Input  : - An AstroImage object.
    %          * ...,key,val,... 
    %            'UseWCS' - A logical indicating if to use the WCS (true)
    %                   or to try and read the corner keywords from the
    %                   header (false). If truem then, will use the image
    %                   size along with xy2sky WCS method to evaluate the
    %                   corners RA/Dec coordinates.
    %                   Default is true.
    %            'KeyCornersRA' - A cell array of header keywords corners
    %                   for RA. Default is {'RA1','RA2','RA3','RA4'}.
    %            'KeyCornersDec' - A cell array of header keywords corners
    %                   for Dec. Default is {'DEC1','DEC2','DEC3','DEC4'}.
    % Output : - A matrix of size 4 X Nimages. Each column contains the
    %            four corners of the RA.
    %          - A matrix of size 4 X Nimages. Each column contains the
    %            four corners of the Dec.
    % Author : Eran Ofek (2025 Dec) 
    % Example: [CornersRA, CornersDec] = imProc.astrometry.getCooCorners(AllSI)

    arguments
        Obj
        Args.UseWCS            = true;
        Args.KeyCornersRA      = {'RA1','RA2','RA3','RA4'};
        Args.KeyCornersDec     = {'DEC1','DEC2','DEC3','DEC4'};
        
    end

    Nobj = numel(Obj);
    CornersRA  = nan(4, Nobj);
    CornersDec = nan(4, Nobj);

    KeyCorners = [Args.KeyCornersRA, Args.KeyCornersDec];

    for Iobj=1:1:Nobj
        if Args.UseWCS
            ImageSizeIJ = size(Obj(Iobj).ImageData.Image);
            PixCornersX = [1; ImageSizeIJ(2); ImageSizeIJ(2); 1];
            PixCornersY = [ImageSizeIJ(1); ImageSizeIJ(1); 1; 1];
            [CornersRA(:,Iobj), CornersDec(:,Iobj)] = Obj(Iobj).WCS.xy2sky(PixCornersX, PixCornersY);
        else
            % use header keywords containg corners
            % get cornersCoo from header
            CornersVal = Obj(Iobj).getStructKey(KeyCorners, 'UseDict',false);

            AllVal     = struct2array(CornersVal).';
            CornersRA(:,Iobj)  = AllVal(1:4);
            CornersDec(:,Iobj) = AllVal(5:8); 
        end
    end

end
