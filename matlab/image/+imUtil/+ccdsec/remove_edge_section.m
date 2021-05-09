function CCDSEC = remove_edge_section(SizeOriginalImageIJ, OverScan, Dir)
% Remove a CCDSEC region located at an edge of another CCDSEC
% Input  : - Image Size [I, J]
%          - CCDSEC to remove [xmin xmax ymin ymax]
%          - Direction (2, in x-axis).
% Output : - The new CCDSEC
% Author : Eran Ofek (May 2021)
% Example: CCDSEC = imUtil.ccdsec.remove_edge_section([100 200],[1 10 1 100],2)
%          CCDSEC = imUtil.ccdsec.remove_edge_section([100 200],[1 200 91 100],1)



% calculate CCDSEC (the non-overscan region)
switch Dir
    case 2
        if OverScan(1)==1
            % assume we have to remove the left-side
            % columns
            CCDSEC = [OverScan(2)+1, SizeOriginalImageIJ(2), 1, SizeOriginalImageIJ(1)];
        elseif OverScan(2)==SizeOriginalImageIJ(2)
            % assume we have to remove the right-side
            % columns
            CCDSEC = [1, OverScan(1)-1, 1, SizeOriginalImageIJ(1)];
        else
            error('Overscan region must be at the edge of the detector');
        end
    case 1
        if OverScan(3)==1
            % assume we have to remove the bottom rows
            CCDSEC = [1, SizeOriginalImageIJ(2), OverScan(4)+1, SizeOriginalImageIJ(1)];
        elseif OverScan(2)==SizeOriginalImageIJ(2)
            % assume we have to remove the upper rows
            CCDSEC = [1, SizeOriginalImageIJ(2), 1, OverScan(3)-1];
        else
            error('Overscan region must be at the edge of the detector');
        end
    otherwise
        error('Unknown Dir=%f (must be 1 or 2)',Dir);
end