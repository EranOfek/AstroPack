function [CCDSEC,UnCCDSEC,Center,Nxy,NewNoOverlap]=subimage_grid(SizeXY,Args)
% Partition image size into a grid of sub images
% Package: mUtil.image
% Description: Given the size of a two dimensional array (e.g., image), and
%              a sub image size or the number of partitions in each
%              dimension, calculate the coordinates of the partitions
%              boundries.
% Input  : - Image size [X, Y].
%          * Arbitrary number of pairs of input arguments ...,key,val,...
%            The following keywords are available:
%            'SubSizeXY' - Sub image size [X,Y]. Default is [128 128].
%            'Nxy' - Number of sub images along each dimension [Nx, Ny].
%                    If empty then use SubSizeXY. Default is [].
%            'OverlapXY' - Overlapping extra [X, Y] to add to SubSizeXY
%                    from each side. Default is [32 32].
%            'MakeEqualSize' - A logical indicating if the sub image sizes
%                   must be of equal size. Default is true.
%            'MakeSquare' - Make square sub grid. Default is true. 
% Output : - CCDSEC of the images with overlap [xmin, xmax, ymin, ymax].
%            A line per sub image.
%          - CCDSEC of the images without overlap.
%          - Two coloum vector of [X,Y] centers of the first output CCDSEC.
%          - Number of sub images in each dimension. [Nx, Ny].
%          - The CCDSEC of the non overlap region in the new image.
% Tested : Matlab R2011b
%     By : Eran O. Ofek                    Mar 2020
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [CCDSEC,unCCDSEC,Center,Nxy]=imUtil.cut.subimage_grid([256 258],'SubSizeXY',[64 64])
%          [CCDSEC,unCCDSEC,Center,Nxy, NonOverlap]=imUtil.cut.subimage_grid([256 258],'Nxy',[5 4])
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    SizeXY
    Args.SubSizeXY(1,2)                                  = [128 128];
    Args.Nxy                                             = [];
    Args.OverlapXY(1,2)                                  = [32  32];
    Args.MakeEqualSize logical                           = true;
    Args.MakeSquare logical                              = true;
end


if isempty(Args.Nxy)
    % use SubSizeXY
    Nxy = round(SizeXY./Args.SubSizeXY);
    SubSizeXY = SizeXY./Nxy;
else
    % use Nxy
    Nxy = Args.Nxy;
    SubSizeXY = SizeXY./Args.Nxy;
    
end

VecX   = (0:SubSizeXY(1):SizeXY(1));
VecXup = floor([VecX(2:end-1)+Args.OverlapXY(1), VecX(end)]);
VecXlow = floor([1, VecX(2:end-1)-Args.OverlapXY(1)]);

VecY   = (0:SubSizeXY(2):SizeXY(2));
VecYup = floor([VecY(2:end-1)+Args.OverlapXY(2), VecY(end)]);
VecYlow = floor([1, VecY(2:end-1)-Args.OverlapXY(2)]);

if all(VecXup==0)
    VecXup = SizeXY(1);
end

if all(VecYup==0)
    VecYup = SizeXY(2);
end


VecXEx = [0, floor(VecX(2:end))].';
VecYEx = [0, floor(VecY(2:end))].';

CX   = [VecXlow.', VecXup.'];
CY   = [VecYlow.', VecYup.'];
unCX = [VecXEx(1:end-1)+1, VecXEx(2:end)];
unCY = [VecYEx(1:end-1)+1, VecYEx(2:end)];
Nx = size(CX,1);
Ny = size(CY,1);
CCDSEC = zeros(Nx.*Ny,4);
UnCCDSEC = zeros(Nx.*Ny,4);

for Ix=1:1:Nx
    Ind = (Ny.*(Ix - 1)+1 : Ix.*Ny);
    CCDSEC(Ind,:)     = [CX(Ix,:).*ones(Ny,1), CY];
    UnCCDSEC(Ind,:)   = [unCX(Ix,:).*ones(Ny,1), unCY];
end

% for Iy=1:1:Ny
%     Ind = (Nx.*(Iy-1)+1:Iy.*Nx);
%     CCDSEC(Ind,:)   = [CX, CY(Iy,:).*ones(Nx,1)];
%     UnCCDSEC(Ind,:) = [CX, CY(Iy,:).*ones(Nx,1)];
%     
% end
Center = [mean(CCDSEC(:,1:2),2), mean(CCDSEC(:,3:4),2)];

DX = UnCCDSEC(:,1)-CCDSEC(:,1);
DY = UnCCDSEC(:,3)-CCDSEC(:,3);
WX = UnCCDSEC(:,2)-UnCCDSEC(:,1);
WY = UnCCDSEC(:,4)-UnCCDSEC(:,3);

NewNoOverlap = 1+[DX, WX-DX, DY, WY-DY];


if Args.MakeEqualSize
    MaxXY = max([CCDSEC(:,2)-CCDSEC(:,1), CCDSEC(:,4)-CCDSEC(:,3)]);
    if Args.MakeSquare
        MM = max(MaxXY);
        MaxXY = [MM MM];
    end
    AddXY = MaxXY - [CCDSEC(:,2)-CCDSEC(:,1), CCDSEC(:,4)-CCDSEC(:,3)];
    
    FlagEdgeX = CCDSEC(:,2)==SizeXY(1);
    FlagEdgeY = CCDSEC(:,4)==SizeXY(2);
    
    CCDSEC(~FlagEdgeX,2) = CCDSEC(~FlagEdgeX,2) + AddXY(~FlagEdgeX,1);
    CCDSEC(FlagEdgeX,1)  = CCDSEC(FlagEdgeX,1)  - AddXY(FlagEdgeX,1);
    
    CCDSEC(~FlagEdgeY,4) = CCDSEC(~FlagEdgeY,4) + AddXY(~FlagEdgeY,2);
    CCDSEC(FlagEdgeY,3)  = CCDSEC(FlagEdgeY,3)  - AddXY(FlagEdgeY,2);
    
end