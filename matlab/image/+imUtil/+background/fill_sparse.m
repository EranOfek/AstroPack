function Image=fill_sparse(GridVal,SizeXY,Method) 
% given a list of sparse 2D positions and values, fill an image by interp
% Package: @imUtil.background
% Input  : - A three column matrix of [X,Y,Val],
%            where X and Y are pixel poistion and Val is the value at that
%            position.
%          - The [X, Y] size of the image to fill.
%          - Interpolation method:
%            'si' - use scatteredInterpolant with linear interpolation and
%                   extrapolation.
%            'impaint' - use impaint (slow).
%            Default is 'si'.
% Output : - The filled image.
%      By: Eran O. Ofek                       Apr 2020             
% Example: GridVal = [10 10 1; 10 20 1.1;20 10 1.2; 20 20 1.3]; 
%          imUtil.background.fill_sparse(GridVal,[30 30])

arguments
    GridVal(:,3)
    SizeXY(1,2)
    Method       {mustBeMember(Method,{'si','impaint'})} = 'si';
end

switch lower(Method)
    case 'si'
        F = scatteredInterpolant(GridVal(:,1),GridVal(:,2),GridVal(:,3));
        Xq = (1:1:SizeXY(1));
        Yq = (1:1:SizeXY(2)).';
        [MatX,MatY] = meshgrid(Xq,Yq);
        Image = F(MatX,MatY);
    case 'impaint'
        Image = nan(SizeXY(2),SizeXY(1));
        Ind = imUtil.image.sub2ind_fast([SizeXY(2), SizeXY(1)],round(GridVal(:,2)), round(GridVal(:,1)));
        Image(Ind) = GridVal(:,3);
        Image = imUtil.trans.iminterp(Image);
    otherwise
        error('Unknown Method option');
end

