function FlagCR=find_crHT(Cat,ColH0,ColH1,Threshold)
% Find cosmic rays in a catalog using hypothesis testing
% Package: +imUtil.sources
% Description:
% Input  : - A catalog. A matrix or a single element catCl object.
%          - Column index or column name in which the H0 hypothesis is
%            stored. For CR detection this is the S/N for a delta function.
%          - Column index or column name in which the H1 hypothesis is
%            stored. For CR detection this is the S/N for a PSF.
%          - Threshold for CR detection ((H0-H1)>Threshold). Default is 0.
% Output :
% Example: FlagCR=imUtil.sources.find_crHT(Cat,ColH0,ColH1,Threshold)

if nargin<4
    Threshold = 0;
end

if catCl.iscatCl(Cat)
    ColH0 = colname2ind(Cat,ColH0);
    ColH1 = colname2ind(Cat,ColH1);
    Cat   = Cat.Cat;
elseif istable(Cat)
    error('table is not treated yet');
else
    % do nothing
end
    
FlagCR = (Cat(:,ColH0) - Cat(:,ColH1)) > Threshold;



