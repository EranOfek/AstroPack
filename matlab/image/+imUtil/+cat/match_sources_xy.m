function [ResM, MatchedCat, Ref]=match_sources_xy(Cat, Ref, Args)
% Match sources between Cat and Ref catalogs using their X/Y (planar) coordinates.
% Package: +imUtil.cat
% Description: Matched the sources in Cat to sources in Ref by planar X/Y
%              coordinates. 
% Input  : - Catalog. A matrix in which two of the columns contains X/Y.
%          - Referece catalog. A matrix in which two of the columns contains X/Y.
%          * Pairs of ...,key,val,... Possible keywords include:
%            'SearchRadius' - Search radius. Default is 3.
%            'IsCatSorted' - Is Cat sorted by Y column. Default is false.
%            'ColCatX' - Column of X coordinate in Cat. Default is 1.
%            'ColCatY' - Column of Y coordinate in Cat. Default is 2.
%            'ColRefX' - Column of X coordinate in Ref. Default is 1.
%            'ColRefY' - Column of Y coordinate in Ref. Default is 2.
% Output : - A structure containing the following fields:
%            .Ind - Ind as returned by VO.search.search_sortedY_multi
%            .MatchedInd - MatchedInd as returned by VO.search.search_sortedY_multi
%          - Matched catalog. Each source is matched to a source in Ref
%            (line by line). NaN if source doesnt exist in Ref.
%          - Matched Ref. (the input Ref catalog).
%     By : Eran Ofek                     Aug 2020
% Example: Ref=rand(2000,2).*1024;
%          Cat=Ref(1:1000,:) + randn(1000,2).*0.5; Cat=[Cat;rand(500,2).*1024];
%          [ResM,MatchedCat,Ref] = imUtil.cat.match_sources_xy(Cat,Ref)

arguments
    Cat
    Ref
    Args.SearchRadius              = 3;
    Args.IsCatSorted(1,1) logical  = false;
    Args.CatColX(1,1)              = 1;
    Args.CatColY(1,1)              = 2;
    Args.RefColX(1,1)              = 1;
    Args.RefColY(1,1)              = 2;
end


% sort Cat
if ~Args.IsCatSorted
    Cat = sortrows(Cat, Args.CatColY);
end

% select coo columns from Cat
Cat = Cat(:,[Args.CatColX, Args.CatColY]);

[ResM.Ind,~,ResM.MatchedInd] = VO.search.search_sortedY_multi(Cat,Ref(:,Args.RefColX),Ref(:,Args.RefColY),Args.SearchRadius);
% NatchedInd contains: [Index in Cat, Dist between nearest match, DeltaX, DeltaY].

% matched statistics
ResM.MeanDX = tools.math.stat.nanmean(ResM.MatchedInd(:,3));
ResM.MeanDY = tools.math.stat.nanmean(ResM.MatchedInd(:,4));
ResM.StdDX  = tools.math.stat.nanstd(ResM.MatchedInd(:,3));
ResM.StdDY  = tools.math.stat.nanstd(ResM.MatchedInd(:,4));
ResM.StdD   = sqrt(tools.math.stat.nanstd(ResM.MatchedInd(:,3).^2 + ResM.MatchedInd(:,4).^2));
ResM.Nmatch = sum(~isnan(ResM.MatchedInd(:,1)));

if nargout>1
    % need to deal with NaNs
    % Example:  ResM.MatchedInd(3,1) - contains the index in Cat of the 3rd object in Ref 
   
    Nsrc       = size(Ref,1);
    MatchedCat = nan(Nsrc,size(Cat,2));
    Flag = ~isnan(ResM.MatchedInd(:,1));
    MatchedCat(Flag,:) = Cat(ResM.MatchedInd(Flag,1),:);
    
end