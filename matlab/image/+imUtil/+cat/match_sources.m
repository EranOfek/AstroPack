function [ResM,MatchedCat,MatchedRef]=match_sources(Cat,Ref,varargin)
% Match sources in two catalogs with the same coordinates systems.
% Package: imUtil.cat
% Description: Match a catalog and reference catalog which are on the
%              same 2-D coordinate system.
%              The catalogs may have spherical or planner coordinates.
% Input  : - A catalog. Coordinate units are defined in 'CooUnits'.
%          - A reference catalog. Unless 'IsSortedRef' is false, this
%            catalog must be sorted by latitude/Y coordinates.
%            Coordinate units are defined in 'CooUnits'.
%          * Pairs of ...,key,val,... Possible keywords include:
%            'CooUnits' - Units of coordinates (for the IsSpherical=true
%                   case).
%                   Default is 'rad'.
%            'SearchRadius' - Search radius for matching.
%                   Default is 3.
%            'SearchRadiusUnits' - In case the 'IsSpherical' argument is
%                   true, then this indicats the units for the SearchRadius
%                   argument.
%                   Default is 'arcsec'.
%            'IsSpherical' - Indicating if catalogs are in sphereical
%                   coordinates [Long,Lat] (true) or planner (false).
%                   Default is true.
%            'IsSortedRef' - Is the refrence catalog is sorted.
%                   Default is true.
%            'ColCatX' - Column for X/Long coordinates in Cat.
%                   Default is 1.
%            'ColCatY' - Column for Y/Lat coordinates in Cat.
%                   Default is 2.
%            'ColRefX' - Column for X/Long coordinates in Ref.
%                   Default is 1.
%            'ColRefY' - Column for Y/Lat coordinates in Ref.
%                   Default is 2.
%            'MatchOnly1' - If true then select only sources with a single
%                   match within search radius. Default is true.
% Output : - Matched catalog (line by line to MatchedRef).
%          - Matched reference catalog (MatchedRef).
%          - A structure with the following fields:
%            'Ind'      - A strucure array in which the number of elements equal to the
%                       number of coordinates in Cat, and with the following
%                       fields:
%                       'Ind' - Vector of indices of matched sources.
%                           'Ind' - index of source in Ref.
%                           'Nmatch' - number of matches.
%                       'Nmatch' - Number of matched sources.
%                       'Dist' - Distance between sources. This is provided only if
%                               the input search radius is negative.
%            'Nmatches' - vector of number of matches for each source in
%                       Cat to sources in Ref.
%            'DeltaX' - Cat-Ref X/Long difference (on great circle).
%            'DeltaY' - Cat-Ref Y/Lat diffrence.
%            'Dist'   - Cat/Ref distance.
%            'PA'     - Position angle.
%            'DeltaRMS' - rms of [DeltaX, DeltaY].
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Cat = rand(1000,2).*1000; Ref = Cat + randn(1000,2).*0.3;
%          Ref = sortrows(Ref,2);
%          [MatchedCat,MatchedRef,Res]=imUtil.cat.match_sources(CatRef = sortrows(Ref,2);,Ref,'IsSpherical',false)
% Reliable: 2


InPar = inputParser;
addOptional(InPar,'CooUnits','rad');
addOptional(InPar,'SearchRadius',3);
addOptional(InPar,'SearchRadiusUnits','arcsec');
addOptional(InPar,'IsSpherical',true);  
addOptional(InPar,'IsSortedRef',true);
addOptional(InPar,'ColCatX',1);
addOptional(InPar,'ColCatY',2);
addOptional(InPar,'ColRefX',1);
addOptional(InPar,'ColRefY',2);
addOptional(InPar,'MatchOnly1',true);
parse(InPar,varargin{:});
InPar = InPar.Results;

if ~InPar.IsSortedRef
    % sort Ref
    Ref = sortrows(Ref,InPar.ColRefY);
end
    
if InPar.IsSpherical
    ConvertFactor = convert.angular(InPar.CooUnits,'rad');
    Cat(:,InPar.ColCatX) = Cat(:,InPar.ColCatX) .* ConvertFactor;
    Cat(:,InPar.ColCatY) = Cat(:,InPar.ColCatY) .* ConvertFactor;
    Ref(:,InPar.ColRefX) = Ref(:,InPar.ColRefX) .* ConvertFactor;
    Ref(:,InPar.ColRefY) = Ref(:,InPar.ColRefY) .* ConvertFactor;
    
    
    % The minus search radius will return also the distance
    SearchRadius = convert.angular(InPar.SearchRadiusUnits,'rad',InPar.SearchRadius);   % rad
    [Ind,~,MatchedInd] = VO.search.search_sortedlat_multi(Ref(:,[InPar.ColRefX InPar.ColRefY]),Cat(:,InPar.ColCatX),Cat(:,InPar.ColCatY),-SearchRadius);
else
    [Ind,~,MatchedInd] = VO.search.search_sortedY_multi(Ref(:,[InPar.ColRefX InPar.ColRefY]),Cat(:,InPar.ColCatX),Cat(:,InPar.ColCatY),-InPar.SearchRadius);
end

if InPar.MatchOnly1
    % match only source which have a single match within the search radius
    % select sources in Cat which have a single match in Ref
    Flag = [Ind.Nmatch].'==1;
    MatchedRef = Ref([Ind(Flag).Ind],:);
    MatchedCat = Cat(Flag,:);
    Nmatches   = int32(Flag); %ones(size(Ref,1),1);
    %Dist       = [Ind(Flag).Dist].';
else
    % match also sources with multiple matches
    N = size(Cat,1);
    Flag  = [Ind.Nmatch]>0;
    IFlag = find(Flag);
    Nm    = numel(IFlag);
    
    MatchedCat = Cat(Flag,:);
    Nmatches   = [Ind(IFlag).Nmatch].';
    MatchedRef = nan(Nm,size(Ref,2));
    for Im=1:1:Nm
        if Ind(IFlag(Im)).Nmatch==1
            MatchedRef(Im,:) = Ref(Ind(IFlag(Im)).Ind,:);
        elseif Ind(IFlag(Im)).Nmatch>1
            % search nearest source
            MinI = min(Ind(IFlag(Im)).Dist);
            MatchedRef(Im,:) = Ref(Ind(IFlag(Im)).Ind(MinI),:);
        else
            % do nothing
        end
    end
            
end

ResM.Ind = Ind;
ResM.MatchedInd = MatchedInd;  % [Ind of Ref, Dist, DeltaX(Cat-Ref), DeltaY(Cat-Ref)]
ResM.Nmatches = Nmatches;

if nargout>1
    
    MatchedCat = Cat;
    MatchedRef = Ref(:,MatchedInd(:,1));
    
    
    spherical???????
    
    
    
    
    
    
    if size(MatchedCat,1)>0
        if InPar.IsSpherical
            [Res.DeltaX,Res.DeltaY,Res.Dist,Res.PA]=celestial.coo.sphere_offset(MatchedCat(:,InPar.ColCatX),...
                                                                                MatchedCat(:,InPar.ColCatY),...
                                                                                MatchedRef(:,InPar.ColRefX),...
                                                                                MatchedRef(:,InPar.ColRefY),'rad','rd');
            Res.DeltaRMS   = std([Res.DeltaX, Res.DeltaY],[],1);
        else
            Res.DeltaX = MatchedCat(:,InPar.ColCatX) - MatchedRef(:,InPar.ColRefX);
            Res.DeltaY = MatchedCat(:,InPar.ColCatY) - MatchedRef(:,InPar.ColRefY);
            Res.Dist  = sqrt(sum([Res.DeltaX, Res.DeltaY].^2,2));
            Res.PA    = atan2(Res.DeltaY,Res.DeltaX);
            Res.DeltaRMS   = std([Res.DeltaX, Res.DeltaY],[],1);
        end
    else
       Res.DeltaX = [];
       Res.DeltaY = [];
       Res.Dist   = [];
       Res.PA     = [];
       Res.DeltaRMS = [];
       
    end
end
    
if InPar.IsSpherical
    ConvertFactor = convert.angular('rad',InPar.CooUnits);
    MatchedCat(:,InPar.ColCatX) = MatchedCat(:,InPar.ColCatX) .* ConvertFactor;
    MatchedCat(:,InPar.ColCatY) = MatchedCat(:,InPar.ColCatY) .* ConvertFactor;
    MatchedRef(:,InPar.ColRefX) = MatchedRef(:,InPar.ColRefX) .* ConvertFactor;
    MatchedRef(:,InPar.ColRefY) = MatchedRef(:,InPar.ColRefY) .* ConvertFactor;
end
    
    

