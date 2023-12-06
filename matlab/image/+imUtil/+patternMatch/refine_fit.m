function [Param,Res,MatchedCat,MatchedRef]=refine_fit(Cat,Ref,varargin)
% Match sources in two catalogs and fit a positional transformation
% Package: imUtil.patternMatch
% Description: Given two catalogs of sources that have rougghly the same
%              coordinate system (e.g., images are aligned to a few pixels
%              lebvel), match the sources, and then fit a transformation
%              between the Reference coordinate system to the Catalog
%              system.
%              This function calls: imUtil.cat.match_sources and
%              imUtil.cat.fit_astrometric_tran
% Input  : - A catalog.
%          - A reference catalog.
%          * Pairs of ...,key,val,... Possible keywords include:
%            'SearchRadius' - Search radius for matching.
%                   Default is 3.
%            'IsSpherical' - Indicating if catalogs are in sphereical
%                   coordinates [Long,Lat] (true) or planner (false).
%                   Default is false.
%            'ErrPos' - A vector of positional errors in each axis for
%                   the sources. Default is 1e-5.
%            'MatchPar' - A cell array of additional parameters to pass to
%                   imUtil.cat.match_sources.
%                   Default is {}.
%            'FitPar' - A cell array of additional parameters to pass to
%                   imUtil.cat.fit_astrometric_tran
%                   Default is {}.
%            'CatColX' - Column for X/Long coordinates in Cat.
%                   Default is 1.
%            'CatColY' - Column for Y/Lat coordinates in Cat.
%                   Default is 2.
%            'RefColX' - Column for X/Long coordinates in Ref.
%                   Default is 1.
%            'RefColY' - Column for Y/Lat coordinates in Ref.
%                   Default is 2.
% Output : - A structure with the transformation parameters
%            See imUtil.patternMatch.fit_astrometric_tran for details.
%          - A structure with the best fit residuals information.
%            See imUtil.patternMatch.fit_astrometric_tran for details.
%          - The matched catalog.
%          - The matched Ref.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [Param,Res,MatchedCat,MatchedRef]=imUtil.patternMatch.refine_fit(Cat,Ref)
% Reliable: 2


InPar = inputParser;
addOptional(InPar,'IsSpherical',false);  % if true then coordinates are in radians
addOptional(InPar,'SearchRadius',3);
addOptional(InPar,'ErrPos',1e-5);

addOptional(InPar,'MatchPar',{});
addOptional(InPar,'FitPar',{});
addOptional(InPar,'ColCatX',1);
addOptional(InPar,'ColCatY',2);
addOptional(InPar,'ColRefX',1);
addOptional(InPar,'ColRefY',2);
addOptional(InPar,'ColRefM',3);

parse(InPar,varargin{:});
InPar = InPar.Results;


if nargin==0
    % simulation mode
    PWD = pwd;
    cd ~/matlab/images
    IC = imCl.fits2imCl('PTF*.fits'); 
    IC = IC(1);
    cd(PWD)
    C  = imUtil.sources.find_measure_sources(IC.Im);
    
    Mag = abs(27 - 2.5.*log10(C.Cat(:,18)));
    Cat = [C.Cat(:,12:13),Mag];
    Ref = Cat + [1 1 0];
    
end


% match sources
Ref = sortrows(Ref,InPar.ColRefY);
[MatchedCat,MatchedRef,ResM]=imUtil.cat.match_sources(Cat,Ref,InPar.MatchPar{:},...
                                                             'IsSpherical',InPar.IsSpherical,...
                                                             'SearchRadius',InPar.SearchRadius,...
                                                             'ColCatX',InPar.ColCatX,...
                                                             'ColCatY',InPar.ColCatY,...
                                                             'ColRefX',InPar.ColRefX,...
                                                             'ColRefY',InPar.ColRefY);
% Fot transformation
[Param,Res,ResLoop]=imUtil.patternMatch.fit_astrometric_tran(MatchedCat,MatchedRef,'ColRefC',[],...
                                                                    'ColRefM',InPar.ColRefM,...
                                                                    InPar.FitPar{:},...
                                                                    'ErrPos',InPar.ErrPos,...
                                                                    'ColCatX',InPar.ColCatX,...
                                                                    'ColCatY',InPar.ColCatY,...
                                                                    'ColRefX',InPar.ColRefX,...
                                                                    'ColRefY',InPar.ColRefY);
                                                         
                                                         


