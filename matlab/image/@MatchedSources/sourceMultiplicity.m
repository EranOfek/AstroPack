function [Multiplicity,NOrphans,OrphanFlag]=sourceMultiplicity(MS)
% check how many times sources appear in a MatchedSources object in one or 
%  more epochs
%
% Input: a MatchedSources object containing sources in N epochs
%
% Output: 
%     - Multiplicity, the number of sources which appear in 0, 1 ... N epochs.
%         The first element should be 0, otherwise it may mean that the
%         MatchedSources object inconsistently contains rows of all NaNs
%
%     - NOrphans, the number of sources appearing only in the i-th epoch
%       (vector of length N)
%
%     - OrphanFlag, logical matrix of the same size as MS.Data.JD, true 
%                   for sources appearing only at a single epoch
%
% It is tacitly assumed that all arrays in MS.Data are consistent, i.e.
%  that each row of the arrays in MS.Data stands for sources in the same
%  epoch, and that it is sufficient to inspect only MS.Data.JD to declare 
%  a source as undetected
% Author: Enrico Segre

nplanes=size(MS.Data.JD,1);

Multiplicity=histcounts(sum(~isnan(MS.Data.JD)),0:nplanes+1);


if nargout>1
    NOrphans=zeros(1,nplanes);
    OrphanFlag=false(size(MS.Data.JD));
    for i=1:nplanes
        OrphanFlag(i,:)=~isnan(MS.Data.JD(i,:)) & ...
                         all(isnan(MS.Data.JD([1:i-1,i+1:end],:)),1);
        NOrphans(i)=sum(OrphanFlag(i,:));
    end
end