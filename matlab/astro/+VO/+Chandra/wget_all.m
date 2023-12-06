function wget_all(Istart,Iend, Args)
% wget all Chandra observations in cats.X.ChandraObs
% Package: VO
% Description: 
% Input  : - 
%          * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
% Output : - 
% License: GNU general public license version 3
%     By : Eran O. Ofek                    Feb 2020
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: VO.Chandra.wget_all
% Reliable: 
%--------------------------------------------------------------------------

arguments
    Istart   = [];
    Iend     = [];
    Args.wget_obsidArgs cell = {};
end


Cat = cats.X.ChandraObs;
Nid = numel(Cat.Cat.ObsID);

if isempty(Istart)
    Istart = 1;
end
if isempty(Iend)
    Iend = Nid;
end

% 4287
for Iid=Istart:1:Iend
    ObsID = Cat.Cat.ObsID(Iid);
    
    fprintf('---------------------------------\n');
    fprintf('wget ObsID=%d  (dir %d out of %d)\n',ObsID,Iid,Nid);
    fprintf('---------------------------------\n');
    
    VO.Chandra.wget_obsid(ObsID, Args.wget_obsidArgs{:});
    
    pause(1);
    
end
