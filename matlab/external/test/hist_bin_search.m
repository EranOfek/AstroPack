function bin_count=hist_bin_search(data,edges)
%bin_search_hist - a histogram algorithm based on binary search of bins
% for each count in the data vector this code performs a binary search of
% the edges to find the apropriate histogram bin to increment 
% gives asymptotic speedup O(n·log(m)) over convertional hitograming O(n·m) for sparse histograms
% (many more bins than counts)
%
% Syntax:         bin_counts=bin_search_hist(data,edges)
% Equivelent to:  bin_counts=histcounts(data,[-inf;edges;inf])
% Designed to replicate histcounts(X,edges) "The value X(i)
%is in the kth bin if edges(k) ? X(i) < edges(k+1)" 
% Inputs:
%    data            - column vector of data/counts , no ordering requirement
%    edges           - column vector of bin edges, MUST BE ORDERED!
%
% Outputs:
%    bin_count - column vector, with length numel(edges)+1,  the first(last) element 
%                are the number of counts below(above) the first(last) edge
% Example: 
%     data=rand(1e5,1);
%     data=sort(data);
%     edges=linspace(0.1,1.1,1e6)';
%     out1=bin_search_hist(data,edges);
%     out2=histcounts(data,[-inf;edges;inf])';
%     isequal(out1,out2)
% Other m-files required: none
% Also See: scaling_tests,test_search_based_hist,adaptive_hist_method,compare_method_speeds
% Subfunctions: binary_search_first_elm
% MAT-files required: none
%
% Known BUGS/ Possible Improvements
%  - try basic search reduction ,
%    - compare count with last value to search only edges above or below that.
%    - might give ~5% improvement, got me thinking about pre search lookup
%    tables 
%
% Author: Bryce Henson
% email: Bryce.Henson@live.com
% Last revision:2019-05-13

%------------- BEGIN CODE --------------

%check if data is the right format
if ~iscolumn(data) || ~iscolumn(edges) 
    error('inputs must be column vectors')
end

% number of bins is edges-1 with 2 extra for below lowest and above highest
num_edges=size(edges,1);
num_bins=num_edges-1 +2;
bin_count=zeros(num_bins,1);
num_data=size(data,1);
for ii=1:num_data
    data_val=data(ii);
    closest_idx=binary_search_first_elm(edges,data_val,1,num_edges);
    closest_idx=closest_idx+1;
    %if closest is on the edge check if it should go up or down
     if closest_idx==2
         if data_val<edges(1)
             closest_idx=closest_idx-1;
         end
     elseif closest_idx==num_edges
         if data_val>edges(num_edges)
             closest_idx=closest_idx+1;
         end
     end
        
    bin_count(closest_idx)=bin_count(closest_idx)+1; 
end



end


%modified from mathworks submission by Benjamin Bernard 
%from https://au.mathworks.com/matlabcentral/fileexchange/37915-binary-search-for-closest-value-in-an-array
function idx_closest = binary_search_first_elm(vec, val,lower_idx,upper_idx)
% Returns index of vec that is closest to val, searching between min_idx start_idx . 
%If several entries
% are equally close, return the first. Works fine up to machine error (e.g.
% [v, i] = closest_value([4.8, 5], 4.9) will return [5, 2], since in float
% representation 4.9 is strictly closer to 5 than 4.8).
% ===============
% Parameter list:
% ===============
% arr : increasingly ordered array
% val : scalar in R
% use for debug in loop %fprintf('%i, %i, %i\n',btm,top,mid)

top = upper_idx(1);
btm = lower_idx(1);

% Binary search for index
while top > btm + 1
    mid = floor((top + btm)/2);
    % Replace >= here with > to obtain the last index instead of the first.
    if vec(mid) <= val %modified to work to suit histogram
        btm = mid;
    else
        top = mid;
    end
end

% Replace < here with <= to obtain the last index instead of the first.
%if top - btm == 1 && abs(arr(top) - val) < abs(arr(btm) - val)
%    btm = top;
%end  

idx_closest=btm;
end




% Copyright (c) 2012, Benjamin Bernard
% All rights reserved.
% 
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are
% met:
% 
%     * Redistributions of source code must retain the above copyright
%       notice, this list of conditions and the following disclaimer.
%     * Redistributions in binary form must reproduce the above copyright
%       notice, this list of conditions and the following disclaimer in
%       the documentation and/or other materials provided with the distribution
% 
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
