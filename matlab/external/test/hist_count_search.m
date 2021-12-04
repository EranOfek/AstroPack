function bin_count=hist_count_search(data,edges)
%bin_search_hist - a histogram algorithm based on binary search of counts
% for each edge in the edge vector this code performs a binary search of
% the ordered data to find the count index of this edge
% !!!!!!!!!!!!!!! REQUIRES ORDERED DATA !!!!!!!!!!!!!!!!!!!!!!!!
% gives asymptotic speedup O(m·log(n)) over convertional histograming O(n·m) for dense histograms
% (many more counts than bins)
% Optimizations
%   - pre search for first(last) edge. Two inital searches for the hist limits eliminates
%     counts from search if they are not in the hisogram.
%   - moving search domain. Uses the previous edge as a lower limit to the
%     search domain.
%   - Simple sparse optimization. checks if there is any counts in the hist
%     bin before conducting search  (costs one compare but saves log(n)·unfilled bins)
%
% Syntax:         bin_counts=count_search_hist(data,edges)
% Equivelent to:  bin_counts=histcounts(data,[-inf;edges;inf])
% Designed to replicate histcounts(X,edges) "The value X(i)
%is in the kth bin if edges(k) ? X(i) < edges(k+1)" 
% Inputs:
%    data            - column vector of data/counts , MUST BE ORDERED!
%    edges           - column vector of bin edges, MUST BE ORDERED!

%
% Outputs:
%    bin_count - column vector, with length numel(edges)+1,  the first(last) element 
%                are the number of counts below(above) the first(last) edge
%
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
%  - try forward prediction for count search.
%    - based on the count in the previous bin estimate a better place to start the binary search.
%    - improvements of log(n)/log(2*n/m) , ~2.6 for n=1e6 m=1e4
%    - worst case log(n)+1
%    - works best for dense histogram
%
% Author: Bryce Henson
% email: Bryce.Henson@live.com
% Last revision:2019-05-13

%------------- BEGIN CODE --------------



%[~,order] =sort(X(:,1),1);
%X=X(order,:);
%X=sortrows(X,1);

if ~iscolumn(data) || ~iscolumn(edges)
    error('inputs must be column vectors')
end

num_edges=size(edges,1);
num_bins=num_edges-1 +2;
num_counts=size(data,1);
%initalize output
bin_count=zeros(num_bins,1);


%find the lowest edge
edge_lowest=edges(1);
idx_lowest=binary_search_first_elm(data,edge_lowest,1,num_counts);
val_lowest=data(idx_lowest);
if val_lowest<edge_lowest
    idx_lowest=idx_lowest+1;
end
%idx_lowest is now the first count in the first bin

% handle the case where the first edge is above the highest count
if idx_lowest==num_counts
    idx_lowest=idx_lowest+1;
end




%fprintf('lowest edge data idx %d\n',idx_lowest)
idx_u=idx_lowest;
idx_l=idx_lowest;
val_u=val_lowest; %for sparse opt
%the number of counts below the first edge is then 
bin_count(1)=idx_lowest-1; 
%check that there are still counts
rem_counts=num_counts-bin_count(1);

%do a binary search for the count index of the last edge to speed up the main loop (set search lims)
if rem_counts~=0
    edge_highest=edges(end);
    idx_max=binary_search_first_elm(data,edge_highest,idx_lowest,num_counts);
    
    %handle the case where the highest edge is below the first count
    if idx_max~=1
        idx_max=idx_max+1;
    end
    val_highest=data(idx_max);
    if val_highest<edge_highest
        idx_max=idx_max+1;
    end
    %idx_max is now the first count after the last bin
    %fprintf('hihgest edge data idx %d\n',idx_max)
    bin_count(end)=num_counts-idx_max+1;
    rem_counts=rem_counts-bin_count(1);
end

if rem_counts~=0
    ii=2;
    while idx_u<idx_max
        %fprintf('edge %d\n',ii)
        upper_bin_edge=edges(ii);
        %sparse opt, if the upper edge is smaller than the first count after the last bin
        if upper_bin_edge>val_u
            %fprintf('upper edge value %f\n',upper_bin_edge)
            %fprintf('upper edge count idx %d\n',idx_u)
            idx_u=binary_search_first_elm(data,upper_bin_edge,idx_u,idx_max);  
            val_u=data(idx_u);
            if val_u<upper_bin_edge 
                    idx_u=idx_u+1;
            end
            %idx_u is now the first count not in this bin
            %fprintf('upper edge count idx %d\n',idx_u)
            %fprintf('lower edge count idx %d\n',idx_l)
            if idx_u~=idx_l
                 bin_count(ii)=idx_u-idx_l;
            end
            idx_l=idx_u;
        end
        ii=ii+1;
    end
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