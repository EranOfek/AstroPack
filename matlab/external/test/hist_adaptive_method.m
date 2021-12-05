function bin_count=hist_adaptive_method(x_dat,edges,is_x_sorted,out_of_bounds)
%adaptive_hist_method - a function that tries to chose the fastest histogram method
% uses a very simple case strucute. Future implmentations may use something like a SVM if it can be made fast
% enough. see scaling_tests to make a plot of the relative method speed and adjust the thresholds for your own
% computer.
%
% Syntax:         bin_counts=adaptive_hist_method(sorted_data,edges,1)
% Equivelent to:  bin_counts=histcounts(sorted_data,[-inf;edges;inf])
% Inputs:
%    x_dat           - column vector of data/counts, 
%    edges           - column vector of bin edges, MUST BE ORDERED!
%    is_x_sorted     - is the x_dat sorted, if you use issorted(x_dat) you will lose a lot of the potential
%                      speedup that these algos can give
%    out_of_bounds   - inclue the out of bounds bins [-inf,edge1...,edge(end),inf]

% Outputs:
%    bin_count - column vector, if out_of_bounds=true will have length numel(edges)+1,  the first(last) element 
%                are the number of counts below(above) the first(last) edge
%                if out_of_bounds=false then will have ength numel(edges)-1
%
% Example: 
%     data=rand(1e5,1);
%     data=sort(data);
%     edges=linspace(0.1,1.1,1e6)';
%     out1=adaptive_hist_method(data,edges);
%     out2=histcounts(data,[-inf;edges;inf])';
%     isequal(out1,out2)
%
% Other m-files required: none
% Also See: scaling_tests,test_search_based_hist,adaptive_hist_method,compare_method_speeds
% Subfunctions: binary_search_first_elm
% MAT-files required: none
%
% Known BUGS/ Possible Improvements
%  - try some kind of ML eg SVM to predict which method to use.
%    - problem is the single inference of the svm can be up to 70ms which is a lot of overhead.
%  - a more complex (manual) inference procedure.
%
% Author: Bryce Henson
% email: Bryce.Henson@live.com
% Last revision:2019-05-13

%------------- BEGIN CODE --------------

dat_size=numel(x_dat);
edges_size=numel(edges);
if nargin<3
    is_x_sorted=0;
end
if nargin<4
    out_of_bounds=0;
end

if is_x_sorted
    if edges_size<2e3
        bin_count=hist_count_search(x_dat,edges);
    else
        if dat_size>1e3 && log10(dat_size)>log10(edges_size)-1
            %call histcounts with edges
            bin_count=histcounts(x_dat,[-inf;edges;inf])';
        else
            bin_count=hist_bin_search(x_dat,edges);
        end
    end
else
    if dat_size>1e3 && log10(dat_size)>log10(edges_size)-1
        %call histcounts with edges
        bin_count=histcounts(x_dat,[-inf;edges;inf])';
    else
         bin_count=hist_bin_search(x_dat,edges);
    end
end

% remove the edge bins
if ~out_of_bounds
    bin_count=bin_count(2:end-1);
end


end