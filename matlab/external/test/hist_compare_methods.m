function [best_method_str,details,best_method_num]=hist_compare_methods(x_dat,edges_in)
% calculate the runtime of various histogram methods
% also check that they return the same answers
% TODO
% -docs
% -issorted flag

meth_times=[];
aux_times=[];
aux_times.sort=0;
out=[];

if ~issorted(x_dat)
    timer_handle=tic;
    x_dat=sort(x_dat);
    aux_times.sort=toc(timer_handle);
end


if ~issorted(edges_in)
    error('edges are not sorted') ;
end

edges_diff=diff(edges_in);
is_unform_bins=numel(uniquetol(edges_diff,eps*5))==1;

bin_lims=edges_in([1,end]);
bin_lims=sort(bin_lims);
bins=numel(edges_in)-1;

if is_unform_bins
    %fprintf('uniform edges detected\n')
    %generate the edges vector
    timer_handle=tic;
    edges=linspace(bin_lims(1),bin_lims(2),bins+1)';
    aux_times.gen_edges=toc(timer_handle);
    if ~isequal(edges_in,edges)
        error('generated edge vecor not the same as input')
    end
else
    edges=edges_in;
    aux_times.gen_edges=0;
end

%call histcounts with edges
timer_handle=tic;
out.histcounts_edges=histcounts(x_dat,[-inf;edges;inf])';
meth_times.histcounts_edges=toc(timer_handle);

%call histcounts with nbins and limits
timer_handle=tic;
out.histcounts_nbins=histcounts(x_dat,bins,'BinLimits',bin_lims)';
meth_times.histcounts_nbins=toc(timer_handle);

%because there is not the below min and above max bin comparison must be simpler
if ~isequal(out.histcounts_edges(2:end-1),out.histcounts_nbins)
    error('histcounts_edges and histcounts_nbins did not return the same output')
end

timer_handle=tic;
out.hist_bin_search=hist_bin_search(x_dat,edges);
meth_times.hist_bin_search=toc(timer_handle);

if ~isequal(out.histcounts_edges,out.hist_bin_search)
    error('histcounts_edges and hist_bin_search did not return the same output')
end

timer_handle=tic;
out.hist_count_search=hist_count_search(x_dat,edges);
meth_times.hist_count_search=toc(timer_handle);

if ~isequal(out.histcounts_edges,out.hist_count_search)
    error('histcounts_edges and hist_count_search did not return the same output')
end

timer_handle=tic;
out.adaptive=hist_adaptive_method(x_dat,edges,1,1);
meth_times.adaptive=toc(timer_handle);

if ~isequal(out.histcounts_edges,out.adaptive)
    error('histcounts_edges and adaptive did not return the same output')
end



% to be perfectly fair the methods that used the edge vector must have the edge vector generation time added
meth_times.histcounts_edges=aux_times.gen_edges+meth_times.histcounts_edges;
meth_times.hist_bin_search=aux_times.gen_edges+meth_times.hist_bin_search;
meth_times.hist_count_search=aux_times.gen_edges+meth_times.hist_count_search;
meth_times.adaptive=aux_times.gen_edges+meth_times.adaptive; %TODO: deal better with the adaptive is_sorted case


meth_times.hist_count_search=meth_times.hist_count_search+aux_times.sort;

feild_names=fields(meth_times);
[~,min_idx]=min(cell2mat(struct2cell(meth_times)));
best_method_str=feild_names{min_idx};
best_method_num=min_idx;

details=[];
details.core_times=meth_times;
details.aux_times=aux_times;
details.out=out;




end

