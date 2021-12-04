%test the hist_count_search and hist_bin_search functions

%add all subfolders to the path
this_folder = fileparts(which(mfilename));
% Add that folder plus all subfolders to the path.
addpath(genpath(this_folder));


%% bin search basic test
% to give the same output for histcounts we need to add inf to both ends of the edges
edges=linspace(1,10,10)';
data=[1.0]';
out1=hist_bin_search(data,edges)';
out2=histcounts(data,[-inf;edges;inf]);
% fprintf('edges   %s\n',sprintf('%.1f  ',edges))
% fprintf('out1       %s\n',sprintf('%d    ',out1))
% fprintf('out2       %s\n',sprintf('%d    ',out2))
logic_str={'FAIL','pass'};
fprintf('equality testing : %s\n',logic_str{isequal(out1,out2)+1})


%% count search basic

edges=linspace(1,10,10)';
%data=[linspace(2.2,2.8,5),linspace(1.1,1.2,3)]';
data=[3.9,6,7]';
data=sort(data);
out1=hist_count_search(data,edges);
out2=histcounts(data,[-inf;edges;inf])';
% fprintf('edges    %s\n',sprintf('%.1f   ',edges))
% fprintf('out1  %s\n',sprintf('%02d    ',out1'))
% fprintf('out2  %s\n',sprintf('%02d    ',out2))
logic_str={'FAIL','pass'};
fprintf('equality testing : %s\n',logic_str{isequal(out1,out2)+1})

%% test adaptive method

edges=linspace(1,10,10)';
%data=[linspace(2.2,2.8,5),linspace(1.1,1.2,3)]';
data=[3.9,6,7]';
data=sort(data);
out1=hist_adaptive_method(data,edges,1);
out2=histcounts(data,[-inf;edges;inf])';
% fprintf('edges    %s\n',sprintf('%.1f   ',edges))
% fprintf('out1  %s\n',sprintf('%02d    ',out1'))
% fprintf('out2  %s\n',sprintf('%02d    ',out2))
logic_str={'FAIL','pass'};
fprintf('equality testing : %s\n',logic_str{isequal(out1,out2)+1})

%% test benchmarking function

data=rand(1e6,1);
edges=linspace(1,10,10)';
[best_method_str,meth_det]=hist_compare_methods(data,edges);

%% use the benchmarking function to test some edge cases

data=rand(1e4,1);
% edges end above counts
edges=linspace(0.5,10,10)';
hist_compare_methods(data,edges)

% edges start below counts

edges=linspace(-1,0.5,10)';
hist_compare_methods(data,edges)
%%
% no counts, edges start above
edges=linspace(2,10,10)';
hist_compare_methods(data,edges)
%%
% no counts, edges start below
edges=linspace(-10,-1,10)';
hist_compare_methods(data,edges)


%% randomized equality testing

yn_logic_str={'no','yes'}; %for non critical cases

iimax=100;
for ii=1:iimax
data=rand(ceil(1+rand(1)*1e6),1);
data=sort(data);
edges=linspace(0.1,1.1,ceil(1+rand(1)*1e6))';
fprintf('testing with random data of length %u and %u bins \n',numel(data),numel(edges))

tic
out1=hist_bin_search(data,edges);
time_bin_search=toc;
fprintf('time bin search hist = %.2fms\n',time_bin_search*1e3)

tic
out2=hist_count_search(data,edges);
time_count_search=toc;
fprintf('time count search hist = %.2fms\n',time_count_search*1e3)

tic
out3=histcounts(data,[-inf;edges;inf])';
time_inbuilt=toc;
fprintf('time  inbuilt        = %.2fms\n',time_inbuilt*1e3)

tic
out4=hist_adaptive_method(data,edges,1);
time_adaptive=toc;

fprintf('time  adaptive       = %.2fms\n',time_adaptive*1e3)
fprintf('rel. speed inbuilt/bin_search= %.1f \n',time_inbuilt/time_bin_search)
fprintf('rel. speed inbuilt /count_search= %.1f \n',time_inbuilt/time_count_search)
fprintf('rel. speed inbuilt /adaptive= %.1f \n',time_inbuilt/time_adaptive)
logic_str={'FAIL','pass'};
is_bin_search_right=isequal(out3,out1);
fprintf('equality testing matlab=bin_search   : %s\n',logic_str{is_bin_search_right+1})
is_count_search_right=isequal(out3,out2);
fprintf('equality testing matlab=count_search : %s\n',logic_str{is_count_search_right+1});
is_count_search_right=isequal(out3,out4);
fprintf('equality testing matlab=adaptive_meth: %s\n',logic_str{is_count_search_right+1});
if ~is_bin_search_right || ~is_count_search_right
    return
end
fprintf('Speedup test   bin_search            : %s \n',yn_logic_str{(time_bin_search<time_inbuilt)+1})
fprintf('Speedup test   count_search          : %s \n',yn_logic_str{(time_count_search<time_inbuilt)+1})
fprintf('Speedup test   adpative_meth         : %s \n',yn_logic_str{(time_adaptive<time_inbuilt)+1})
end

