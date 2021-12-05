%comparing scaling of matlabs inbuilt histogram to a sort based method

%add all subfolders to the path
this_folder = fileparts(which(mfilename));
% Add that folder plus all subfolders to the path.
addpath(genpath(this_folder));




%% ok so it looks like matlab does things a pretty brutish way and has O(n·m) scaling
% we can beat that !!
% there are two algorithms that can be employed
% 1. Bin search O( n log(m) )
%    - for each count do a binary search for the apropriate bin
% 2. Count search O( n·log(n)+m·log(n) ) 
%    - sort the data
%    - for each bin edge find the nearest data point, use the difference in
%    indicies to give the bin count
% bin search seems better in most cases as there is not the sort overhead




%%
evaluations=1e3;
update_interval=10; %seconds between plot updates
nmax=3e6;
mmax=3e6;

lin_eval=round(sqrt(evaluations));
evaluations=lin_eval.^2     ;
num_counts_vec=unique(round(logspace(1,round(log10(nmax)),lin_eval)));
num_edges_vec=unique(round(logspace(1,round(log10(mmax)),lin_eval)));
[num_counts_mesh,num_edges_mesh] = meshgrid(num_counts_vec,num_edges_vec);
num_counts_vec=num_counts_mesh(:);
num_edges_vec=num_edges_mesh(:);

%where to query
num_counts_query_vec=logspace(log10(min(num_counts_vec)),log10(max(num_counts_vec)),100);
num_edges_query_vec=logspace(log10(min(num_counts_vec)),log10(max(num_counts_vec)),100);
[num_counts_query,num_edges_query] = meshgrid(...
    num_counts_query_vec,...
    num_edges_query_vec);

rand_order=randperm(numel(num_edges_vec));
num_counts_vec=num_counts_vec(rand_order);
num_edges_vec=num_edges_vec(rand_order);

iimax=numel(num_counts_mesh);
%sort,inbuilt,bin_search,counts_search
runtimes=nan(iimax,4);

last_update=posixtime(datetime('now')); %time for updating plots every few seconds

sfigure(1);
clf
set(gcf,'color','w')
set(gcf, 'Units', 'pixels', 'Position', [100, 100, 1600, 900])

surface_colors= prism(6);

fprintf('  \n%04u:%04u',iimax,0) 
for ii=1:iimax
    fprintf('\b\b\b\b%04u',ii)
    data=rand(num_counts_vec(ii),1);
    
    [~,meth_det]=hist_compare_methods(data,[0,1],num_edges_vec(ii));
    
    runtimes(ii,1)=meth_det.aux_times.sort;
    runtimes(ii,2)=meth_det.core_times.histcounts_edges;
    runtimes(ii,3)=meth_det.core_times.histcounts_nbins;
    runtimes(ii,4)=meth_det.core_times.hist_bin_search;
    runtimes(ii,5)=meth_det.core_times.hist_count_search-meth_det.aux_times.sort;
    runtimes(ii,6)=meth_det.core_times.adaptive;
    

    ptime=posixtime(datetime('now'));
    if ptime-last_update>update_interval && ii>30 || ii==iimax
        %TODO: clean this up, so much repeated code
        %matlab inbuilt histcounts(edges)
        f_interp= scatteredInterpolant(num_counts_vec(1:ii),...
            num_edges_vec(1:ii),runtimes(1:ii,2));
        time_inbuilt_edges_interp =f_interp(num_counts_query,num_edges_query);
        %matlab inbuilt histcounts(nbins)
        f_interp= scatteredInterpolant(num_counts_vec(1:ii),...
            num_edges_vec(1:ii),runtimes(1:ii,3));
        time_inbuilt_nbins_interp =f_interp(num_counts_query,num_edges_query);
        %count search method with ordered data
        f_interp= scatteredInterpolant(num_counts_vec(1:ii),...
            num_edges_vec(1:ii),runtimes(1:ii,5));
        time_ocsearch_interp=f_interp(num_counts_query,num_edges_query);
        %count search method with unordered data
        f_interp= scatteredInterpolant(num_counts_vec(1:ii),...
            num_edges_vec(1:ii),runtimes(1:ii,5)+runtimes(1:ii,1));
        time_ucsearch_interp=f_interp(num_counts_query,num_edges_query);
        %bin search method
        f_interp = scatteredInterpolant(num_counts_vec(1:ii),...
            num_edges_vec(1:ii),runtimes(1:ii,4));
        time_bsearch_interp=f_interp(num_counts_query,num_edges_query);
        %adaptive method
         f_interp = scatteredInterpolant(num_counts_vec(1:ii),...
            num_edges_vec(1:ii),runtimes(1:ii,6));
        time_adaptive_interp=f_interp(num_counts_query,num_edges_query);
        
        surface_alpha=0.8;
        sfigure(1);
        surf(num_counts_query,num_edges_query,time_inbuilt_edges_interp,'FaceAlpha',surface_alpha,'FaceColor', surface_colors(1,:))
        hold on
        surf(num_counts_query,num_edges_query,time_inbuilt_nbins_interp,'FaceAlpha',surface_alpha,'FaceColor', surface_colors(2,:))
        surf(num_counts_query,num_edges_query,time_ocsearch_interp,'FaceAlpha',surface_alpha,'FaceColor', surface_colors(3,:))
        surf(num_counts_query,num_edges_query,time_ucsearch_interp,'FaceAlpha',surface_alpha,'FaceColor', surface_colors(4,:))
        surf(num_counts_query,num_edges_query,time_bsearch_interp,'FaceAlpha',surface_alpha,'FaceColor', surface_colors(5,:))
        surf(num_counts_query,num_edges_query,time_adaptive_interp,'FaceAlpha',surface_alpha,'FaceColor', surface_colors(6,:))
        hold off
        legend('inbuilt(edges)','inbuilt(nbins)','ordered count search','unordered count search','bin search','adaptive')
        set(gca,'XScale','log','YScale','log','ZScale','log')
        set(gca, 'Zdir', 'reverse')
        xlabel('num data n')
        ylabel('num bins m')
        zlabel('runtime (s)')
        view(135,25)
        pause(1e-6)
        last_update=ptime;
        fprintf('\n%04u',0) 
    end
end
figure(1)
fprintf('\n') 

saveas(gcf,fullfile('figs','scaling_comparison_raw.png'))

%% make classification data
%give each point (n,m) a value of 1,2,3
[~,type]=min([runtimes(:,2),runtimes(:,3),runtimes(:,4)],[],2);
class_data=[log10(num_edges_vec),log10(num_counts_vec),type];
%%
t = templateSVM('Standardize',1,'KernelFunction','gaussian');
%t = templateSVM('Standardize',1,'KernelFunction','polynomial','PolynomialOrder',2);
%t=templateDiscriminant('DiscrimType','quadratic');
classifier=fitcecoc(class_data(:,1:2),class_data(:,3),'Learners',t,'FitPosterior',1);
[pred score cost] = predict(classifier, class_data(:,1:2));
accuracy = sum(class_data(:,3) == pred)/size(pred,1);
predict(classifier,[3,3]);
predict(classifier,[3,3]);
tic; 
predict(classifier,[3,3]); 
model_runtime=toc;
fprintf('classifier accuracy %.3f, runtime %.1f ms\n',accuracy,model_runtime*1e3)


%%
%tic; trainedModel.predictFcn([5,5]); toc

%%
Smoothness = 0.0001;
%GridFit with bicubic interpolation. 
time_inbuilt_edges_interp = RegularizeData3D(...
                        log10(num_counts_vec),...
                        log10(num_edges_vec),...
                        log10(runtimes(1:ii,2)),...
                        log10(num_counts_query_vec),...
                        log10(num_edges_query_vec),...
                        'interp', 'bicubic', 'smoothness', Smoothness);
time_inbuilt_edges_interp=10.^(time_inbuilt_edges_interp);


time_inbuilt_bins_interp = RegularizeData3D(...
                        log10(num_counts_vec),...
                        log10(num_edges_vec),...
                        log10(runtimes(1:ii,3)),...
                        log10(num_counts_query_vec),...
                        log10(num_edges_query_vec),...
                        'interp', 'bicubic', 'smoothness', Smoothness);
time_inbuilt_bins_interp=10.^(time_inbuilt_bins_interp);

%search method with ordered data
time_ocsearch_interp = RegularizeData3D(...
                        log10(num_counts_vec),...
                        log10(num_edges_vec),...
                        log10(runtimes(1:ii,5)),...
                        log10(num_counts_query_vec),...
                        log10(num_edges_query_vec),...
                        'interp', 'bicubic', 'smoothness', Smoothness);
time_ocsearch_interp=10.^(time_ocsearch_interp);                    
%search method with unordered data
time_ucsearch_interp = RegularizeData3D(...
                        log10(num_counts_vec),...
                        log10(num_edges_vec),...
                        log10(runtimes(1:ii,5)+runtimes(1:ii,1)),...
                        log10(num_counts_query_vec),...
                        log10(num_edges_query_vec),...
                        'interp', 'bicubic', 'smoothness', Smoothness);
time_ucsearch_interp=10.^(time_ucsearch_interp);                    
time_bsearch_interp = RegularizeData3D(...
                        log10(num_counts_vec),...
                        log10(num_edges_vec),...
                        log10(runtimes(1:ii,4)),...
                        log10(num_counts_query_vec),...
                        log10(num_edges_query_vec),...
                        'interp', 'bicubic', 'smoothness', Smoothness);
time_bsearch_interp=10.^(time_bsearch_interp);                    
sfigure(3);
clf
set(gcf,'color','w')
set(gcf, 'Units', 'pixels', 'Position', [100, 100, 1600, 900])
hold on
alpha=1;
surf(num_counts_query,num_edges_query,time_inbuilt_edges_interp,'FaceAlpha',alpha,'FaceColor', surface_colors(1,:))

surf(num_counts_query,num_edges_query,time_inbuilt_bins_interp,'FaceAlpha',alpha,'FaceColor', surface_colors(2,:))


surf(num_counts_query,num_edges_query,time_ocsearch_interp,'FaceAlpha',alpha,'FaceColor', surface_colors(3,:))
surf(num_counts_query,num_edges_query,time_ucsearch_interp,'FaceAlpha',alpha,'FaceColor', surface_colors(4,:))
surf(num_counts_query,num_edges_query,time_bsearch_interp,'FaceAlpha',alpha,'FaceColor', surface_colors(5,:))
%scatter3(num_counts_vec,num_edges_vec,runtimes(:,2), 'xr');
%scatter3(num_counts_vec,num_edges_vec,runtimes(:,3), 'xg');
%scatter3(num_counts_vec,num_edges_vec,runtimes(:,3)+runtimes(:,1), 'xb');
%scatter3(num_counts_vec,num_edges_vec,runtimes(:,4)+runtimes(:,1), 'xm');
hold off
legend('inbuilt(edges)','inbuilt(nbins)','ordered count search','unordered count search','bin search')
set(gca,'XScale','log','YScale','log','ZScale','log')
set(gca, 'Zdir', 'reverse')
xlabel('num data n')
ylabel('num bins m')
zlabel('runtime (s)')
view(135,25)
pause(1e-6)

saveas(gcf,fullfile('figs','scaling_comparison_smooth.png'))

%% speedup plots
sfigure(4);
set(gcf,'color','w')
set(gcf, 'Units', 'pixels', 'Position', [100, 100, 1500, 500])
subplot(1,4,1)
was_one_of_mine_faster=min(cat(3,time_ocsearch_interp,time_bsearch_interp),[],3)<time_inbuilt_edges_interp; % time_inbuilt_bins_interp
imagesc(num_counts_query_vec,num_edges_query_vec-1,was_one_of_mine_faster)
title('any search method  better?')
colormap('gray')
set(gca,'YDir','normal')
ylabel('num bins m')
xlabel('num counts n')

subplot(1,4,2);
%was count search faster
was_ucsearch_faster=time_ucsearch_interp<time_inbuilt_edges_interp;
imagesc(num_counts_query_vec,num_edges_query_vec,was_ucsearch_faster)
title('sort+ordered count search better?')
colormap('gray')
set(gca,'YDir','normal')
xlabel('num counts n')

subplot(1,4,3);
%was count search faster
was_ocsearch_faster=time_ocsearch_interp<time_inbuilt_edges_interp;
imagesc(num_counts_query_vec,num_edges_query_vec,was_ocsearch_faster)
title('ordered count search better?')
colormap('gray')
set(gca,'YDir','normal')
xlabel('num counts n')

subplot(1,4,4);
was_bsearch_faster=time_bsearch_interp<time_inbuilt_edges_interp;
imagesc(num_counts_query_vec,num_edges_query_vec,was_bsearch_faster)
title('bin search better?')
colormap('gray')
set(gca,'YDir','normal')
xlabel('num counts n')
%%
sfigure(5);
min_seach_methods=min(cat(3,time_ocsearch_interp,time_ucsearch_interp,...
    time_bsearch_interp),[],3);
speedup_factor=time_inbuilt_edges_interp./min_seach_methods;
colormap(viridis())
surf(num_counts_query,num_edges_query,speedup_factor,'FaceAlpha',0.5) %,'FaceColor','r'

set(gca,'XScale','log','YScale','log','ZScale','log')
%set(gca, 'Zdir', 'reverse')
title('speedup relative to inbuilt')
xlabel('num data n')
ylabel('num bins m')
zlabel('speedup ')
pause(1e-6)


%%
hold on
%surf(,,'FaceAlpha',0.5,'FaceColor','r')
%surf(num_counts_query,num_edges_query,10.^(),'FaceAlpha',0.5,'FaceColor','g')
surf(num_counts_query,num_edges_query,10.^(time_ucsearch_interp),'FaceAlpha',0.5,'FaceColor','b')
surf(num_counts_query,num_edges_query,10.^(time_bsearch_interp),'FaceAlpha',0.5,'FaceColor','m')
scatter3(num_counts_vec,num_edges_vec,runtimes(1:ii,2), 'xr');
scatter3(num_counts_vec,num_edges_vec,runtimes(1:ii,3), 'xg');
scatter3(num_counts_vec,num_edges_vec,runtimes(1:ii,3)+runtimes(1:ii,1), 'xb');
scatter3(num_counts_vec,num_edges_vec,runtimes(1:ii,4)+runtimes(1:ii,1), 'xm');
hold off
legend('inbuilt','ordered count search','unordered count search','bin search')
set(gca,'XScale','log','YScale','log','ZScale','log')
set(gca, 'Zdir', 'reverse')
xlabel('num data n')
ylabel('num bins m')
zlabel('runtime (s)')
pause(1e-6)
last_update=ptime;


%interpunct · · alt+0183


%% investigate the scaling of the matlab inbuilt with the aomount of data

num_counts_vec=round(logspace(1,7,400));
edges=linspace(0,1,1e3);
bin_centers=(edges(2:end)+edges(1:end-1))*0.5;
iimax=numel(num_counts_vec);
runtime=nan(iimax,3);
last_update=posixtime(datetime('now')); %time for updating plots every few seconds

sfigure(1);
clf
set(gcf,'color','w')
set(gcf, 'Units', 'pixels', 'Position', [100, 100, 1600, 900])

fprintf('  \n%03u',0) 
for ii=1:iimax
fprintf('\b\b\b%03u',ii)  
data=rand(num_counts_vec(ii),1); %centered at zero, interval=2)
tic
bin_counts=histcounts(data,edges);
runtime(ii)=toc;
ptime=posixtime(datetime('now'));

if ptime-last_update>2 || ii==iimax
    sfigure(1);
    loglog(num_counts_vec,runtime,'k') 
    xlabel('num data (n)')
    ylabel('runtime (s)')
    title(sprintf('Scaling with 10^{%.1f} bins',log10(numel(edges)-1)))
    pause(1e-6)
    last_update=ptime;
end

end
figure(1)
fprintf('\n') 

saveas(gcf,fullfile('dev','matlab_inbuilt_ndata_scaling.png'))



%%

num_edges_vec=round(logspace(1,7,400));
data=rand(1e6,1); %centered at zero, interval=2)


iimax=numel(num_edges_vec);
runtime=nan(iimax,3);
last_update=posixtime(datetime('now')); %time for updating plots every few seconds

sfigure(1);
clf
set(gcf,'color','w')
set(gcf, 'Units', 'pixels', 'Position', [100, 100, 1600, 900])

fprintf('  \n%03u',0) 
for ii=1:iimax
fprintf('\b\b\b%03u',ii)  
edges=linspace(0,1,num_edges_vec(ii));

tic
bin_counts=histcounts(data,edges);
%bin_centers=(edges(2:end)+edges(1:end-1))*0.5; %no need to compute

runtime(ii)=toc;
ptime=posixtime(datetime('now'));

if ptime-last_update>2 || ii==iimax
    sfigure(1);
    loglog(num_edges_vec-1,runtime,'k') 
    xlabel('num bins (m)')
    ylabel('runtime (s)')
     title(sprintf('Scaling with 10^{%.1f} data pts',log10(numel(data))))
    pause(1e-6)
    last_update=ptime;
end

end
figure(1)
fprintf('\n') 

saveas(gcf,fullfile('dev','matlab_inbuilt_mbins_scaling.png'))

%%
sfigure(2)
plot(num_edges_vec,runtime) 