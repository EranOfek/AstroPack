function Result = unitTest()
    % unitTest for imUtil.frt package
    
   
    % create an image with two lines:
    Image = imUtil.frt.createLine([1700 1700],'A',[0.8;-0.3],'B',[500,200],'Fun','gauss', 'PixClass','double');

    % apply gaussian filter:
    FiltImage = imUtil.filter.filter2_fast(Image, imUtil.kernel2.gauss(2));
    
    % Calculate the FRT
    [R,P] = imUtil.frt.frt(FiltImage,'NumThreads',4);
    [Max, PeakI] = tools.math.stat.maxnd(R);
    [A,B,Theta,X0,L]=imUtil.frt.transformRadonCoo(PeakI(1), 1, PeakI(2), size(FiltImage), size(P{end}), numel(P));
    % A, B need to be similar to 0.8, 500
    
    
    
    % [FlatR] = imUtil.frt.frt(ones(size(Image)),'NumThreads',4);
    
    Result = true;
end