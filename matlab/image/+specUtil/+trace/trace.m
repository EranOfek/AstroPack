function trace
    %
    
   
    
    % Build trace kenel
    imUtil.kernel2.line
    
    % filter image with trace kernel
    imUtil.filter.filter2_sn
    
    % find local max in filtered image    
    specUtil.trace.peakDetectionFilter1
    
    % select ctraces - collapse? 
    
    % fit trace
    
    % extract the trace and background
    
end