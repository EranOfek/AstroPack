function Result = unitTest()
    % unitTest for imUtil.filter
    % Example: imUtil.filter.unitTest
    
    
    
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    
    %% Test: imUtil.filter.xcorr1_fft_multi
    Shift = 10;
    AS = AstroSpec.getSkyArcsSpecLines;
    R1=AS(1).Flux;
    R2=[zeros(Shift,1); R1];
    [Lag,XC,Info] = imUtil.filter.xcorr1_fft_multi(R1,R2+randn(size(R2)).*10);
    if Info.Shift~=-Shift
        error('Error in imUtil.filter.xcorr1_fft_multi');
    end
    
    %% imUtil.filter.xcorr1_scale_shift
    % only shift
    Shift = 20;
    AS = AstroSpec.getSkyArcsSpecLines;
    Template = AS(1).Flux;
    Data     = [zeros(Shift,1); Template];
    Data     = [(1:1:numel(Data)).', Data];
    Template = [(1:1:numel(Template)).', Template];
    Info     = imUtil.filter.xcorr1_scale_shift(Data,Template);
    
    % only scale
    Scale    = 1.1;
    Template = AS(1).Flux;
    Data     = Template;
    Data     = [(1:Scale:numel(Data).*Scale).', Data];
    Template = [(1:1:numel(Template)).', Template];
    Info     = imUtil.filter.xcorr1_scale_shift(Data,Template);
    
    % scale and shift
    Scale    = 1.1;
    Shift    = 10;
    Template = AS(1).Flux;
    Data     = Template;
    Data     = [(1:Scale:numel(Data).*Scale).', Data];
    Template = [(1:1:numel(Template)+Shift).', [zeros(Shift,1); Template]];
    Info     = imUtil.filter.xcorr1_scale_shift(Data,Template);
    %plot(Data(:,1),Data(:,2)); hold on; plot(Info.ScaledTemplate(:,1), Info.ScaledTemplate(:,2))
    
        
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end
