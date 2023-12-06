function Result = unitTest()
    % unitTest for imUtil.image
    % Example: imUtil.image.unitTest
    
    
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    func_unitTest();
    
    % sub2ind_fast
    Ind=imUtil.image.sub2ind_fast([3 3],2,2);
    if Ind~=5
        error('sub2ind_fast failed');
    end
 
    % moment2
    Image = rand(1024,1024); X=rand(1e4,1).*1023+1; Y=rand(1e4,1).*1023+1;
    [M1,M2,Aper]=imUtil.image.moment2(Image,X,Y);
    
    Matrix = imUtil.kernel2.gauss(2, [31 31]);
    [M1,M2,Aper]=imUtil.image.moment2(Matrix,16,16);
    if ~(abs(M1.X-16)<0.01 & abs(M1.Y-16)<0.01)
        error('moment2 failed');
    end
    
    %%
    Nsim     = 1000; %1e5; %1000;
    SimFlux  = 1000; %1000;
    BackFlux = 100;
    
    
    Matrix = imUtil.kernel2.gauss(2, [51 51],[32.0 34.0]).*SimFlux + BackFlux;
    Matrix = poissrnd(Matrix); %+BackFlux;
    [M1,M2,AperN]=imUtil.image.moment2(Matrix,32,34, 'SubPixShift','none');

          
    AllFlux  = zeros(Nsim,1);
    AllFluxN = zeros(Nsim,1);
    AllB     = zeros(Nsim,1);
    X1       = zeros(Nsim,1);
    Y1       = zeros(Nsim,1);
    for Isim=1:1:Nsim
        Matrix = imUtil.kernel2.gauss(2, [31 31],[16.5 16.5]).*SimFlux + BackFlux;
        Matrix = poissrnd(Matrix); %+BackFlux;
        
        [M1,M2,Aper]=imUtil.image.moment2(Matrix,16,16, 'SubPixShift','fft');
        [M1,M2,AperN]=imUtil.image.moment2(Matrix,16,16, 'SubPixShift','none');
        AllFlux(Isim)  = Aper.AperPhot(end);
        AllFluxN(Isim) = AperN.AperPhot(end);
        AllB(Isim)     = Aper.AnnulusBack;
        X1(Isim) = M1.X;
        Y1(Isim) = M1.Y;
    end
    [mean(AllFlux),std(AllFlux), mean(AllFluxN), std(AllFluxN)]
    [mean(X1), std(X1), mean(Y1), std(Y1)]
    [mean(AllB), std(AllB)]
    if abs(mean(AllFlux)-SimFlux)>30
        error('imUtil.image.moment2 aper phot bias');
    end
    if abs(mean(X1)-16.5)>0.05
        error('imUtil.image.moment2 position bias');
    end
    
    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

%--------------------------------------------------------------------------


function Result = func_unitTest()
	% Function Unit-Test
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
   
	%io.msgStyle(LogLevel.Test, '@passed', 'passed');
	Result = true;
end


%--------------------------------------------------------------------------

