function readULTRASATlabDB
% Make .mat DB files from the ASCII DB of ULTRASAT PSFs for 5 resolution levels
% Package: imUtil.psf
% Description: Make .mat DB files from the ASCII DB of ULTRASAT PSFs for 5 resolution levels
% Input  : - no user input is needed
% Output : - .mat DB files 
% Author : A. Krassilchtchikov (Feb 2023)
% Example: readULTRASATlabDB 
    
    % Lab DB parameters:

    Nx          = 1024; % grid size of the data files
    Ny          = 1024; 
    TableSize   = [Nx Ny];

    Nwave       = 91; % grid points in wavelength
    Nrad        = 25; % grid points in radius

    Wave        = linspace(200,1100,Nwave);
    %Rad         = linspace(0,10,Nrad);

    PixRat      = 47.5; % the ratio of the ULTRASAT pixel size to that of the experimental image

    % set the data path
    
    % Datacat     = '~/matlab/data/ULTRASAT/PSF/Raw/'; 

    Datacat     = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/PSF/Raw/');    
    
    % initialize data structures and read the data

    PSFdata     = zeros(Nx,Ny,Nwave,Nrad);

    PSFdata10   = zeros(216,216,Nwave,Nrad); % rounding does not give the same sizes as imresize, thus put by hand
    PSFdata5    = zeros(108,108,Nwave,Nrad);
    PSFdata2    = zeros(44,44,Nwave,Nrad);
    PSFdata1    = zeros(22,22,Nwave,Nrad);

    tic

    for Iw = 1:1:Nwave
        for Ir = 1:1:Nrad

            % construct a file name
            DataFile = sprintf('%s%s%d%s%d%s',Datacat,'psf_wl_',Wave(Iw),'nm/f_',Ir,'.txt');

            % open the data file
            FileID = fopen(DataFile,'r');

            % skip the first 10 lines in a datafile
            for Skip = 1:10
                Empty = fgets(FileID);
            end
                   
            % read the data  
            [PSFdata0, DataCount] = fscanf(FileID,'%f %f',TableSize); 
            
            % TRANSPOSE the data matrix so that it complies to the original table as stored in the datafile
            % PSFdata0 = PSFdata0'; 
                    
            % check the amount of the data
            if DataCount ~= Nx*Ny 
                fprintf('%s%s\n','Incomplete data file? ', DataFile)
            else
                fprintf('%s%s\n','Read data from', DataFile)
            end

            % close the file
            fclose(FileID);

            % normalize to unity and put into the main 4D array:

            PSFdata(:,:,Iw,Ir) = PSFdata0 / sum( PSFdata0,'all'); 

            % regrid to larger pixel scales:

            PSF10 = imresize(PSFdata0,10./PixRat,'bilinear');
            PSF5  = imresize(PSFdata0,5./PixRat,'bilinear');
            PSF2  = imresize(PSFdata0,2./PixRat,'bilinear');
            PSF1  = imresize(PSFdata0,1./PixRat,'bilinear');

            PSFdata10(:,:,Iw,Ir) = PSF10 / sum( PSF10,'all');
            PSFdata5(:,:,Iw,Ir)  = PSF5  / sum( PSF5, 'all');
            PSFdata2(:,:,Iw,Ir)  = PSF2  / sum( PSF2, 'all');
            PSFdata1(:,:,Iw,Ir)  = PSF1  / sum( PSF1, 'all');
            
        end
    end

    toc

    % save the data in .mat files: 
            
    tic
            
    Dummy = 0;
    save('ULTRASATlabPSF47.5.mat','Dummy','PSFdata','-v7.3');
            
    toc 

    fprintf('%s\n','DB0 written')

    tic

    save('ULTRASATlabPSF10.mat','Dummy','PSFdata10','-v7.3');
    save('ULTRASATlabPSF5.mat' ,'Dummy','PSFdata5' ,'-v7.3');
    save('ULTRASATlabPSF2.mat' ,'Dummy','PSFdata2' ,'-v7.3');
    save('ULTRASATlabPSF1.mat' ,'Dummy','PSFdata1' ,'-v7.3');

    toc
    
    fprintf('%s\n','DB1-4 written')
    
end

