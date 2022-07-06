function searchVarMerged(Files, Args)
    %
   
    arguments
        Files          = 'LAST*_sci_merged_Cat_*.fits';   
        Args.Dir       = '';  % local dir
    end
    
    PWD = pwd;
    
    if isempty(Args.Dir)
        % search template in local dir
        
    else
        cd(Args.Dir);
    end
    
    List  = io.files.rdir(Files);
    Nlist = numel(List);   
   
    for Ilist=1:1:Nlist
        FileName = fullfile(List(Ilist).folder, List(Ilist).name);
        
        T = FITS.readTable1(FileName);
        if sum(T.PolyDeltaChi2>18)>0
            [Max, MaxInd] = max(T.PolyDeltaChi2);
            
            [Ilist, Max, T.Mean_SN_3(MaxInd)]
        
            T.RA(MaxInd)
            T.Dec(MaxInd)
            
            FileMergedMat = strrep(FileName, '_sci_merged_Cat_001.fits', '_sci_merged_MergedMat_001.hdf5');
            MatRA = h5read(FileMergedMat, '/RA');
            nanmean(MatRA)
        end
        
    end
        
    cd(PWD);
    
end
