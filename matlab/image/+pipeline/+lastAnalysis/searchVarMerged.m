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
    %%
    BD=BitDictionary;
    
    List  = io.files.rdir(Files);
    Nlist = numel(List);   
   
    Nsub = 24;
    State = false(Nlist./Nsub, Nsub);
    for Ilist=1:1:Nlist
        FileName = fullfile(List(Ilist).folder, List(Ilist).name);
        FileMergedMat = strrep(FileName, '_sci_merged_Cat_001.fits', '_sci_merged_MergedMat_001.hdf5');
        IP=ImagePath.parseFileName(FileMergedMat);
        CropID = str2double(IP.CropID);        
        
        FC = sum(State(:,CropID));
        Counter = sum(State(:,CropID)) + 1;
        
        State(Counter, CropID) = true;
        
        
        MS(Counter, CropID) = MatchedSources.read(FileMergedMat);
        
        T = FITS.readTable1(FileName);
        [FlagBad] = findBit(BD, T.FLAGS, {'Saturated','NearEdge','Overlap','NaN'});
        
        FlagSelected = T.PolyDeltaChi2>16 & ~FlagBad & T.Std_MAG_CONV_2>0.1;
                
        %T = T(~FlagBad,:);
        if any(FlagSelected)
            
            Ilist
            
            
            [SC2, SI] = sort(T.PolyDeltaChi2);
            FlagBad   = FlagBad(SI);
            
            %[Max, MaxInd] = max(T.PolyDeltaChi2);
            MaxInd = SI(end);
            
            [Ilist, Max, T.Mean_SN_3(MaxInd), T.RA(MaxInd), T.Dec(MaxInd)]
        
            T.RA(MaxInd)
            T.Dec(MaxInd)
            
            
            MatRA = h5read(FileMergedMat, '/RA');
            MatMag = h5read(FileMergedMat, '/MAG_CONV_2');
            MS = MatchedSources.read(FileMergedMat);
            nanmean(MatRA)
            plot(MatMag(:,SI(end-0)),'o')
        end
        
    end
        
    %%
    cd(PWD);
    
end
