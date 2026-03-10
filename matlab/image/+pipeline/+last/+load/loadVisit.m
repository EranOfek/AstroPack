function [AllSI, Coadd, MS] = loadVisit(Path, Args)
    % Load all visit-related files into AstroImage/AstroCatalog/MatchedSources objects
    % Input  : - Path of visit to load.
    %            If empty, use current directory. Default is [].
    %          * ...,key,val,... 
    %            'TempName_IndivIm' - File template name for individual
    %                   images. If empty, do not read individual images.
    %                   Default is 'LAST*_sci_proc_Image_*.fits'.
    %            'TempName_Coadd' - File template name for coad
    %                   images. If empty, do not read coadd images.
    %                   Default is 'LAST*_sci_coadd_Image_*.fits'.
    %            'TempName_MS' - File template name for MatchedSources
    %                   objects. If empty, do not read matched sources objects.
    %                   Default is 'LAST*_sci_merged_MatchedMat_*.hdf5'.
    % Output : - An AstroImage object with all individual images.
    %          - An AstroImage object with all coadd images.
    %          - An MatchedSources object with all matched sources files.
    % Author : Eran Ofek (2026 Mar) 
    % Example: [AllSI, Coadd, MS] = pipeline.last.load.loadVisit;

    arguments
        Path                   = [];
        Args.TempName_IndivIm  = 'LAST*_sci_proc_Image_*.fits';
        Args.TempName_Coadd    = 'LAST*_sci_coadd_Image_*.fits';
        Args.TempName_MS       = 'LAST*_sci_merged_MatchedMat_*.hdf5';
    end

    PWD = pwd;
    if ~isempty(Path)
        cd(Path);
    end

    % load individual images
    if isempty(Args.TempName_IndivIm)
        AllSI = [];
    else
        AllSI = pipeline.last.load.directLoadCrop(Args.TempName_IndivIm);
        % order
        St = AllSI.getStructKey('MIDJD','CROPID','COUNTER');
        [~,SI] = sortrows([[St.MIDJD].', [St.CROPID].'],[1 2]);
        Nepoch = max([St.COUNTER]);
        Nsub   = max([St.CROPID]);
        AllSI = reshape(AllSI(SI),[Nepoch Nsub]);        
    end

    % loadd coadd images
    if isempty(Args.TempName_Coadd)
        Coadd = [];
    else
        Coadd = pipeline.last.load.directLoadCrop(Args.TempName_Coadd);
    end

    % load MS
    if isempty(Args.TempName_MS)
        MS = [];
    else
        MS = pipeline.last.load.directLoadCrop(Args.TempName_MS);
    end

    cd(PWD);


end
