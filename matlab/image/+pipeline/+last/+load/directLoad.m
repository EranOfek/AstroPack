function [Result] = directLoad(TempName, Args)
    % Direct search and load images in LAST directory tree (No DB).
    %     Search for all files with a given file name template and load
    %     them into AstroImage/AstroCatalog/MatchedSources object.
    %     The type of loaded object is decided by the name of the first
    %     file to load (can't mix file types).
    % Input  : - File template name to load.
    %            Default is 'LAST*_TNO*_016_sci_proc_Image_1.fits'.
    %          * ...,key,val,... 
    %            'Path' - Default is pwd.
    % Output : - AstroImage/AstroCatalog/MatchedSources of loaded files.
    % Author : Eran Ofek (2026 Jan) 
    % Example: AI=pipeline.last.load.directLoadCrop('LAST*_TNO*_016_sci_proc_Image_1.fits');

    arguments
        TempName          = 'LAST*_TNO*_016_sci_proc_Image_1.fits';
        Args.Path         = pwd;
    end

    PWD = pwd;
    cd(Args.Path);

    F=io.files.findFiles(TempName,'NotModified',0,'MinSize',[],'MaxSize',[]);
    FN = fullfile({F.folder},{F.name}).';
    if isempty(FN)
        Result = [];
    else
        if contains(FN{1}, 'Image')
            Result = AstroImage.readProducts(FN);
        elseif contains(FN{1}, 'Cat')
            Result = AstroCatalog(FN);
        elseif contains(FN{1}, 'MergedMat')
            Result = MatchedSources.read(FN);
        else
            error('Unknown file type');
        end
    end
end
