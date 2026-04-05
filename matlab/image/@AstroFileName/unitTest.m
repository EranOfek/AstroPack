function Result=unitTest()
    % unitTest for AstroFileName class
    
    %%
    AF = AstroFileName;

    if AF.nFiles~=0
        error('Problem with AstroFileName.nFiles');
    end

    AF.JD = 2451545+rand(5,1);
    AF.julday2time;
    AF.Compression = []; %"fz"
    AF.genFile



    %%

    Result = true;
end