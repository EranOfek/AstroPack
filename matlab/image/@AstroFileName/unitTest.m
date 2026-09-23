function Result=unitTest()
    % unitTest for AstroFileName class
    
    %%
    AF = AstroFileName;

    if AF.nFiles~=0
        error('Problem with AstroFileName.nFiles');
    end

    AF.JD = 2451545+rand(5,1);
    AF.julday2time;
    AF.genFile

    %% parse -> genFile round trip (issue #1313)
    % a compressed file is expressed by FileType "fits.fz" (the Compression
    % property was removed); the round trip is what PipelineDemon relies on
    % to recognize malformed raw names (issue #1286)
    Name = "LAST.01.08.03_20230616.222625.384_clear_346+79_001_001_001_sci_raw_Image_1.fits";
    for Ext=["", ".fz"]
        AF = AstroFileName.parseString2AstroFileName(Name + Ext);
        assert(AF.genFile==Name+Ext, 'AstroFileName: parse -> genFile round trip failed for %s', Name+Ext)
    end
    assert(AF.FileType=="fits.fz", 'AstroFileName: .fits.fz must be parsed into FileType "fits.fz"')

    %% an illegal Type is reported with its value (issue #1290)
    AF = AstroFileName;
    try
        AF.Type = ["sci"; "bad"];
        error('AstroFileName: an illegal Type must raise an error')
    catch ME
        assert(contains(ME.message, '(element 2): bad'), 'AstroFileName: unexpected error: %s', ME.message)
    end



    %%

    Result = true;
end