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

    %% selectByPropVal with CreateNewObj returns the selection (issue #1315)
    Dark = "LAST.01.08.03_20260913.150000.000_clear__001_001_001_dark_raw_Image_1.fits";
    Flat = "LAST.01.08.03_20260913.161144.996_clear_284+28_001_001_001_twflat_raw_Image_1.fits";
    AF = AstroFileName.parseString2AstroFileName([Dark; Flat]);
    [Sel, Flag] = AF.selectByPropVal('Type', "dark", 'CreateNewObj',true);
    assert(Sel.nFiles==1 && Sel.Type=="dark" && AF.nFiles==2 && isequal(Flag(:).', [true false]),...
           'AstroFileName: selectByPropVal with CreateNewObj must return the selection and keep the input')
    Sel = AF.selectByPropVal('Type', "bias", 'CreateNewObj',true);
    assert(Sel.nFiles==0, 'AstroFileName: an empty selection must give an empty object')

    %% the JD of an empty object is empty, not the current date (issue #1315)
    AF = AstroFileName;
    assert(isempty(AF.julday), 'AstroFileName: julday of an empty object must be empty')
    assert(isempty(AF.selectLastJD), 'AstroFileName: selectLastJD of an empty object must be empty')

    %% julday2timeString carries the rounded seconds (issue #1315)
    assert(AstroFileName.julday2timeString(2461297.5 + 86399.9996/86400)=="20260915.000000.000",...
           'AstroFileName: 23:59:59.9996 must round to the next day')
    assert(AstroFileName.julday2timeString(2461297.5 + 59.9996/86400)=="20260914.000100.000",...
           'AstroFileName: 00:00:59.9996 must round to the next minute')
    assert(AstroFileName.julday2timeString(2461297.12742477)=="20260913.150329.500",...
           'AstroFileName: julday2timeString of a plain JD changed')

    %% raw path: UT date by default, night date with RawDateFromJD (issue #1315)
    AF = AstroFileName.parseString2AstroFileName("LAST.01.08.03_20260914.020000.000_clear_803_001_001_001_sci_raw_Image_1.fits");
    AF.BasePath = "/base"; AF.Path = [];
    assert(AF.genPath(1,'PathType','raw','BasePathIncludeProjName',false)==fullfile("/base","2026","09","14","raw"),...
           'AstroFileName: the raw path must use the UT date by default')
    assert(AF.genPath(1,'PathType','raw','BasePathIncludeProjName',false,'RawDateFromJD',true)==fullfile("/base","2026","09","13","raw"),...
           'AstroFileName: the raw path with RawDateFromJD must use the date of the night')

    %% groupByTimeGaps returns AstroFileName groups (issue #1315)
    T  = ["20260913.1611" + compose("%02d",(10:5:40)') + ".000"; "20260914.0240" + compose("%02d",(10:5:35)') + ".000"];
    AF = AstroFileName.parseString2AstroFileName("LAST.01.08.03_" + T + "_clear_284+28_001_001_001_twflat_raw_Image_1.fits");
    [G, Res] = AF.groupByTimeGaps('MinInGroup',5);
    assert(numel(G)==2 && isa(Res,'AstroFileName') && isequal([Res.nFiles],[7 6]),...
           'AstroFileName: groupByTimeGaps must return AstroFileName groups')

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