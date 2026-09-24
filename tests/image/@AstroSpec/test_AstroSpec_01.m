function Result = unitTest
    % unitTest for AstroSpec

    io.msgStyle(LogLevel.Test, '@start', 'AstroSpec test started')

    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);

    % constructor

    FileList = dir('*');
    FileName = 'SN2011fe_2011-09-01_00-00-00_WHT-4.2m_ISIS_None.dat';
    if ismember(FileName,{FileList.name})
        Spec1 = readtable(FileName,"NumHeaderLines",0);
        AS = AstroSpec(Spec1{:,:});
    else
        AS = AstroSpec({rand(100,4)});
    end
    cd(PWD);
    % setters/getters
    NS = AS.copy();
    NS.WaveUnits = 'cm';
    if ~(strcmp(NS.WaveUnits,'cm') && all(abs(AS.Wave./NS.Wave./1e8 - 1)<10.*eps))
        error('Problem with WaveUnits conversion');
    end

    % convert flux units
    NS.FluxUnits = 'cgs/hz';
    AS.FluxUnits = 'mJy';
    
    assert(all(abs(NS.Flux./AS.Flux*1e26-1)<10.*eps) || all(abs(NS.FluxErr./AS.FluxErr*1e26-1)<10.*eps),'Problem with FluxUnits conversion')

    WaveData = (1000:1:10000)';
    Temp = 5e3;
    AS = AstroSpec.blackBody(WaveData, Temp);

    Factors = AstroSpec.applyExtinctionZ(AS.Wave,0,1);

    assert(all(Factors>0),'Problem with applyExtinctionZ')

    Result = true;
end
