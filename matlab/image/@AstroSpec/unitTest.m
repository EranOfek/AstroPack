function Result = unitTest
    % unitTest for AstroSpec

    %io.msgStyle(LogLevel.Test, '@start', 'AstroSpec test started')

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

    % binary operators on object arrays
    Wave = (4000:10:5000).';
    SpecA = [AstroSpec({[Wave, ones(size(Wave))]}), ...
             AstroSpec({[Wave, 2.*ones(size(Wave))]}), ...
             AstroSpec({[Wave, 3.*ones(size(Wave))]})];

    % funBinary: one to many - each element must use its own second operand
    Res = funBinary(SpecA(1), SpecA, @minus);
    assert(numel(Res)==3,'funBinary one-to-many returned the wrong number of elements')
    for Ir=1:1:3
        assert(all(abs(Res(Ir).Flux - (1-Ir))<10.*eps), ...
               'funBinary used the wrong element of the second object')
    end

    % rdivide: divisor shorter than the object array - flux must follow the
    % first operand, not the divisor index
    Res = rdivide(SpecA, {2});
    for Ir=1:1:3
        assert(all(abs(Res(Ir).Flux - Ir./2)<10.*eps), ...
               'rdivide used the wrong element of the first object')
    end

    % rdivide: unimplemented divisor types must fail explicitly
    Msg = '';
    try
        rdivide(SpecA, SpecA(1));
    catch ME
        Msg = ME.message;
    end
    assert(contains(Msg,'Unsupported class'), ...
           'rdivide did not reject an unsupported second input object')

    Result = true;
end
