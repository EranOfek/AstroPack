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

    % FluxUnits converts FluxErr/Back/Cont from their own values (issue #1186)
    Wave = (4000:10:5000).';  Nw = numel(Wave);
    SpecU = AstroSpec({[Wave, ones(Nw,1), 2.*ones(Nw,1), 3.*ones(Nw,1)]}, ...
                      {'Wave','Flux','FluxErr','Back'}, {'Ang','cgs/A','cgs/A',''});
    SpecU.Cont = 4.*ones(Nw,1);
    SpecU.FluxUnits = 'mJy';
    assert(all(abs(SpecU.FluxErr./SpecU.Flux - 2)<1e-12) && all(abs(SpecU.Back./SpecU.Flux - 3)<1e-12) && ...
           all(abs(SpecU.Cont./SpecU.Flux - 4)<1e-12), 'FluxUnits did not convert FluxErr/Back/Cont from their own values')
    assert(all(strcmp(SpecU.Data.Properties.VariableUnits(2:end), 'mJy')), 'FluxUnits did not relabel the converted columns')
    SpecU.FluxUnits = 'cgs/A';
    assert(all(abs(SpecU.Back - 3)<1e-12), 'FluxUnits round trip failed')

    % no FluxErr/Back columns are created
    SpecU = AstroSpec({[Wave, ones(Nw,1)]}, {'Wave','Flux'}, {'Ang','cgs/A'});
    SpecU.FluxUnits = 'mJy';
    assert(isequal(SpecU.Data.Properties.VariableNames, {'Wave','Flux'}), 'FluxUnits added columns')

    % magnitude units apply to Flux only
    SpecU = AstroSpec({[Wave, ones(Nw,1), 0.1.*ones(Nw,1)]}, {'Wave','Flux','FluxErr'}, {'Ang','cgs/A','cgs/A'});
    SpecU.FluxUnits = 'AB';
    assert(all(SpecU.FluxErr==0.1) && strcmp(SpecU.Data.Properties.VariableUnits{3}, 'cgs/A'), 'FluxUnits=AB changed FluxErr')
    SpecU.FluxUnits = 'cgs/A';
    assert(all(abs(SpecU.Flux - 1)<1e-10), 'FluxUnits AB round trip failed')

    WaveData = (1000:1:10000)';
    Temp = 5e3;
    AS = AstroSpec.blackBody(WaveData, Temp);

    Factors = AstroSpec.applyExtinctionZ(AS.Wave,0,1);

    assert(all(Factors>0),'Problem with applyExtinctionZ')

    % ObjName - default empty, and set by the spectrum factories
    assert(isempty(AstroSpec({rand(10,2)}).ObjName),'ObjName should default to empty')

    BBs = AstroSpec.blackBody((4000:10:9000).', [5800; 10000]);
    assert(strcmp(BBs(1).ObjName,'Planck spectrum T=5800.000000'), ...
           'blackBody did not set ObjName in the expected format')

    % Pickles file name -> spectral designation
    assert(strcmp(AstroSpec.picklesName('uka0v.mat'),'A 0.0 V'),'Problem with picklesName')
    assert(strcmp(AstroSpec.picklesName('uka05v.mat'),'A 0.5 V'),'Problem with picklesName half type')
    assert(strcmp(AstroSpec.picklesName('wg8iii.mat'),'wg8iii'),'picklesName should fall back to the file name')

    % zodiacal light / sky background spectrum
    Zod = AstroSpec.zodiacSpectrum;
    assert(numel(Zod)==1 && numel(Zod.Wave)==59,'Problem with zodiacSpectrum')

    ZodMat = AstroSpec.zodiacSpectrum([],'OutType','mat');
    assert(isequal(Zod.Wave,ZodMat(:,1)) && isequal(Zod.Flux,ZodMat(:,2)), ...
           'zodiacSpectrum AstroSpec and mat outputs disagree')

    ZodAll = AstroSpec.zodiacSpectrum([],'BackType','all');
    assert(numel(ZodAll)==3,'zodiacSpectrum BackType=all must return three elements')
    assert(isequal(ZodAll(2).Flux,Zod.Flux),'zodiacSpectrum BackType=all ordering changed')

    ZodInt = AstroSpec.zodiacSpectrum([5000; 5500]);
    assert(numel(ZodInt.Wave)==2,'Problem with zodiacSpectrum interpolation')

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
