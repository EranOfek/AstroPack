function Result = unitTest
    % unitTest for AstroTransmission

    % constructor
    AT = AstroTransmission(1);
    AT = AstroTransmission(1);





    AS = AstroSpec({rand(100,4)});

    % setters/getters
    NS = AS.copy();
    NS.WaveUnits = 'cm';
    if ~(strcmp(NS.WaveUnits,'cm') && all(abs(AS.Wave./NS.Wave./1e8 - 1)<10.*eps))
        error('Problem with WaveUnits conversion');
    end

    % convert flux units
    NS.FluxUnits = 'AB';

    Result = true;
end
