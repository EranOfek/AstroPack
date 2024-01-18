% Class for astronomical difference/subtraction images and transients data
%
%

classdef AstroDiff < Component
    
    properties (Dependent)
        % Linked to the Image in the D image
        Image    
        Back
        Var
        Mask
        Header
        Key
        PSF
    end

    properties
        Ref AstroImage
        New AstroImage
        
        % Option I:
        D ImageComponent  % or better a new DiffComponent (inclusing IsFFT), or adding IsFFT to ImageComponent?
        PSFData AstroPSF
        HeaderData AstroHeader

        % Option II:
        D AstroImage

        %
        S ImageComponent  % with IsFFT
        Scorr ImageComponent
        Z2 ImageComponent

        %
        Fn
        Fr

        %
        RA
        Dec
        Table AstroCatalog              % Measurments
        Cutouts
        DiffCutouts
    end
    
    properties (Hidden)

    end


    methods % constructor
       
    end
    
    methods % read/write

    end

    methods % utilities
        % norm

    end

    methods % main functionality
        % loadRef

        % register

        % estimateF

        % estimateVar

        % subtractionD

        % subtractionS

        % subtractionScorr

        % subtractionZ2

        % findTransients

        % injectArt

        

    end
    
    methods % display
        function ds9(Obj, Args)
            % Open selected cutouts in ds9
            
        end
        
    end    
    
    
    methods (Static) % Unit-Test
        Result = unitTest()
    end
    
end
