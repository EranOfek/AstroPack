% Class for astronomical difference/subtraction images and transients data
%
%

classdef AstroDiff < AstroImage
    
    properties (Dependent)
      
    end

    properties
        Ref AstroImage
        New AstroImage
        
        S ImageComponent  % with IsFFT
        Scorr ImageComponent
        Z2 ImageComponent

        %
        Fn
        Fr

    end
    
    properties (Hidden)
        FFT
%         R_hat
%         N_hat
%         D_hat
%         Pd_hat
%         S_hat
%         Zvec_hat
        
        Fd
        F_S
%         AuxFFT
%         D_den_hat
%         D_num_hat
%         D_denSqrt_hat
%         P_deltaNhat
%         P_deltaRhat
    end


    methods % constructor
       
    end
    
    methods % setters/getters
        function Val=get.Image(Obj)
            % getter for Image
            Val = Obj.D.Image;
        end
        
        function set.Image(Obj, Val)
            % setter for Image
            Obj.D.Image = Val;
        end
        
        
        
    end
    
    methods % read/write

    end

    methods % utilities
        % norm

        % cleanFFT

        % fft(Obj, Fields) - store the results in the *_hat properties
        % 

        % ifft(Obj, Fields) from hat to non-hat

        % shift
        
        % shiftfft
        
        % resizePSF

    end

    methods % main functionality
        % loadRef

        % register

        % estimateF

        % estimateVar

        % subtractionD
        % [D_hat, Pd_hat, Fd, F_S, D_den, D_num, D_denSqrt, P_deltaNhat, P_deltaRhat] = subtractionD(N_hat, R_hat, Pn_hat, Pr_hat, SigmaN, SigmaR, Fn, Fr, Args)

        % subtractionS

        % subtractionScorr

        % subtractionZ2

        % findTransients
    end
    
    methods % injection simulations
        % injectArtNew

    end

    
    methods % transients inspection and measurment
        % transientsCutouts

        % mergeTransients

        % searchSolarSystem

        % nearRedshift

        % nearGalaxy
        
        % nearStar

    end

    
    methods % display
        % ds9
        % Display Ref, New, D, S, Z2 in ds9 and mark transients
        
    end    
    
    
    methods (Static) % Unit-Test
        Result = unitTest()
    end
    
end
