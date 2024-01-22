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
        Fd
        SigmaN
        SigmaR

        ZeroPadRowsFFT   = [];
        ZeroPadColsFFT   = [];
    end
    
    properties (Hidden)  % auxilary images
       
        %FFT
        R_hat
        Pr_hat
        N_hat
        Pn_hat

        D_hat
        Pd_hat
        S_hat
        
        D_den_hat
        D_num_hat
        D_denSqrt_hat
        P_deltaNhat
        P_deltaRhat
        
        F_S

        Zvec_hat
    end


    methods % constructor
       
    end
    
    methods % setters/getters
        function Val=get.R_hat(Obj)
            % getter for R_hat

            if isempty(Obj.R_hat)
                % R_hat is not available - calculate
                if Obj.Ref.isemptyImage
                    error('Ref image is not populated');
                else
                    Obj.R_hat = fft2(Obj.Ref.Image, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                end
            else
                % R_hat is already available - use as is
            end
            Val = Obj.R_hat;
        end

        function Val=get.N_hat(Obj)
            % getter for N_hat

            if isempty(Obj.N_hat)
                % N_hat is not available - calculate
                if Obj.New.isemptyImage
                    error('New image is not populated');
                else
                    Obj.N_hat = fft2(Obj.New.Image, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                end
            else
                % N_hat is already available - use as is
            end
            Val = Obj.N_hat;
        end

        function Val=get.Pr_hat(Obj)
            % getter for Pr_hat

            if isempty(Obj.Pr_hat)
                % Pr_hat is not available - calculate
                if Obj.Ref.isemptyPSF
                    error('Ref PSF is not populated');
                else
                    % pad and shift PSF to full image size
                    ImageSize = size(Obj.Ref.Image);

                    % Padded Pr to the size of the full image and shifted
                    % such that the PSF center is at origin:
                    Pr = Obj.Ref.PSFData.getPSF('StampSize',ImageSize, 'fftshift','fftshift');

                    Obj.Pr_hat = fft2(Pr, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                end
            else
                % Pr_hat is already available - use as is
            end
            Val = Obj.Pr_hat;

        end

        function Val=get.Pn_hat(Obj)
            % getter for Pn_hat

            if isempty(Obj.Pn_hat)
                % Pn_hat is not available - calculate
                if Obj.New.isemptyPSF
                    error('New PSF is not populated');
                else
                    % pad and shift PSF to full image size
                    ImageSize = size(Obj.New.Image);

                    % Padded Pr to the size of the full image and shifted
                    % such that the PSF center is at origin:
                    Pn = Obj.New.PSFData.getPSF('StampSize',ImageSize, 'fftshift','fftshift');

                    Obj.Pn_hat = fft2(Pn, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                end
            else
                % Pn_hat is already available - use as is
            end
            Val = Obj.Pn_hat;

        end

        function Val=get.D_hat(Obj)
            % getter for D_hat

            if isempty(Obj.D_hat)
                % D_hat is not available - calculate
                if Obj.isemptyImage
                    % consider calculating D here
                    fprintf('In the future D will be calculated in the getter')
                    error('D image is not populated');
                else
                    Obj.D_hat = fft2(Obj.Image, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                end
            else
                % D_hat is already available - use as is
            end
            Val = Obj.D_hat;
        end

        function Val=get.Pd_hat(Obj)
            % getter for Pd_hat

            if isempty(Obj.Pd_hat)
                % Pd_hat is not available - calculate
                if Obj.isemptyPSF
                    error('D PSF is not populated');
                else
                    % pad and shift PSF to full image size
                    ImageSize = size(Obj.Image);

                    % Padded Pd to the size of the full image and shifted
                    % such that the PSF center is at origin:
                    Pd = Obj.PSFData.getPSF('StampSize',ImageSize, 'fftshift','fftshift');

                    Obj.Pd_hat = fft2(Pd, Obj.ZeroPadRowsFFT, Obj.ZeroPadColsFFT);
                end
            else
                % Pd_hat is already available - use as is
            end
            Val = Obj.Pd_hat;

        end

        function Val=get.S_hat(Obj)
            % getter for S_hat

            % get D_hat
            % get_Pd_hat
            % cross correlate
            % normalize to sigma units

        end

        % Need getters for:
        % Fn
        % Fr
        % Fd
        % SigmaN
        % SigmaR

        % need getters for:
        % D_den_hat
        % D_num_hat
        % D_denSqrt_hat
        % P_deltaNhat
        % P_deltaRhat
        % 
        % F_S
        % 
        % Zvec_hat

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

        function Obj=subtractionD(Obj, Args)
            %
            % Input  : - 
            %          * ...,key,val,...
            %            'AbsFun' - absolute value function.
            %                   Default is @(X) abs(X)
            %            'Eps' - A small value to add to the demoninators in order
            %                   to avoid division by zero due to roundoff errors.
            %                   Default is 0. (If needed set to about 100.*eps).
            %            'CleanPd' - A logical indicating if to clean Pd (zero low
            %                   frequencies).
            %                   Default is true.
            

            arguments
                Obj
                
                Args.AbsFun            = @(X) abs(X);
                Args.Eps               = 0;
                Args.CleanPd logical   = true;Args.PopS_hat          = true;
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                
                [Obj(Iobj).D_hat, Obj(Iobj).Pd_hat, Obj(Iobj).Fd, Obj(Iobj).F_S, Obj(Iobj).D_den, Obj(Iobj).D_num, Obj(Iobj).D_denSqrt, Obj(Iobj).P_deltaNhat, Obj(Iobj).P_deltaRhat] = subtractionD(Obj(Iobj).N_hat,...
                                                                                                           Obj(Iobj).R_hat,...
                                                                                                           Obj(Iobj).Pn_hat,...
                                                                                                           Obj(Iobj).Pr_hat,...
                                                                                                           Obj(Iobj).SigmaN,...
                                                                                                           Obj(Iobj).SigmaR,...
                                                                                                           Obj(Iobj).Fn,...
                                                                                                           Obj(Iobj).Fr,...
                                                                                                           'AbsFun',Args.AbsFun,...
                                                                                                           'Eps',Args.Eps,...
                                                                                                           'IsFFT',true,...
                                                                                                           'IsOutFFT',true,...
                                                                                                           'CleanPd',Args.CleanPd);

    
            end
        end


        function Obj=subtractionS(Obj, Args)
            % Given D and Pd, populate S and S_hat
            %
            %            'PopS_hat' - A logical indicating if to populate
            %                   S_hat. Default is true.

            arguments
                Obj
                Args.PopS_hat logical    = true;
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                S_hat           = Obj(Iobj).D_hat.*conj(Obj(Iobj).Pd_hat);
                if Args.PopS_hat
                    Obj(Iobj).S_hat = S_hat;
                end
                Obj(Iobj).S     = ifft2(Obj(Iobj).S_hat);
            end
        end


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
