function [MeanEff, EffMat, CentralWave, Alt] = fracOfLightWavelengthInFiber(SeeingSigma, FiberRadius, Args)
    % Calculate the effect of color refraction on the spectrum using circular slit (fiber).
    %     Color refraction will smear the PSF such that the blue and red
    %     parts may go out of the fiber.
    %     This function can be used to estimate the transmission effiency
    %     as a function of wavelength and altitude, due to this
    %     color-refraction fiber losses.
    %     It also estimate the best central wavelength of the source that
    %     need to be in the fiber center.
    % Input  : - Seeing + jitter in units of the Gaussian sigma (FWHM/2.35)
    %            in arcsec. Default is sqrt((1.4./2.35).^2 + 0.3.^2
    %          - Fiber radius [arcsec]. Default is 1.5.
    %          * ...,key,val,... 
    %            'Alt' - Vectot of Altitudes [deg]. Default is (30:15:90)
    %            'Wave' - Vector of wavelength [Ang].
    %                   Default is (4000:100:8500).'
    %            'CentralWave' - Central wavelength [Ang].
    %                   If empty, then estimate optimal value.
    %            'Plot' - Default is true.
    % Output : - Mean eff. as a function of Alt.
    %          - Eff(Wave,Alt)
    %          - Central wavelength [Ang].
    %          - Vector of Alt.
    % Author : Eran Ofek (2024 Nov) 
    % Example: [MeanEff, Eff, CW, Alt] = telescope.spec.fracOfLightWavelengthInFiber;

    arguments
        SeeingSigma            = sqrt((1.4./2.35).^2 + 0.3.^2);  % 
        FiberRadius            = 1.5;
        Args.Alt               = [30, 45, 60, 75, 90]; %(30:10:90).';
        Args.Wave              = (4000:100:8500)';
        Args.CentralWave       = []; %5100;
        Args.Plot logical      = true;
    end    
    RAD = 180./pi;
    
    Alt = Args.Alt;
    
    Wave  = Args.Wave(:);
    Nwave = numel(Wave);
        
    if isempty(Args.CentralWave)
        % find nominal CentralWave
        
        MeanAlt = mean(Args.Alt);
        
        W1 = Wave(1);
        W2 = Wave(end);
        Wm = 0.5.*(W1 + W2);
        
        Cont = true;
        while Cont
            D1 = abs(diff(celestial.coo.refraction_wave(MeanAlt./RAD,[Wave(1) Wm]),[],2));
            D2 = abs(diff(celestial.coo.refraction_wave(MeanAlt./RAD,[Wave(end) Wm]),[],2));

            %[D1, D2].*RAD.*3600
            
            if D1>D2
                W1 = W1;
                W2 = Wm;
            else
                W1 = Wm;
                W2 = W2;
            end
            Wm = 0.5.*(W1 + W2);
            
            if abs(D1./D2 - 1)<1e-3
                Cont = false;
            end
        end
        CentralWave = Wm;
    else
        CentralWave = Args.CentralWave;
    end
    
    Nalt = numel(Args.Alt);
    
    EffMat = zeros(Nwave, Nalt);
    MeanEff = zeros(Nalt,1);
    LG      = cell(1,Nalt);
    for Ialt=1:1:Nalt
        
        

        D = diff(celestial.coo.refraction_wave(Args.Alt(Ialt)./RAD,[Wave CentralWave.*ones(Nwave,1)]),[],2).*RAD.*3600;

        [Eff] = tools.math.geometry.integralShiftedGaussianInCircle(FiberRadius, SeeingSigma, abs(D));
        
        %Eff = (erf((FiberRadius+D)./(sqrt(2).*Sigma)) - erf((D-FiberRadius)./(sqrt(2).*Sigma))).*0.5;

        EffMat(:,Ialt) = Eff(:);
        
        MeanEff(Ialt) = mean(Eff);
        
        if Args.Plot
            plot(Wave, Eff, 'LineWidth',2)
            hold on
           
            LG{Ialt} = sprintf('%4.1f deg, %4.2f',Alt(Ialt), MeanEff(Ialt));
        end
                
    end
    
    if Args.Plot
        H = xlabel('Wavelength [\AA]');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        H = ylabel('Transmission');
        H.FontSize = 18;
        H.Interpreter = 'latex';
        
        legend(LG{:});
    end

end
