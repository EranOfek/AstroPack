function Const = accDisk_physConst(Args)
    % Physical constants (cgs) used by the astro.accretion.accDisk_* functions.
    % Package: astro.accretion
    % Description: Convenience wrapper that collects, into a single
    %              struct, the physical constants needed throughout the
    %              accDisk_* family of functions. All values are pulled
    %              from AstroPack's @constant class
    %              (https://github.com/EranOfek/AstroPack/blob/dev1/matlab/util/%40constant/constant.m),
    %              so this function has no hardcoded physics of its own
    %              -- it is only a thin, repeatedly-needed bundle. The
    %              one addition is a Julian year length in seconds
    %              (Yr = 365.25 * day), since @constant does not provide
    %              a year constant.
    % Input  : * ...,key,val,...
    %            'System' - 'cgs'|'SI'. Default is 'cgs'.
    %                       (All other astro.accretion.accDisk_* functions
    %                       assume cgs internally, so only override this
    %                       if you are using accDisk_physConst on its own.)
    % Output : - Const : A struct with fields G, c, h, kB, sigma, SunM,
    %                    mp, sigmaT, Yr (all in the requested unit system;
    %                    Yr is always in seconds).
    % Author : (fill in your name) (Sep 2026)
    % Example: Const = astro.accretion.accDisk_physConst();
    %          Const = astro.accretion.accDisk_physConst('System','SI');

    arguments
        Args.System (1,1) string {mustBeMember(Args.System, ["cgs","SI"])} = "cgs"
    end

    Sys = char(Args.System);

    Const.G      = constant.G(Sys);
    Const.c      = constant.c(Sys);
    Const.h      = constant.h(Sys);
    Const.kB     = constant.kB(Sys);
    Const.sigma  = constant.sigma(Sys);
    Const.SunM   = constant.SunM(Sys);
    Const.mp     = constant.mp(Sys);
    Const.sigmaT = constant.sigmaT(Sys);
    Const.Yr     = 365.25 .* constant.day(Sys);   % Julian year [s]; @constant has no 'yr'/'Yr'
end
