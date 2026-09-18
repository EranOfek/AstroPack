function [SzOut, varargout] = accDisk_broadcast(varargin)
    % Implicitly expand (broadcast) several arrays to a common size.
    % Package: astro.accretion
    % Description: Internal utility used by the accDisk_* functions to
    %              vectorize over an arbitrary grid of physical
    %              parameters (BH mass, accretion rate, wavelength,
    %              redshift, inner radius, ...). Given N input arrays -
    %              any mix of scalars, vectors, matrices or N-D arrays -
    %              this determines the common size they broadcast to
    %              under MATLAB's implicit-expansion rules (each
    %              dimension must either agree, or be singleton, across
    %              all inputs that have that dimension), and returns
    %              every input explicitly expanded to that common size,
    %              so that simple linear/element indexing afterwards is
    %              guaranteed to line up between all of them. Inputs
    %              with incompatible sizes make MATLAB raise its normal
    %              "arrays have incompatible sizes for this operation"
    %              error.
    % Input  : - Arr1, Arr2, ... : Any number (>=1) of numeric arrays.
    % Output : - SzOut : The common broadcast size, e.g. [3 4].
    %          - Arr1f, Arr2f, ... : Each input, explicitly expanded to
    %            size SzOut (one output per input, same order).
    % Author : (fill in your name) (Sep 2026)
    % Example: [Sz, Af, Bf] = astro.accretion.accDisk_broadcast(ones(3,1), ones(1,4));
    %          % Sz = [3 4]; Af is 3x4 (each column repeated), Bf is 3x4 (each row repeated)

    NumIn = numel(varargin);
    Zero  = 0;
    for I = 1:NumIn
        Zero = Zero + zeros(size(varargin{I}));
    end
    SzOut = size(Zero);

    varargout = cell(1, NumIn);
    for I = 1:NumIn
        varargout{I} = varargin{I} + Zero;
    end
end
