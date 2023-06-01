% AstroDataType is an enumartion class for image types
% It includes the following types:
% ADU | Counts | Intensity | Evt | ExpTime | Back | Var | Std | ErrAbs | ErrRel |
% Mask | Cat | Psf
classdef AstroDataType
   enumeration
      Data
      ADU
      Electrons
      Counts
      Intensity
      Evt
      ExpTime
      Back
      Var
      Std
      ErrAbs
      ErrRel
      Mask
      Cat
      PSF
   end
end
