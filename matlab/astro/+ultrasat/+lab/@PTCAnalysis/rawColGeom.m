function G = rawColGeom(Obj)
    % Geometry of the raw readout columns of the stored image.
    %   Same derivation as parityMap (RAWSEC / RAWXOFF / ORIENT keys), but
    %   exposed as an index vector so that per-column statistics can be
    %   computed in either orientation. In the DESY orientation the readout
    %   columns run along the image ROWS (the analysis image is the TIFF
    %   half transposed and rotated by 180 deg).
    % Output : - Structure with
    %            RawCol - raw readout-column index of every image row
    %                     (ORIENT='desy') or column (ORIENT='tiff'), counted
    %                     from 1 at the first pixel column of the selected
    %                     gain half, so odd = detector columns 1,3,5,...
    %            Dim    - image dimension along which RawCol varies
    %                     (1 = rows, 'desy'; 2 = columns, 'tiff')
    %            Nx, Ny - image size
    % Example: G = P.rawColGeom
    if strcmp(Obj.Mode, 'region')
        H = Obj.AI(1).HeaderData;
    else
        A0 = ultrasat.lab.readPTC(Obj.DeviceDir, 'Test',Obj.Test, 'ReadImage',false, 'Gain',Obj.Gain, 'Orient',Obj.Orient);
        H  = A0(1).HeaderData;
    end
    RawSec = sscanf(H.getVal('RAWSEC'), '[%d:%d,%d:%d]').';
    Xoff   = H.getVal('RAWXOFF');
    G      = struct('Ny',H.getVal('NAXIS2'), 'Nx',H.getVal('NAXIS1'));
    switch lower(H.getVal('ORIENT'))
        case 'tiff'
            G.RawCol = RawSec(1) - Xoff + (0:G.Nx-1);
            G.Dim    = 2;
        case 'desy'
            G.RawCol = RawSec(2) - Xoff - (0:G.Ny-1).';
            G.Dim    = 1;
        otherwise
            error('ultrasat:lab:PTCAnalysis:orient', 'Unknown ORIENT %s', H.getVal('ORIENT'));
    end
end
