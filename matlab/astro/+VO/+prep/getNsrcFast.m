function Nsrc = getNsrcFast(CatName)
% Fast count of sources per HTM cell using h5info metadata
% Package: VO.prep
% Description: Replacement for HDF5.get_nsrc that reads dataset sizes
%              from HDF5 metadata (h5info) instead of loading all data
%              with h5read. Orders of magnitude faster for large catalogs.
% Input  : - Catalog base name (e.g., 'DECaLS10').
%            HDF5 files must be in the current directory.
% Output : - Matrix [HTM_index, Nsrc] with source count per cell.
% Author : Dana Kovaleva + Claude (Mar 2026)
% Example: Nsrc = VO.prep.getNsrcFast('DECaLS10');

    Dir = dir(sprintf('%s_htm_*.hdf5', CatName));
    Ndir = numel(Dir);
    Nsrc = zeros(100 .* Ndir, 2);
    K = 0;
    for Idir = 1:Ndir
        Info = h5info(Dir(Idir).name);
        IndH = find(cellfun(@numel, strfind({Info.Datasets.Name}, '_')) == 1);
        Nih = numel(IndH);
        for Iih = 1:Nih
            K = K + 1;
            IndHTM = str2double(Info.Datasets(IndH(Iih)).Name(5:end));
            % Dataspace.Size is in MATLAB column-major order [Nrows, Ncols]
            % (HDF5.save writes with fliplr, so h5info reports it flipped back).
            DsSize = Info.Datasets(IndH(Iih)).Dataspace.Size;
            Nsrc(K, :) = [IndHTM, DsSize(1)];
        end
    end
    Nsrc = Nsrc(1:K, :);
end
