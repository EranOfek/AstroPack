% catsHTM static class. Read / write HDF5/HTM catalogs
% Package: @catsHTM
% Description: A static class for catsHTM related functions.
% Tested : Matlab R2014a
%     By : Eran O. Ofek                    Jan 2018
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Reliable: 2
%--------------------------------------------------------------------------
%
% #functions (autogen)
% addSource - Insert new sources into a catsHTM catalog (read-only source). Package: @catsHTM Description: Add sources to an existing catsHTM catalog. The source catalog at BaseDir is read but never modified; modified files are written under OutDir. (Author: Dana Kovaleva, May 2026)
% catalogs - List of catsHTM catalogs Example: Data = catsHTM.catalogs
% catalogs_html - generate an html table of catalogs Example: catsHTM.catalogs_html
% catalogSignature - Compute a lightweight version signature of a catsHTM catalog. Package: @catsHTM Description: LayoutHash (per-cell counts) + ColHash + optional ChecksumHash, so persisted pointers can be validated. Cheap (index-only). (Author: Dana Kovaleva, Aug 2026)
% catRowID - Collapse a (CellID,RowInCell) storage pointer into one contiguous per-catalog scalar id. Package: @catsHTM Description: CatRowID = block-start(CellID) + RowInCell; version-bound. Inverse is catRowID2Pointer. (Author: Dana Kovaleva, 2026)
% catRowID2Pointer - Invert a scalar CatRowID back to a (CellID,RowInCell) pointer. Package: @catsHTM Description: Exact inverse of catRowID. (Author: Dana Kovaleva, 2026)
% checkCatalogSignature - Validate a stored catsHTM signature against the catalog's current state. Package: @catsHTM Description: Classifies change as valid / columns-changed / stale-layout / stale-suspect so stale pointers are caught before use. (Author: Dana Kovaleva, Aug 2026)
% cone_search - Cone earch on local HDF5/HTM catalog Package: @catsHTM Description: Perform a cone search around RA/Dec on a local catalog in HDF5 format sorted into HTM.
% count_edge_in_cat - Example: catsHTM.count_edge_in_cat('APASS');
% create_catalog_lists4wget - Create list of catalogs foe wget including checsums
% create_indiv_catalog_lists4wget - Create list of individual catalogs for wget including checsums
% crossIDCatsHTM - Build a cross-identification index table between an anchor catsHTM catalog and all/selected others over a field. Package: @catsHTM Description: cone_search + cross-match to a growing master list; orphan handling; stable CellID_/RowInCell_ pointer per catalog. Returns [T, Cats_cone, Summary]. (Author: Dana Kovaleva, 2026)
% delete_dataset - Delete a dataset from an HDF5 file using H5L.delete (in place). Package: @catsHTM Description: Unlink a dataset from an HDF5 file. No-op if the dataset does not exist. (Author: Dana Kovaleva, May 2026)
% filename2base - Convert HDF5/HTM file name to catalog name (file base name) Package: @catsHTM Description: Convert HDF5/HTM file name (e.g., 'APASS_htm.hdf5') to catalog name (file base name; e.g., 'APASS').
% gatherByPointer - Read catalog rows addressed by (CellID,RowInCell) pointers. Package: @catsHTM Description: Groups pointers by cell, loads each htm_<id> once, returns [Data,ColNames]; any column, no cone_search. (Author: Dana Kovaleva, 2026)
% gatherCrossIDData - Materialize a crossIDCatsHTM index T into a per-source data table. Package: @catsHTM Description: Source='cats' reads the Cats_cone snapshot via Ind_; Source='pointer' reads live from catsHTM via CellID_/RowInCell_. (Author: Dana Kovaleva, 2026)
% generate_edge_cat - OBSOLOTE
% get_file_var_from_htmid - Construct file and var name for HTM file stored in HDF5 Package: @catsHTM Description: Given a file base (e.g., 'UCAC4') and HTM ID and number of files in HDF5 file, construct the HDF5 file name (e.g., UCAC4_htm_032400.hdf5),
% get_index_filename - Get HDF5/HTM index file name and variable name from CatName Package: @catsHTM Description: Get HDF5/HTM index file name and variable name from CatName.
% get_nsrc - Count number of sources over all HTM in HDF5 files Package: @catsHTM
% getNsrcMeta - Count sources per HTM cell from HDF5 metadata only. Package: @catsHTM Description: Same output as catsHTM.get_nsrc but reads each dataset's row count from h5info dataspace dimensions instead of loading data with h5read. (Author: Dana Kovaleva, Mar 2026)
% htm_search_cone - Search for all HTM leafs interscting a small circle (cone search) Package: @catsHTM Description: Search for all HTM leafs interscting a small circle (i.e., cone search).
% insertColumns - Insert one or more new columns into every HTM cell of a catsHTM catalog. Package: @catsHTM Description: Adds a contiguous block of new columns in a single pass by rewriting every htm_<id> dataset and updating the ColCell .mat file. Auto-shifts SortCol if inserted at or before its position. insertColumn is a single-column alias. (Author: Dana Kovaleva, May 2026)
% load_1htm - Load a single tile of HDF5/HTM catalog Package: @catsHTM Description: Load a single HTM tile of HDF5/HTM catalog based on its HTM index. This is slower relative to catsHTM.load_cat,
% load_cat - Load catalog stored in an HDF5 file Package: @catsHTM Description: Load catalog stored in an HDF5 file. Given a a catalog in HDF5 file created by HDF5.save_cat, load the catalog. The catalog is
% load_cat_edge - Load and concat HDF5/HTM catalog and its edge catalog Package: @catsHTM Description: Load and concat HDF5/HTM catalog and its edge catalog
% load_cat_with_edges - load catalogs from all HTMs near a specific HTM triangle. Package: @catsHTM Description:
% load_colcell - Load ColCell and ColUnits for an HDF5/HTM catalog Package: @catsHTM
% load_colcell_from_dir - Load <CatName>_htmColCell.mat from a specific directory. Package: @catsHTM Description: Used instead of catsHTM.load_colcell when reading from a path that is not on the MATLAB path (e.g., a read-only BaseDir like /euclid/catsHTM). (Author: Dana Kovaleva, May 2026)
% load_htm_ind - load HTM data into structure from an HDF5 file Package: @catsHTM Description: load HTM data into structure from an HDF5 file
% load_multiple_cats - Load HDF5/HTM catalog from multiple files/datasets Package: @catsHTM Description: Load HDF5/HTM catalog from multiple files/datasets Not as fast as expected.
% nsrc - Count sources in the HDF5/HTM index file Package: @catsHTM Description: Count sources in the HDF5/HTM index file
% plot_density - Plot a catsHTM catalog surface density Package: @catsHTM Description: Plot a catsHTM catalog surface density in sources per deg^2 or sources per HTM on a celestial sphere map.
% project_to_colcell - Project an AstroCatalog or numeric matrix onto ColCell order. Package: @catsHTM Description: AstroCatalog/AstroTable columns matched by name; missing columns become NaN; deg->rad if NewCat.ColUnits says 'deg'. (Author: Dana Kovaleva, May 2026)
% read_colnames - read HDF5 catalog column names from index file Package: @catsHTM
% reference - Get references for an HDF5/HTM catalog Package: @catsHTM Description: Get references for an HDF5/HTM catalog
% removeSource - Remove sources from a catsHTM catalog (read-only source). Package: @catsHTM Description: Remove sources from an existing catsHTM catalog by cone match. The source catalog at BaseDir is read but never modified; modified files are written under OutDir. (Author: Dana Kovaleva, May 2026)
% removeColumn - Remove a column from every HTM cell of a catsHTM catalog. Package: @catsHTM Description: Drops a column (matched by name) from every htm_<id> dataset and updates ColCell .mat. Refuses to drop RA/Dec/SortCol; auto-shifts SortCol when columns to its left are removed. (Author: Dana Kovaleva, May 2026)
% resolve_cat_paths - Resolve BaseDir and catalog subdir for a catsHTM catalog. Package: @catsHTM Description: Falls back to ASTROPACK_CATSHTM_PATH then '/euclid/catsHTM' for BaseDir, and to catsHTM.catalogs for CatRelDir. (Author: Dana Kovaleva, May 2026)
% save_cat - save catalog data in HDF5 file Package: @catsHTM Description: save catalog data in HDF5 file Given a matrix containing a catalog, save the data in an HDF5 file. The data will be saved
% save_cat_colcell - Save ColCell cell array of an HTM catalog Package: @catsHTM
% save_htm_ind - Save HTM indinces of the celestial sphere in an HDF5 file Package: @catsHTM Description: Generate HDF5 file with HTM indices. The HTM indices contains the HTM tree and the 3 poles of the 3 great circles that defines each
% search_htm_ind - A coordinate cone search in an HTM stored in HDF5 file. Package: @catsHTM Description: A coordinate cone search in an HTM stored in HDF5 file. See also: celestial.htm.htm_search_cone
% serial_search - Execute a function on entire HDF5/HTM catalog Package: @catsHTM Description: Execute a function on entire HDF5/HTM catalog. This can be used for selection of sources based on any parameters.
% serial_search_x - Execute a function on entire HDF5/HTM catalog Package: @catsHTM Description: Execute a function on entire HDF5/HTM catalog. This can be used for selection of sources based on any parameters.
% sourcePointer - Stable per-source storage pointer (HTM cell id + row-in-cell) in a catsHTM catalog. Package: @catsHTM Description: coords [rad] -> [CellID,RowInCell,Dist,CatRowID]; unique-within-catalog, query-independent, version-bound. (Author: Dana Kovaleva, 2026)
% sources_match - Match sources in an input catalog with catsHTM catalog Package: @catsHTM Description: Given a catalog of sources with their RA/Dec, match each one of them to a source in an catsHTM catalog.
% xmatch_2cats - Cross match two HDF5/HTM catalogs Package: @catsHTM Description: Cross match two HDF5/HTM catalogs. For each source in the first catalog the index of the nearest source, within some distance, in the
% #/functions (autogen)
%
% ------------------------------------------------------------------------
% Source pointers & cross-identification  (Dana Kovaleva, 2026)
% ------------------------------------------------------------------------
% A "pointer" is a source's intrinsic STORAGE ADDRESS in a catsHTM catalog:
% the HTM leaf-cell id plus the row within that cell's dataset. Unlike a
% cone_search row index (query-relative) or the native SourceID columns
% (unreliable - every column is stored as double, so large int ids are
% corrupted), the pointer is unique within the catalog and stable for a
% given catalog BUILD/version.
%
%   sourcePointer(Cat,RA,Dec) -> [CellID,RowInCell,Dist,CatRowID]
%       coords [radians] -> storage pointer (+ optional scalar as 4th out).
%   catRowID(Cat,CellID,RowInCell) -> [CatRowID,Offset]
%       collapse the pointer pair into ONE contiguous per-catalog scalar
%       (CatRowID = block-start(CellID) + RowInCell). Version-bound.
%   catRowID2Pointer(Cat,CatRowID) -> [CellID,RowInCell]
%       the exact inverse of catRowID.
%   gatherByPointer(Cat,CellID,RowInCell) -> [Data,ColNames]
%       read the actual catalog rows at those pointers (grouped by cell,
%       each htm_<id> loaded once); any column, no cone_search needed.
%   getNsrcMeta(Cat) -> [CellID,Nsrc]
%       per-cell source counts (the offset table catRowID/catRowID2Pointer
%       share); metadata-only and location-independent.
%
% Because a pointer is bound to one catalog BUILD, persisted pointers must be
% validated after the catalog may have changed (re-ingest, add/removeSource,
% insertColumns):
%   catalogSignature(Cat) -> Sig
%       a cheap version fingerprint: LayoutHash (per-cell row counts, read
%       from the index only) + ColHash (columns) + optional ChecksumHash
%       (deployment checksum list). crossIDCatsHTM stamps one per catalog
%       into Summary.Signature.
%   checkCatalogSignature(Cat,Sig) -> [Ok,Report]
%       compare a stored Sig against the catalog now and classify: valid /
%       columns-changed (row pointers still OK) / stale-layout (pointers
%       INVALID) / stale-suspect. gatherByPointer and gatherCrossIDData
%       take a 'Signature' and refuse to dereference a stale-layout catalog.
%
%   crossIDCatsHTM(RA,Dec,Radius,...) -> [T, Cats_cone, Summary]
%       Cross-identification index over a field: cone_search an anchor
%       catalog (default GAIADR3) vs all/selected catsHTM catalogs,
%       cross-match to a growing master (union) list, append orphans, and
%       stamp Ind_/Nmatch_/Dist_ + the stable CellID_/RowInCell_ pointer
%       (opt-in CatRowID_ scalar; IdExtras for multiple matches) per cat.
%   gatherCrossIDData(T[,Cats_cone],...) -> Data
%       Materialize the index T into a per-source DATA table. Source='cats'
%       reads the Cats_cone snapshot via Ind_; Source='pointer' reads live
%       from catsHTM via CellID_/RowInCell_ (any column, no snapshot).
%
% Example (field at RA=0, Dec=0 [rad], 600 arcsec radius):
%   [T,Cats_cone,S] = catsHTM.crossIDCatsHTM(0,0,600,'CatList',{'PS1'});
%   D = catsHTM.gatherCrossIDData(T,Cats_cone);              % from snapshot
%   D = catsHTM.gatherCrossIDData(T,[],'Source','pointer', ...
%           'Columns',struct('PS1',{{'gPSFMag'}}), ...
%           'Signature',S.Signature);                        % live + validated
%   % a stable, cross-session handle to a source, then re-fetch its row:
%   [cid,row] = catsHTM.sourcePointer('PS1', RA, Dec);
%   srcRow    = catsHTM.gatherByPointer('PS1', cid, row);
%   % later, verify the catalog has not changed under a stored pointer:
%   [ok,rep]  = catsHTM.checkCatalogSignature('PS1', S.Signature.PS1);
%

classdef catsHTM
             
    
    % file and variable names
    methods (Static)
        function CatName=filename2base(FileName)
            % Convert HDF5/HTM file name to catalog name (file base name)
            % Package: @catsHTM
            % Description: Convert HDF5/HTM file name (e.g., 'APASS_htm.hdf5')
            %              to catalog name (file base name; e.g., 'APASS').
            % Input  : - HDF5 file name that contains the catalog name.
            %            The file name is composed of strings seperated by
            %            "_", where the first string is the catalog name.
            % Output : - Catalog name.
            % Example: CatName=catsHTM.filename2base('SDSSDR10_htm.hdf5')
            % Reliable:
            
             Tmp = regexp(FileName,'_','split');
             CatName = Tmp{1};
             
        end
        
        function [FileName,DataName]=get_file_var_from_htmid(FileBase,ID,NfilesInHDF)
            % Construct file and var name for HTM file stored in HDF5
            % Package: @catsHTM
            % Description: Given a file base (e.g., 'UCAC4') and HTM ID
            %              and number of files in HDF5 file, construct the
            %              HDF5 file name (e.g., UCAC4_htm_032400.hdf5),
            %              and the data variable name (e.g., htm_032412).
            % Input  : - Catalog base name (e.g., 'UCAC4').
            %          - HTM index.
            %          - Number of variables in file (default is 100).
            % Output : - File name.
            %            If ID is a vector then this is a cell array.
            %          - Variable name.
            %            If ID is a vector then this is a cell array.
            % Example: [FileName,DataName]=catsHTM.get_file_var_from_htmid('UCAC4',45661,100)
            % Reliable: 2


            if (nargin<3)
                NfilesInHDF = 100;
            end

            FileID    = floor(ID./NfilesInHDF).*NfilesInHDF;
            Nid       = numel(FileID);
            if (Nid==1)
                FileName  = sprintf('%s_htm_%06d.hdf5',FileBase,FileID);
                DataName  = sprintf('htm_%06d',ID);
            else
                FileName = cell(Nid,1);
                DataName = cell(Nid,1);
                for Iid=1:1:Nid
                    FileName{Iid}  = sprintf('%s_htm_%06d.hdf5',FileBase,FileID(Iid));
                    DataName{Iid}  = sprintf('htm_%06d',ID(Iid));
                end
            end
        end
       
        function [IndexFileName,IndexVarName]=get_index_filename(CatName)
            % Get HDF5/HTM index file name and variable name from CatName
            % Package: @catsHTM
            % Description: Get HDF5/HTM index file name and variable name
            %              from CatName.
            % Input  : - Catalog name (e.g., 'APASS').
            % Output : - File name (e.g., 'APASS_htm.hdf5')
            %          - Variable name (e.g., 'APASS_HTM')
            % Example: [IndexFileName,IndexVarName]=catsHTM.get_index_filename('PS1')
            % Reliable: 2
            
            IndexFileName = sprintf('%s_htm.hdf5',CatName);
            IndexVarName = sprintf('%s_HTM',CatName);
            
        end
        
        
        
    end
    
    % Create/save data into HDF5/HTM related files
    methods (Static)
           
        function save_cat(FileName,VarName,Data,SortCol,StepRows)
            % save catalog data in HDF5 file
            % Package: @catsHTM
            % Description: save catalog data in HDF5 file
            %              Given a matrix containing a catalog, save the
            %              data in an HDF5 file. The data will be saved
            %              under two variable names in the HDF5 file.
            %              /<base>_Cat will contain the catalog, while
            %              /<base>_Ind will contain an index data.
            %              The index data contains two columns [Ind Val],
            %              where Val is the values of the sorted column
            %              at the line index specified by Ind. Ind are in
            %              steps given by the StepRows parameter.
            % Input  : - File name
            %          - Base variable name.
            %            The actual name will be <base> and <base>_Ind.
            %          - Matrix containing the data to save
            %          - Column index by which to sort the catalog.
            %          - Number of rows step by for which to save the index
            %            data. Default is 30.
            % Outout : null
            % Example: catsHTM.save_cat('try_cat.hdf5','V',Data,2,1000);
            % Reliable: 2
            
            if (nargin<5)
                StepRows = 30;
            end
            
            % prep Data
            Data  = sortrows(Data,SortCol);
            Nrows = size(Data,1);
            VecInd       = [(1:StepRows:Nrows), Nrows]';
            VecSortedCol = Data(VecInd,SortCol);
            IndexData    = [VecInd, VecSortedCol];
            
            % save index data
            VarNameInd = sprintf('/%s_Ind',VarName);
            HDF5.save(IndexData,FileName,VarNameInd);
            
            % save catalog
            SizeData = size(Data);
            Attrib = {'NCOL',SizeData(2); 'NROW',SizeData(1)};
            
            HDF5.save(Data,FileName,sprintf('/%s',VarName),Attrib);
            
        end
        
        function save_htm_ind(HTM,FileName,VarName,Attrib,Nsrc)
            % Save HTM indinces of the celestial sphere in an HDF5 file
            % Package: @catsHTM
            % Description: Generate HDF5 file with HTM indices.
            %              The HTM indices contains the HTM tree and the 3
            %              poles of the 3 great circles that defines each
            %              HTM.
            % Input  : - Either a structure of HTM to save (created by
            %            celestial.htm.htm_build), or the HTM level.
            %          - HDF5 File name in which to store the HTM indices.
            %          - Variable name in which to store the data.
            %            Default is '<CatName>_HTM'
            %          - Cell array of attribute {Key,Val} to save
            %            in a 'ColCell' variable name.
            %          - Nsrc matrix [IndHTM Nsrc]
            % Output : null
            % Example: catsHTM.save_htm_ind(7,'try_htm.hdf5',[],{},Nsrc)
            % Reliable: 2
            
            Tmp = regexp(FileName,'_','split');
            Def.HTM = sprintf('%s_HTM',Tmp{1});
            
            if (nargin<3)
                VarName = Def.HTM;
                Attrib  = {};
                Nsrc    = [];
            end
            
            if (isempty(VarName))
                VarName = Def.HTM;
            end
            
            if (isnumeric(HTM))
                % generate HTM index
                [HTM]=celestial.htm.htm_build(HTM);
            end
            
            Nhtm = numel(HTM);
            
            Data = nan(Nhtm,13);
            for Ihtm=1:1:Nhtm
                Nlev = numel(HTM(Ihtm).id);
                ID   = sum(logspace(1,Nlev,Nlev).*HTM(Ihtm).id)./10;
                % Level, Father, Son1, Son2, Son3, Son4, Poles 1 long,
                % poles 1 lat, ..., Nsrc
                if isempty(HTM(Ihtm).father)
                    Father = NaN;
                else
                    Father = HTM(Ihtm).father;
                end
                if (isempty(HTM(Ihtm).son))
                    Son = [NaN NaN NaN NaN];
                else
                    Son = HTM(Ihtm).son;
                end
                
                if (isempty(Nsrc))
                    Ns = NaN;
                else
                    Ns = Nsrc(Nsrc(:,1)==Ihtm,2);
                    if (isempty(Ns))
                        Ns = NaN;
                    end
                end
                Data(Ihtm,:) = [HTM(Ihtm).level, Father, Son, HTM(Ihtm).PolesCoo(1,:), HTM(Ihtm).PolesCoo(2,:), HTM(Ihtm).PolesCoo(3,:), Ns];
            end
            
            % save HTM
            AttribHTM = {'Table.Col.1','Level'; ...
                      'Table.Col.2','Father'; ...
                      'Table.Col.3','Son1'; ...
                      'Table.Col.4','Son2'; ...
                      'Table.Col.5','Son3'; ...
                      'Table.Col.6','Son4'; ...
                      'Table.Col.7', 'Poles1Lon';...
                      'Table.Col.8', 'Poles1Lat';...
                      'Table.Col.9', 'Poles2Lon';...
                      'Table.Col.10','Poles2Lat';...
                      'Table.Col.11','Poles3Lon';...
                      'Table.Col.12','Poles3Lat';...
                      'Table.Col.13','Nsrc'};
            HDF5.save(single(Data),FileName,VarName,AttribHTM);
            % save column names
            HDF5.save([],FileName,'ColNames',Attrib);
                
        end
        
        function save_cat_colcell(CatName,ColCell,ColUnits)
            % Save ColCell cell array of an HTM catalog
            % Package: @catsHTM
            % Input  : - Catalog name (e.g., 'APASS')
            %          - ColCell cell array
            %          - ColUnits cell array
            % Reliable : 2
            
            if (nargin<3)
                ColUnits = {};
            end
            FileName = sprintf('%s_htmColCell.mat',CatName);
            save(FileName,'ColCell','ColUnits')
            
            
        end
        
        function count_edge_in_cat(CatName,SearchRadius,NfilesInHDF)
            %
            % Example: catsHTM.count_edge_in_cat('APASS');
            RAD = 180./pi;
            
            if (nargin<3)
                NfilesInHDF = 100;
                if (nargin<2)
                    SearchRadius = 3./3600./RAD;
                end
            end
            
            % load HTM index
            [IndexFileName,IndexVarName]=catsHTM.get_index_filename(CatName);
            [~,DataHTM] = catsHTM.load_htm_ind(IndexFileName,IndexVarName);
            Level=celestial.htm.nhtm2level(size(DataHTM,1));
            HTM = celestial.htm.htm_build(Level);
            
            % for each HTM that contain sources
            Ihtm = find(DataHTM(:,13)>0);
            Nh = numel(Ihtm);
            for Ih=1:1:Nh
                tic;
                I = Ihtm(Ih);
                
                % load catalog of HTM
                Cat = catsHTM.load_cat(CatName,I);
                Nsrc = size(Cat,1);
                % Search for all sources in HTM tile that are near the
                % edges.
                FlagEdge = false(Nsrc,1);
                for Isrc=1:1:Nsrc
                    FlagEdge(Isrc) = numel(celestial.htm.htm_search_cone(HTM,Cat(Isrc,1),Cat(Isrc,2),SearchRadius))>1;
                end
                toc
                sum(FlagEdge)
                
            end
            
        end
        
        function generate_edge_cat(CatName,SearchRadius,NfilesInHDF)
            % OBSOLOTE
            
            RAD = 180./pi;
            if (nargin<3)
                NfilesInHDF = 100;
                if (nargin<2)
                    SearchRadius = 5./3600./RAD;
                end
            end
            
            % load HTM index
            [IndexFileName,IndexVarName]=catsHTM.get_index_filename(CatName);
            [~,DataHTM] = catsHTM.load_htm_ind(IndexFileName,IndexVarName);
            Level=celestial.htm.nhtm2level(size(DataHTM,1));
            HTM = celestial.htm.htm_build(Level);
            
            % for each HTM that contain sources
            Ihtm = find(DataHTM(:,13)>0);
            Nh = numel(Ihtm);
            for Ih=1:1:Nh
                I = Ihtm(Ih);
                
                % search for all HTMs in Cat2 that may opverlap with
                % Cat1 current HTM
                MeanRA  = mean(HTM(I).coo(:,1));
                MeanDec = mean(HTM(I).coo(:,2));

                D = celestial.coo.sphere_dist_fast(MeanRA,MeanDec,HTM(I).coo(:,1),HTM(I).coo(:,2));
                CircRadius = max(D) + SearchRadius; % [rad]

                ID2 = celestial.htm.htm_search_cone(HTM,MeanRA,MeanDec,CircRadius);

                % load all ID2 from HTM2
                Nid2 = numel(ID2);
                for Iid2=1:1:Nid2
                    if (Iid2==1)
                        Cat   = catsHTM.load_cat(CatName,ID2(Iid2));
                        N2     = size(Cat,1);
                    else
                        Cat   = [Cat; catsHTM.load_cat(CatName,ID2(Iid2))];
                        N2     = size(Cat,1);
                    end
                end

                % search for sources in edge of HTM
                FlagInHTM   = celestial.htm.in_polysphere(Cat(:,1:2),HTM(I).coo);
                FlagNearHTM = celestial.htm.cone_in_polysphere(HTM(I).PolesCoo(:,1),HTM(I).PolesCoo(:,2),Cat(:,1),Cat(:,2),SearchRadius);
                FlagEdge    = ~FlagInHTM(:) & FlagNearHTM(:);
                %sum(FlagEdge)
                % store the sources
                [FileName,DataName]=catsHTM.get_file_var_from_htmid(CatName,I,NfilesInHDF);
                HDF5.save(Cat(FlagEdge,:),FileName,sprintf('/htm_%06d_Edge',I));

            end
        end

        function Status = delete_dataset(FileName, DatasetName)
            % Delete a dataset from an HDF5 file using H5L.delete (in place).
            % Package: @catsHTM
            % Description: Unlink a dataset from an HDF5 file. The HDF5
            %              file does not reclaim freed space, but the
            %              dataset name becomes available for re-creation
            %              via H5D.create / HDF5.save. No-op if the dataset
            %              does not exist.
            % Input  : - HDF5 file name.
            %          - Dataset name. Leading '/' is added if missing.
            % Output : - true if the dataset existed and was deleted,
            %            false if it did not exist.
            % Author : Dana Kovaleva (May 2026)
            % Example: catsHTM.delete_dataset('ForcedPhotList_htm_001200.hdf5','/htm_001234');
            % Reliable: 2

            if ~startsWith(DatasetName, '/')
                DatasetName = ['/' DatasetName];
            end

            % Probe existence without throwing on missing dataset
            Exists = false;
            try
                Info = h5info(FileName, DatasetName); %#ok<NASGU>
                Exists = true;
            catch
                Exists = false;
            end

            if ~Exists
                Status = false;
                return;
            end

            FID = H5F.open(FileName, 'H5F_ACC_RDWR', 'H5P_DEFAULT');
            CleanupFid = onCleanup(@() H5F.close(FID));
            H5L.delete(FID, DatasetName, 'H5P_DEFAULT');
            Status = true;
        end


        function Data=catalogs
            % List of catsHTM catalogs
            % Example: Data = catsHTM.catalogs
           
            FileSep = filesep;
            I = 0;
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/2MASS/';
            Data(I).Name = 'TMASS';
            Data(I).Desc = '2MASS catalog';
            Data(I).Ref  = 'Cutri et al. 2003';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2003yCat.2246....0C/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/2MASSxsc/';
            Data(I).Name = 'TMASSxsc';
            Data(I).Desc = '2MASS extended source catalog';
            Data(I).Ref  = 'Cutri et al. 2003';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2003yCat.2246....0C/abstract';
            
            I = I + 1;
            Data(I).Status  = false;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/AAVSO_VSX/';
            Data(I).Name = 'AAVSO_VSX';
            Data(I).Desc = 'AAVSO Variable stars index';
            Data(I).Ref  = 'Watson et al. 2006';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2006SASS...25...47W/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/AKARI/';
            Data(I).Name = 'AKARI';
            Data(I).Desc = 'AKARI mid IR all-sky catalog';
            Data(I).Ref  = 'Ishihara et al. 2010';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2010A%26A...514A...1I/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/APASS/';
            Data(I).Name = 'APASS';
            Data(I).Desc = 'AAVSO Photometric All Sky Survey (APASS) DR9';
            Data(I).Ref  = 'Henden et al. 2015';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2015AAS...22533616H/abstract';
            
            I = I + 1;
            Data(I).Status  = false;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/CRTS_per_var/';
            Data(I).Name = 'CRTS_per_var';
            Data(I).Desc = 'CRTS periodic variable star catalog';
            Data(I).Ref  = 'Drake et al. 2014';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2014ApJS..213....9D/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/Cosmos/';
            Data(I).Name = 'Cosmos';
            Data(I).Desc = 'COSMOS multi band photometry';
            Data(I).Ref  = 'Capak et al. 2007';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2007ApJS..172...99C/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/DECaLS/DR5/';
            Data(I).Name = 'DECaLS';
            Data(I).Desc = 'The Dark Energy Camera Legacy Survey (DECaLS)';
            Data(I).Ref  = 'Dey et al. 2019';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2019AJ....157..168D/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/DECaLS/DR10/';
            Data(I).Name = 'DECaLS10';
            Data(I).Desc = 'The DESI Legacy Imaging Surveys Data Release 10';
            Data(I).Ref  = 'Dey et al. 2019';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2019AJ....157..168D/abstract';
            
            I = I + 1;
            Data(I).Status   = true;
            Data(I).iscatsHTM = true;
            Data(I).Dir      = '/DESI/dr1/';
            Data(I).Name     = 'DESIdr1zpix';
            Data(I).Desc     = 'DESI DR1 zpix with non-NaN redshift';
            Data(I).Ref      = 'DESI Collaboration 2025 (DR1)';
            Data(I).RefLink  = 'https://ui.adsabs.harvard.edu/abs/2026AJ....171..285D/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/FIRST/';
            Data(I).Name = 'FIRST';
            Data(I).Desc = 'The FIRST 21cm radio survey catalog';
            Data(I).Ref  = 'Helfand et al. 2015';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2015ApJ...801...26H/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/ForcedPhotList/';
            Data(I).Name = 'ForcedPhotList';
            Data(I).Desc = 'A merge of GAIA WD catalog + AGN + CVs from SIMBAD';
            Data(I).Ref  = '';
            Data(I).RefLink = '';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/GAIA/DR1/';
            Data(I).Name = 'GAIADR1';
            Data(I).Desc = 'GAIA-DR1 catalog';
            Data(I).Ref  = 'Gaia collaboration et al. 2016';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2016A%26A...595A...1G/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/GAIA/DR2/';
            Data(I).Name = 'GAIADR2';
            Data(I).Desc = 'GAIA-DR2 catalog';
            Data(I).Ref  = 'Gaia collaboration et al. 2018';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2018A%26A...616A...1G/abstract';
            
            % I = I + 1;
            % Data(I).Status  = true;
            % Data(I).iscatsHTM  = true;
            % Data(I).Dir  = '/GAIA/DR2_19/';
            % Data(I).Name = 'GAIADR2_19';
            % Data(I).Desc = 'GAIA-DR2 catalog / with sources brighter than 19 / slim';
            % Data(I).Ref  = 'Gaia collaboration et al. 2018';
            % Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2018A%26A...616A...1G/abstract';

            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/GAIA/DRE3/';
            Data(I).Name = 'GAIAEDR3';
            Data(I).Desc = 'GAIA-EDR3 catalog';
            Data(I).Ref  = 'Gaia collaboration et al. 2020';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2020arXiv201201533G/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/GAIA/DR3/';
            Data(I).Name = 'GAIADR3';
            Data(I).Desc = 'GAIA-DR3 catalog';
            Data(I).Ref  = 'Gaia collaboration et al. 2022';
            Data(I).RefLink = '';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/GAIA/DR3extraGal/';
            Data(I).Name = 'GAIADR3extraGal';
            Data(I).Desc = 'GAIA-DR3 catalog';
            Data(I).Ref  = 'Gaia collaboration et al. 2022';
            Data(I).RefLink = '';
            
            % I = I + 1;
            % Data(I).Status  = true;
            % Data(I).iscatsHTM  = true;
            % Data(I).Dir  = '/GAIA/DR3slim/';
            % Data(I).Name = 'GAIADR3slim';
            % Data(I).Desc = 'GAIA-DR3 catalog / slim version';
            % Data(I).Ref  = 'Gaia collaboration et al. 2022';
            Data(I).RefLink = '';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/GAIA/DR3spec/';
            Data(I).Name = 'GAIADR3spec';
            Data(I).Desc = 'GAIA-DR3 catalog / low resolution spectra / 30M';
            Data(I).Ref  = 'Gaia collaboration et al. 2022';
            Data(I).RefLink = '';

            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/GAIA/DR3var/';
            Data(I).Name = 'GAIADR3var';
            Data(I).Desc = 'GAIA-DR3 catalog / variable sources';
            Data(I).Ref  = 'Gaia collaboration et al. 2022';
            Data(I).RefLink = '';

            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/GALEX/DR6Plus7Old/';
            Data(I).Name = 'GALEX';
            Data(I).Desc = 'GALEX-DR6Plus7 source catalog';
            Data(I).Ref  = 'Martin et al. 2005';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2005ApJ...619L...1M/abstract';
  
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/GALEX/DR6Plus7/';
            Data(I).Name = 'GALEXAIS';
            Data(I).Desc = 'Revised catalog of GALEX Ultraviolet Sources';
            Data(I).Ref  = 'Bianchi et al. 2017';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2017ApJS..230...24B/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/GLADE/v1/';
            Data(I).Name = 'GLADEp';
            Data(I).Desc = 'GLADE galaxy catalog; http://glade.elte.hu/';
            Data(I).Ref  = 'Dalya et al. 2021';
            Data(I).RefLink = 'https://arxiv.org/abs/2110.06184';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/GLADE/plus/';
            Data(I).Name = 'GLADEplus';
            Data(I).Desc = 'GLADE+ galaxy catalog; http://glade.elte.hu/';
            Data(I).Ref  = 'Dalya et al. 2021';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2022MNRAS.514.1403D/abstract';
    
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/IPHAS/DR2/';
            Data(I).Name = 'IPHAS';
            Data(I).Desc = 'INT Photometric Hα Survey of the Northern Galactic Plane (IPHAS DR2)';
            Data(I).Ref  = 'Barentsen et al. 2014';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2014MNRAS.444.3230B/abstract';
            
            I = I + 1;
            Data(I).Status  = false;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/LAMOST/DR4/';
            Data(I).Name = 'LAMOST_DR4';
            Data(I).Desc = 'LAMOST DR4 catalog';
            Data(I).Ref  = 'Luo et al. 2018';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2018RAA..in.prep..L/abstract';
   
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/LOFAR/';
            Data(I).Name = 'LoTSS_DR3';
            Data(I).Desc = 'The LOFAR Two-metre Sky Survey: Third Data Release';
            Data(I).Ref  = 'Shimwell et al. 2026';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2026A%26A...707A.198S/abstract';
            
%             I = I + 1;
%             Data(I).Dir  = '/NED/20170328/';
%             Data(I).Name = 'NEDz';
%             Data(I).Desc = 'NED redshift catalog 28-03-2017';
%             Data(I).Ref  = 'Helou et al. 1991';
%             Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/1991ASSL..171...89H/abstract';
            

            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/MergedCat/V2/';
            Data(I).Name = 'MergedCat';
            Data(I).Desc = 'Merged catalog';
            Data(I).Ref  = 'Ofek et al. 2021';
            Data(I).RefLink = 'in prep.';

            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/NED/20260512/';
            Data(I).Name = 'NEDz';
            Data(I).Desc = 'NED objects with non-NaN redshift, 2026-05-12';
            Data(I).Ref  = 'Helou et al. 1991';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/1991ASSL..171...89H/abstract';
            
            I = I + 1;
            Data(I).Status  = false;  % ready
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/NOAO/merged/';
            Data(I).Name = 'NOAO';
            Data(I).Desc = 'NOAO-DR1 All-Sky source catalog';
            Data(I).Ref  = 'Nidever et al. 2018';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2018AJ....156..131N/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/NVSS/';
            Data(I).Name = 'NVSS';
            Data(I).Desc = 'NVSS 21cm radio source catalog';
            Data(I).Ref  = 'Condon et al. 1998';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/1998AJ....115.1693C/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/PGC/';
            Data(I).Name = 'PGC';
            Data(I).Desc = 'The HYPERLEDA catalog of galaxies';
            Data(I).Ref  = 'Paturel et al. 2003';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2003A%26A...412...45P/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/PS1/DR1/';
            Data(I).Name = 'PS1';
            Data(I).Desc = 'The Pan-STARRS DR1 catalog';
            Data(I).Ref  = 'Chambers et al. 2016';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2016arXiv161205560C/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/PS1/DR2/';
            Data(I).Name = 'PS1DR2';
            Data(I).Desc = 'The Pan-STARRS release 1 (PS1) DR2 catalog';
            Data(I).Ref  = 'Magnier et al. 2020';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2020ApJS..251....6M/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/PTFpc/';
            Data(I).Name = 'PTFpc';
            Data(I).Desc = 'The PTF photometric catalog';
            Data(I).Ref  = 'Ofek et al. 2012';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2012PASP..124..854O/abstract';
            
            % I = I + 1;
            % Data(I).Status  = true;
            % Data(I).iscatsHTM  = true;
            % Data(I).Dir  = '/PTFprocim/';
            % Data(I).Name = 'PTFprocim';
            % Data(I).Desc = 'The PTF processed images catalog';
            % Data(I).Ref  = 'Law et al. 2009';
            % Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2009PASP..121.1395L/abstract';

            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/QSO/Flesch2021/';
            Data(I).Name = 'QSO1M';
            Data(I).Desc = 'QSO 1M catalog';
            Data(I).Ref  = 'Flesch et al. 2021';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2021yCat.7290....0F/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/ROSATfsc/';
            Data(I).Name = 'ROSATfsc';
            Data(I).Desc = 'The ROSAT faint source catalog';
            Data(I).Ref  = 'Voges et al. 2010';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2000IAUC.7432....3V/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/SDSS/DR10/';
            Data(I).Name = 'SDSSDR10';
            Data(I).Desc = 'SDSS-DR10 source catalog';
            Data(I).Ref  = 'Alam et al. 2015';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2015ApJS..219...12A/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/SDSS/DR14offset/';
            Data(I).Name = 'SDSSoffset';
            Data(I).Desc = 'SDSS-DR14 source catalog with color offsets';
            Data(I).Ref  = 'Alam et al. 2015';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2015ApJS..219...12A/abstract';
            
            I = I + 1;
            Data(I).Status  = false;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/Simbad_PM200/';
            Data(I).Name = 'Simbad_PM200';
            Data(I).Desc = 'SIMBAD sources with proper motion larger than 200mas/yr';
            Data(I).Ref  = 'Wenger et al. 2000';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2000A%26AS..143....9W/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/SkyMapper/';
            Data(I).Name = 'SkyMapper';
            Data(I).Desc = 'SkyMapper DR1 catalog (to magnitude 19)';
            Data(I).Ref  = 'Wolf et al. 2018';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2018PASA...35...10W/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/SpecSDSS/DR14/';
            Data(I).Name = 'SpecSDSS';
            Data(I).Desc = 'SDSS-DR14 spectroscopic catalog';
            Data(I).Ref  = 'Abolfathi et al. 2018';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2018ApJS..235...42A/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/SpecSDSS/DR17/';
            Data(I).Name = 'SpecSDSSDR17';
            Data(I).Desc = 'SDSS-DR17 spectroscopic catalog';
            Data(I).Ref  = 'Abdurrouf et al. 2022';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2022ApJS..259...35A/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/Spitzer/IRACgc/';
            Data(I).Name = 'IRACgc';
            Data(I).Desc = 'Spitzer IRAC galactic center catalog';
            Data(I).Ref  = 'Ramírez et al 2008';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2008ApJS..175..147R/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/Spitzer/SAGE/';
            Data(I).Name = 'SAGE';
            Data(I).Desc = 'Spitzer SAGE (LMC+SMC survey) catalog';
            Data(I).Ref  = 'Meixner et al. 2006';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2006AJ....132';
            
            I = I + 1;
            Data(I).Status  = false;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/SWIREz/';
            Data(I).Name = 'SWIREz';
            Data(I).Desc = 'SWIRE photometric redshift catalog';
            Data(I).Ref  = 'Rowan-Robinson et al. 2013';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2013MNRAS.428.1958R/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/UCAC4/';
            Data(I).Name = 'UCAC4';
            Data(I).Desc = 'The UCAC-4 astrometric catalog';
            Data(I).Ref  = 'Zacharias et al. 2013';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2013AJ....145...44Z/abstract';
            
            I = I + 1;
            Data(I).Status  = false;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/UCACGAIADR2accel/';
            Data(I).Name = 'UCACGAIADR2accel';
            Data(I).Desc = 'The GAIA-DR2 UCAC-4 accelerations catalog';
            Data(I).Ref  = 'Ofek and Hallakoun';
            Data(I).RefLink = '';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/UKIDSS/DR10/';
            Data(I).Name = 'UKIDSS';
            Data(I).Desc = 'UKIDSS-DR9 Large Area Survey';
            Data(I).Ref  = 'Lawrence et al. 2007';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2007MNRAS.379.1599L/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/unWISE/';
            Data(I).Name = 'unWISE';
            Data(I).Desc = 'The unWISE catalog';
            Data(I).Ref  = 'Schlafly et al. 2019';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2019ApJS..240...30S/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/URAT1/';
            Data(I).Name = 'URAT1';
            Data(I).Desc = 'The First U.S. Naval Observatory Robotic Astrometric Telescope Catalog';
            Data(I).Ref  = 'Zacharias et al. 2015';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2015AJ....150..101Z/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/VISTA/Viking/DR2/';
            Data(I).Name = 'VISTAviking';
            Data(I).Desc = 'The VISTA Kilo-degree Infrared Galaxy (VIKING) Survey';
            Data(I).Ref  = 'Edge et al. 2013';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2013Msngr.154...32E/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/VST/ATLAS/DR3/';
            Data(I).Name = 'VSTatlas';
            Data(I).Desc = 'The VLT Survey Telescope ATLAS';
            Data(I).Ref  = 'Shanks et al. 2015';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2015MNRAS.451.4238S/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/VLASS/ep1/';
            Data(I).Name = 'VLASSep1';
            Data(I).Desc = 'The VLASS radio survey / epoch 1';
            Data(I).Ref  = 'Gordon et al. 2021';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2021yCat..22550030G/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/VST/KiDS/DR3/';
            Data(I).Name = 'VSTkids';
            Data(I).Desc = 'The first and second data releases of the Kilo-Degree Survey';
            Data(I).Ref  = 'de Jong et al. 2015';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2015A%26A...582A..62D/abstract';
            
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/WD/WDEDR3/';
            Data(I).Name = 'WDEDR3';
            Data(I).Desc = 'GAIA-EDR3 WD catalog';
            Data(I).Ref  = 'Gentile Fusillo et al. 2021';
            Data(I).RefLink = 'https://arxiv.org/abs/2106.07669';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/WD/WDEDR3v1/';
            Data(I).Name = 'WDEDR3maincat';
            Data(I).Desc = 'GAIA-EDR3 WD catalog, all columns';
            Data(I).Ref  = 'Gentile Fusillo et al. 2021';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2021MNRAS.508.3877G/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/WISE/';
            Data(I).Name = 'WISE';
            Data(I).Desc = 'The WISE IR catalog';
            Data(I).Ref  = 'Cutri et al. 2012';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2012wise.rept....1C/abstract';
            
            I = I + 1;
            Data(I).Status  = true;
            Data(I).iscatsHTM  = true;
            Data(I).Dir  = '/XMM/';
            Data(I).Name = 'XMM';
            Data(I).Desc = 'The XMM-Newton serendipitous survey (3XMM-DR7)';
            Data(I).Ref  = 'Traulsen et al. 2019';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2019A%26A...624A..77T/abstract';
            
            % I = I + 1;
            % Data(I).Status  = true;
            % Data(I).iscatsHTM  = true;
            % Data(I).Dir  = '/XMMONUV/';
            % Data(I).Name = 'XMMOMUV';
            % Data(I).Desc = 'The XMM-Newton optical UV monotor observations';
            % Data(I).Ref  = 'Page et al. 2022';
            % Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2012MNRAS.426..903P/abstract';


            I = I + 1;
            Data(I).Status  = false;  % non catsHTM
            Data(I).iscatsHTM  = false;
            Data(I).Dir  = '/ZTF/LCDR1/';
            Data(I).Name = 'ztfLCDR1';
            Data(I).Desc = 'ZTF-DR1 light curve catalog (non catsHTM)';
            Data(I).Ref  = 'Ofek et al. 2020';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2020MNRAS.499.5782O/abstract';
            
            % I = I + 1;
            % Data(I).Status  = true;
            % Data(I).iscatsHTM  = true;
            % Data(I).Dir  = '/ZTF/SrcLCDR1/';
            % Data(I).Name = 'ztfSrcLCDR1';
            % Data(I).Desc = 'ZTF-DR1 stellar variability catalog';
            % Data(I).Ref  = 'Ofek et al. 2020';
            % Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2020MNRAS.499.5782O/abstract';
            
            I = I + 1;
            Data(I).Status  = false;
            Data(I).iscatsHTM  = false;
            Data(I).Dir  = '/ZTF/ztfDR1var/';
            Data(I).Name = 'ztfDR1var';
            Data(I).Desc = 'ZTF-DR1 variable star candidates';
            Data(I).Ref  = 'Ofek et al. 2020';
            Data(I).RefLink = 'https://ui.adsabs.harvard.edu/abs/2020MNRAS.499.5782O/abstract';
            
        end
        
        function Data=create_indiv_catalog_lists4wget(BaseDir,WriteDir)
            % Create list of individual catalogs for wget including checsums
            % Input  : - Directory in which the catsHTM catalog resides
            %            (e.g., '/raid/eran/catsHTM').
            %          - Directory in which to write wget lists.
            %            Default is '' - i.e., current dir.
            % Example:
            % Data=catsHTM.create_indiv_catalog_lists4wget('/data/euler/catsHTM','/home/eran/');
            
            if (nargin<2)
                WriteDir = '';
                if nargin<1
                    BaseDir = '/raid/eran/catsHTM';
                end
            end
            
            URL  = 'https://wao-data.org/catsHTM';
            Pars = '-U Mozilla/5.0 --no-check-certificate';
            
            Data = catsHTM.catalogs;
            Nd = numel(Data);
            
            for Id=1:1:Nd
                if Data(Id).Status
                    Data(Id)
                    Dir = sprintf('%s%s',BaseDir,Data(Id).Dir);
                    PWD = pwd;
                    cd(Dir);

                    F1 = dir('*.hdf5');
                    F2 = dir('*.mat');
                    F  = [F1;F2];

                    Nf = numel(F);

                    if Data(Id).iscatsHTM
                        
                        Nsrc = nansum(catsHTM.nsrc(Data(Id).Name));
                        Data(Id).Nsrc = Nsrc(2);
                    else
                        Data(Id).Nsrc = NaN;
                    end

                    ListFileNameW = sprintf('list.euler.wget.%s',strrep(Data(Id).Dir,'/','_'));
                    ListFileNameC = sprintf('list.euler.checksum.%s',strrep(Data(Id).Dir,'/','_'));

                    FIDw = fopen(sprintf('%s%s%s',WriteDir,filesep,ListFileNameW),'w');
                    FIDc = fopen(sprintf('%s%s%s',WriteDir,filesep,ListFileNameC),'w');


                    tic;
                    for If=1:1:Nf
                        Pars1 = sprintf('%s -P .%s',Pars,Data(Id).Dir(1:end-1));
                        fprintf(FIDw,'wget %s %s%s/%s\n',Pars1,URL,Data(Id).Dir(1:end-1),F(If).name);
                        [~,Str] = system(sprintf('md5sum %s',F(If).name));
                        fprintf(FIDc,'%s',Str);
                    end
                    fclose(FIDw);
                    fclose(FIDc);

                    cd(PWD);

                    Data(Id).ListFileNameW = ListFileNameW;
                    Data(Id).ListFileNameC = ListFileNameC;

                end
            end
            
        end
        
        function catalogs_html(FileName, BaseDir)
            % generate an html table of catalogs
            % Input  : - FileName for output HTML.
            %            Default 'catsHTM_catalogs.html'.
            %          - BaseDir of catsHTM tree on disk; the function
            %            cd's into BaseDir+Data(I).Dir per catalog before
            %            calling catsHTM.nsrc (which opens its index file
            %            by bare filename from cwd). Default
            %            '/euclid/catsHTM'.
            % Example: catsHTM.catalogs_html
            %          catsHTM.catalogs_html('~/tmp/catsHTM_catalogs.html', '/euclid/catsHTM');

            if nargin<2
                BaseDir = '/euclid/catsHTM';
                if nargin<1
                    FileName = 'catsHTM_catalogs.html';
                end
            end

            Data=catsHTM.catalogs;
            Flag = [Data.Status];
            Data = Data(Flag);
            N = numel(Data);

            Text = '';
            Text = sprintf('%s <table><tr><th> Name </th> <th> Description</th> <th>wget file</th> <th>checksum</th> <th> Nsrc</th><th>Reference</th> </tr>\n',Text);
            PWD = pwd;
            for I=1:1:N
                if Data(I).iscatsHTM
                    % catsHTM.nsrc opens <Name>_htm.hdf5 by bare filename,
                    % so cd into the catalog directory first. Catalogs
                    % whose tree is missing/unreadable on disk fall back
                    % to NaN instead of killing the whole regen.
                    CatDir = fullfile(BaseDir, Data(I).Dir);
                    try
                        cd(CatDir);
                        Nsrc = catsHTM.nsrc(Data(I).Name);
                        Nsrc = nansum(Nsrc(:,2));
                    catch ME
                        fprintf('  skip %s: %s\n', Data(I).Name, ME.message);
                        Nsrc = NaN;
                    end
                    cd(PWD);
                else
                    Nsrc = NaN;
                end

                WgetFile = sprintf('list.euler.wget.%s',strrep(Data(I).Dir,'/','_'));
                ChecksumFile = sprintf('list.euler.checksum.%s',strrep(Data(I).Dir,'/','_'));

                Text = sprintf('%s \n <tr><td> %s </td>  <td> %s </td>   <td><a href="./%s">%s</a></td><td><a href="./%s">%s</a></td>    <td> %d </td> <td> <a href="%s">%s</a> </td></tr>',...
                            Text,Data(I).Name,Data(I).Desc,WgetFile,WgetFile,ChecksumFile,ChecksumFile,Nsrc,Data(I).RefLink,Data(I).Ref);
            end
            Text = sprintf('%s </table>\n',Text);
            www.html_page(FileName,{Text},'PageTitle','catsHTM list of catalogs');

            %rsync -avx catsHTM_catalogs.html eran@euler1:/var/www/html/data/catsHTM/

        end
        
        function create_catalog_lists4wget(Dir,WriteDir)
            % Create list of catalogs foe wget including checsums
            % Input  : - Directory in which the catsHTM catalog resides
            %            (e.g., '/raid/eran/catsHTM').
            %          - Directory in which to write wget lists.
            %            Default is '' - i.e., current dir.
            % Example:
            % catsHTM.create_catalog_lists4wget('/data/euler/catsHTM','/home/eran/');
           
            if (nargin<2)
                WriteDir = '';
            end
            
            URL  = 'https://astro.weizmann.ac.il/catsHTM/';
            Pars = '-U Mozilla/5.0 --no-check-certificate';
            Nc   = numel(Dir);
            
            PWD = pwd;
            cd(Dir);
            
            F1 = io.files.rdir('*.hdf5');
            F2 = io.files.rdir('*.mat');
            F  = [F1;F2];
            
            Nf = numel(F);
            FIDw = fopen(sprintf('%s%s%s',WriteDir,filesep,'list.euler.wget'),'w');
            FIDc = fopen(sprintf('%s%s%s',WriteDir,filesep,'list.euler.checksum'),'w');
            tic;
            for If=1:1:Nf
                Pars1 = sprintf('%s -P .%s',Pars,F(If).folder(Nc+1:end));
                fprintf(FIDw,'wget %s %s%s/%s\n',Pars1,URL,F(If).folder(Nc+1:end),F(If).name);
                [~,Str] = system(sprintf('md5sum %s%s%s',F(If).folder,filesep,F(If).name));
                fprintf(FIDc,'%s',Str);
            end
            fclose(FIDw);
            fclose(FIDc);
            toc
            
            cd(PWD);
            
        end
        
    end

    methods % build catsHTM catalogs
        % additional functions:
        % VO.prep.build_htm_catalog
        % VO.prep.build_PS1_htm_cat
        % VO.prep.prep_generic_htm
        % ...

        function buildCatFromTAP(Args)
            %

            arguments
                Args.HTM       = [];
                Args.HTM_Level = 7;
                Args.CatName   = [];
                
                
                Args.ColCell   = {};
                Args.ColUnits  = {};

                Args.ColRA     = 1;
                Args.ColDec    = 2;

                Args.SaveInd   = true;
                Args.NfilesInHDF = 100;
                Args.CheckExist  = true;

            end
            RAD = 180./pi;
            ARCSEC_DEG = 3600;

            RadiusHTM = (sqrt(2).*90./(2.^(Args.HTM_Level - 1)))./RAD;
            Radius    = 0.00001./(RAD.*ARCSEC_DEF);


            % build HTM
            if (~isempty(Args.HTM) && ~isempty(Args.LevelHTM))
                HTM      = InPar.HTM;
                LevelHTM = InPar.LevelHTM;
            else
                [HTM,LevelHTM] = celestial.htm.htm_build(InPar.HTM_Level);
            end

            ListIndexHTM   = LevelHTM(InPar.HTM_Level).ptr;
            Nhtm           = numel(ListIndexHTM);

            Nsrc = nan(Nhtm,2);
            for Ihtm=1:1:Nhtm
                %Ihtm
                % check if HTM mean Dec is in dec range
                IndHTM = ListIndexHTM(Ihtm);
                MeanRA  = mean(HTM(IndHTM).coo(:,1));
                MeanDec = mean(HTM(IndHTM).coo(:,2));

                % query TAP around MeanRA, MeanDec, RadiusHTM
                % CatCC is a matrix with data
                % Args.ColRA  = 1;
                % Args.ColDec = 2;

                
                % select sources in HTM polygon
                Flag  = celestial.htm.in_polysphere(CatCC(:,[Args.ColRA, Args.ColDec]),HTM(IndHTM).coo,2);
                CatCC = CatCC(Flag,:);
                % sort by dec
                CatCC = sortrows(CatCC, Args.ColDec);
                Nsrc(Ihtm,:) = [IndHTM, size(CatCC,1)];

                % save data
                if (Nsrc(Ihtm,2)>0)
                    [FileName,DataName]=HDF5.get_file_var_from_htmid(Args.CatName,IndHTM,Args.NfilesInHDF);
                    Exist = false;
                    if (Args.CheckExist)
                        try
                            In = h5info(FileName);
                            Exist = any(strcmp({In.Datasets.Name},DataName));
                        end
                    end
                    %try
                    if (~Exist)
                        HDF5.save_cat(FileName,DataName,CatCC(Flag,:),InPar.ColDec,InPar.IndStep);
                    end
                    %catch
                    %    fprintf('Failed save_cat: Ihtm=%d\n',Ihtm);
                    %end
                end
       
            end


            % save HTM index file
            if (Args.SaveInd)
                IndFileName = sprintf('%s_htm.hdf5',Args.CatName);
                delete(IndFileName);
                Nsrc=HDF5.get_nsrc(Args.CatName);
                HDF5.save_htm_ind(HTM,IndFileName,[],{},Nsrc)
            
                HDF5.save_cat_colcell(Args.CatName,Args.ColCell,Args.ColUnits);
            end


        end

    end


    
    % Load and search HDF5/HTM files
    methods (Static)
        
        function [Cat,Ind]=load_cat(FileName,VarName,SearchParValue,Ncol,NfilesInHDF)
            % Load catalog stored in an HDF5 file
            % Package: @catsHTM
            % Description: Load catalog stored in an HDF5 file. Given a
            %              a catalog in HDF5 file created by
            %              HDF5.save_cat, load the catalog. The catalog is
            %              sorted by one of the columns and it is possible
            %              to retrieve only line in some range. The search
            %              is done using the index data.
            % Input  : - HDF5 File name, or catalog name.
            %            If catalog name, then second argument must be
            %            numeric index.
            %          - Variable name from which to load the catalog,
            %            or a numeric HTM index.
            %          - A two element vector of lower and upper value.
            %            Only lines in which the sorted parameter is
            %            between the low and high value will be retrieved.
            %            If empty, retrieve all lines. Default is empty.
            %          - Number of columns in the catalog.
            %            Default is empty (will attempt to find it).
            %          - Number of HTM data matrix in each hdf5 file.
            %            Default is 100.
            % Output : - A matrix containing the catalog
            % Example: Cat=catsHTM.load_cat('APASS_htm_010000.hdf5','htm_010001',[0.1 0.101],20);
            %          Cat=catsHTM.load_cat('APASS_htm_043600.hdf5','htm_043601');
            %          Cat=catsHTM.load_cat('APASS',43601);
            % Reliable: 2
            
            Def.SearchParValue = [];
            Def.Ncol           = [];
            Def.NfilesInHDF    = 100;
            if (nargin<3)
                SearchParValue = Def.SearchParValue;
                Ncol           = Def.Ncol;
                NfilesInHDF    = Def.NfilesInHDF;
            elseif (nargin<4)
                Ncol           = Def.Ncol;
                NfilesInHDF    = Def.NfilesInHDF;
            elseif (nargin<5)
                NfilesInHDF    = Def.NfilesInHDF;
            else
                % do nothing
            end
            
            if (isnumeric(VarName))
                % assume FileName is CatName and VarName in HTM index
                [FileName,VarName] = catsHTM.get_file_var_from_htmid(FileName,VarName,NfilesInHDF);
            end
            
            VarNameStr = sprintf('/%s',VarName);
            if (isempty(SearchParValue))
                % read entire catalog [only if exist]
                Ind = 1;
                try
                    Cat = HDF5.load(FileName,VarNameStr);
                catch
                    Cat = [];
                end
            else
                % read index data first
                try
                    VarIndStr = sprintf('/%s_Ind',VarName);
                    DataInd   = HDF5.load(FileName,VarIndStr);

                    Ndi = size(DataInd,1);

                    % search the index
                    I1 = tools.find.bin_sear(DataInd(:,2),SearchParValue(1));
                    I2 = tools.find.bin_sear(DataInd(:,2),SearchParValue(2));

                    if (isempty(Ncol))
                        % get number of columns from HDF5 file attributes
                        error('Get Ncol from attributes not implemented yet');
                    end

                    % read data
                    Ind    = DataInd(I1,1);
                    Offset = [DataInd(I1,1), 1];
                    if (I1==I2)
                        I2 = I2 + 1;
                    end
                    I2 = min(I2,Ndi);

                    Block  = [1+DataInd(I2,1)-DataInd(I1,1), Ncol];
                    Cat = HDF5.load(FileName,VarNameStr,Offset,Block);
                catch
                    Ind = 1;
                    Cat = [];
                end
            end
            
        end

        function [Cat,CatID,Cat1,Ihtm,ColCell]=load_cat_with_edges(CatName,Ih,IsHTMindex,Args)
            % load catalogs from all HTMs near a specific HTM triangle.
            % Package: @catsHTM
            % Description:
            % Input  : - Catalog name
            %          - Either running index, or HTM index.
            %            Running index is a serial number starting with 1.
            %            HTM index is the index of the HTM triangle in te HTM
            %            structure.
            %            The HTM index (Ihtm) is related to the serial number (Ih) by
            %            Ihtm   = Level.ptr(Ih).
            %          - A logical indicating if the second input is HTM index.
            %            Default is true.
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            %            'HTM' - A structure of HTM generated by celestial.htm.htm_build
            %                    If empty, then Level must be provided and the HTM will
            %                    be generated.
            %            'LevelH' - A structure of Level generated by celestial.htm.htm_build
            %                    If empty, then Level must be provided and the HTM will
            %                    be generated.
            %            'Level' - Level number. Ignored if 'HTM' and 'LevelH' are
            %                    provided.
            %            'SearchRadius' - Default is 2.
            %            'SearchRadiusUnits' - Default is 'arcsec'.
            % Output : - The combined catalog of all HTMs adjuscent to the requested
            %            HTM, including the HTM itself.
            %          - A vector of the HTM index for each source in the catalog
            %            (first output argument).
            %          - The catalog of the requested HTM only.
            %          - Cell array of catalog column names.
            % Example:
            % [Cat,CatID,Cat1,Ihtm,ColCell]=catsHTM.load_cat_with_edges('FIRST',1,false,'Level',7);

            arguments
                CatName
                Ih
                IsHTMindex(1,1) logical   = true;
                Args.HTM                  = [];
                Args.LevelH               = [];
                Args.Level                = [];
                Args.SearchRadius         = 2;  % [arcsec]
                Args.SearchRadiusUnits    = 'arcsec';
            end
            
            SearchRadius = convert.angular(Args.SearchRadiusUnits,'rad',Args.SearchRadius);  % [rad]

            % load HTM data for Cat2
            [IndexFileName,IndexVarName] = catsHTM.get_index_filename(CatName);
            % HTM2 is the HTM index file
            [HTM,DataHTM] = catsHTM.load_htm_ind(IndexFileName,IndexVarName);
            % Level, Father, Son1, Son2, Son3, Son4, Poles 1 long,
            % poles 1 lat, ..., Nsrc


            if isempty(Args.HTM) && isempty(Args.LevelH)
                % generate HTM and Level
                if isempty(Args.Level)
                    error('If HTM and LevelH are not provided, Level must be provided');
                end

                [HTM,LevelH] = celestial.htm.htm_build(Args.Level);   % < from input
            else
                HTM    = Args.HTM;
                LevelH = Args.LevelH;
            end


            Nhtm = numel(HTM);

            L = celestial.htm.nhtm2level(Nhtm);

            Level = LevelH(L);

            [ColCell] = catsHTM.load_colcell(CatName);
            Ncol      = numel(ColCell);


            if IsHTMindex
                Ihtm = Ih;
            else
                Ihtm   = Level.ptr(Ih);
            end


            Cat1 = catsHTM.load_cat(CatName,Ihtm);

            % Cat1 current HTM   % deg
            MeanRA  = mean(HTM(Ihtm).coo(:,1));
            MeanDec = mean(HTM(Ihtm).coo(:,2));
            MinDec  = min(HTM(Ihtm).coo(:,2))-SearchRadius;
            MaxDec  = max(HTM(Ihtm).coo(:,2))+SearchRadius;

            %%
            %if ((MeanDec.*180./pi)>-30)

            D = celestial.coo.sphere_dist_fast(MeanRA,MeanDec,HTM(Ihtm).coo(:,1),HTM(Ihtm).coo(:,2));
            CircRadius = max(D) + SearchRadius; % [rad]

            ID = celestial.htm.htm_search_cone(HTM,MeanRA,MeanDec,CircRadius);

            % load all ID from HTM
            Nid = numel(ID);
            for Iid=1:1:Nid
                %Iid
                if (Iid==1)
                    [Cat,Ind]   = catsHTM.load_cat(CatName,ID(Iid),[MinDec MaxDec],Ncol);
                    N           = size(Cat,1);
                    CatID       = [ID(Iid).*ones(N,1), Ind-1+(1:1:N)'];
                else
                    [Cattmp, Ind] = catsHTM.load_cat(CatName,ID(Iid),[MinDec MaxDec],Ncol);
                    Cat   = [Cat; Cattmp];
                    N     = size(Cat,1);
                    CatID = [CatID; [ID(Iid).*ones(N,1), Ind-1+(1:1:N)']];
                end
            end
        end

        function Cat=load_multiple_cats(CatName,ID,NfilesInHDF)
            % Load HDF5/HTM catalog from multiple files/datasets
            % Package: @catsHTM
            % Description: Load HDF5/HTM catalog from multiple files/datasets
            %              Not as fast as expected.
            % Input  : - CatName
            %          - Vector of htm indices.
            %          - Number of datasets in HDF5 file. Default is 100.
            % Output : - Joint catalog.
            % Example: Data=catsHTM.load_multiple_cats('UCAC4',[19100:1:191002]')
            % Reliable: 2
            
            if (nargin<3)
                NfilesInHDF = 100;
            end
            
            %Nid = numel(ID);
            % get file/dataset name for all IDs
            [FileName,DataName] = catsHTM.get_file_var_from_htmid(CatName,ID,NfilesInHDF);
            
            % select unique files
            FileID = floor(ID./NfilesInHDF).*NfilesInHDF;
            UniqueFID = unique(FileID);
            Nufid = numel(UniqueFID);
            for Iufid=1:1:Nufid
                % select all IDs in file
                Ifile = find(UniqueFID(Iufid)==FileID);
                if (Iufid==1)
                    Cat = HDF5.load_muti_datasets(FileName{Ifile(1)}, DataName(Ifile));
                else
                    Cat = [Cat; HDF5.load_muti_datasets(FileName{Ifile(1)}, DataName(Ifile))];
                end
            end
            
        end
        
        function [Cat,EdgeOk]=load_cat_edge(CatName,IndHTM,NfilesInHDF)
            % Load and concat HDF5/HTM catalog and its edge catalog
            % Package: @catsHTM
            % Description: Load and concat HDF5/HTM catalog and its edge catalog
            % Input  : - Catalog base name.
            %          - HTM index.
            %          - Number of HTM datasets in file. Default is 100.
            % Output : - THe catalog.
            %          - A logical flag indicating if the Edge catalog was
            %            sucessfully uploaded.
            % Example: Cat=catsHTM.load_cat_edge('APASS',19000);
            % Reliable: 2
            
            ColDec = 2;
            if (nargin<3)
                NfilesInHDF = 100;
            end
            
            [FileName,DataName]=catsHTM.get_file_var_from_htmid(CatName,IndHTM,NfilesInHDF);
            EdgeOK = true;
            try
                Cat  = HDF5.load(FileName,sprintf('/%s',DataName));
            catch
                Cat = [];
            end
            if (~isempty(Cat))
                try
                    CatE = HDF5.load(FileName,sprintf('/%s_Edge',DataName));
                catch
                    CatE = [];
                    EdgeOK = false;
                end
            else
                CatE = [];
            end
            Cat = [Cat; CatE];
            
            if (~isempty(Cat))
                Cat = sortrows(Cat,ColDec);
            end
            
        end
        
        function [Cat,ColCell]=load_1htm(CatName,IndexHTM,NfilesInHDF)
            % Load a single tile of HDF5/HTM catalog
            % Package: @catsHTM
            % Description: Load a single HTM tile of HDF5/HTM catalog based
            %              on its HTM index.
            %              This is slower relative to catsHTM.load_cat,
            %              since it also loads the index file.
            % Input  : - Catalog name (e.g., 'APASS').
            %          - HTM index.
            %          - Number of data varaible in HDF5 file.
            %            Default is 100.
            % Output : - Catalog matrix.
            %          - Cell array of column names.
            % Example: [Cat,ColCell]=catsHTM.load_1htm('APASS',25000)
            % Reliable: 2
            
            if (nargin<3)
                NfilesInHDF = 100;
            end
            
            FileName = sprintf('%s_htm.hdf5',CatName);
            DataName = sprintf('%s_HTM',CatName);
            Data     = HDF5.load(FileName,DataName);
            
            if ~(IndexHTM>0 && IndexHTM<=size(Data,1))
                error('IndexHTM was not found in index file');
            end
            if (Data(IndexHTM,13)>0)
                [FileName,DataName]=catsHTM.get_file_var_from_htmid(CatName,IndexHTM,NfilesInHDF);
                Cat = catsHTM.load_cat(FileName,DataName);
            else
                Cat = [];
            end
            
            if (nargout>1)
                File = sprintf('%s_htmColCell.mat',CatName);
                io.files.load1(File);
            end
            
        end
        
        function [ColCell,ColUnits,Col] = load_colcell(CatName)
            % Load ColCell and ColUnits for an HDF5/HTM catalog
            % Package: @catsHTM
            % Input  : - Catalog base name (e.g., 'DECaLS').
            % Output : - Cell array of column names.
            %          - Cell array of column units
            %          - Structure with column names and indices
            % Example: [ColCell,ColUnits]=catsHTM.load_colcell('APASS')
            % Reliable: 2
            
            File = sprintf('%s_htmColCell.mat',CatName);
            io.files.load1(File);
            
            if (nargout>2)
                Col = cell2struct(num2cell(1:1:numel(ColCell)),ColCell,2)
            end
        end
        
        function [ColCell,Col]=read_colnames(FileName,VarName)
            % read HDF5 catalog column names from index file
            % Package: @catsHTM
            % Input  : - HDF5 file name.
            %          - Variable name. Default is '/ColNames'.
            % Output : - Cell array of column names.
            %          - Structure array of column indices.
            % Example: [ColCell,Col]=catsHTM.read_colnames('GAIADR1_htm.hdf5');
            
            if (nargin<2)
                VarName = '/ColNames';
            end
            
            Ncol = h5readatt('GAIADR1_htm.hdf5','/ColNames','Table.Ncol');
            ColCell = cell(1,Ncol);
            for Icol=1:1:Ncol
                ColCell{Icol} = h5readatt('GAIADR1_htm.hdf5','/ColNames',sprintf('Table.Col.%d',Icol));
                Col.(ColCell{Icol}) = Icol;
            end
        end
       
        function [HTM,Data]=load_htm_ind(FileName,VarName)
            % load HTM data into structure from an HDF5 file
            % Package: @catsHTM
            % Description: load HTM data into structure from an HDF5 file
            % Input  : - HDF5 file name containing the HTM data.
            %          - Variable name. Default is '<CatName>_HTM'.
            % Output : - A structure array containing the HTM structure.
            %          - Thr matrix containing the HTM data.
            % Example: HTM=catsHTM.load_htm_ind('try_htm.hdf5','HTM');
            % Reliable :2
            
            if (nargin<2)
                Tmp = regexp(FileName,'_','split');
                VarName = sprintf('%s_HTM',Tmp{1});
            
            end
            
            % read data from HDF5 file
            Data = HDF5.load(FileName,VarName);
            
            % load into HTM structure
            Nhtm = size(Data,1);
            HTM  = tools.struct.struct_def({'level','father','son','PolesCoo'},1,Nhtm);
            for Ihtm=1:1:Nhtm
                HTM(Ihtm).level = Data(Ihtm,1);
                %HTM(Ihtm).id    = [];
                %HTM(Ihtm).coo   = [];
                %HTM(Ihtm).cosd  = [];
                %HTM(Ihtm).center_coo = [];
                %HTM(Ihtm).center_cosd = [];
                if (isnan(Data(Ihtm,2)))
                    HTM(Ihtm).father  = [];
                else
                    HTM(Ihtm).father  = Data(Ihtm,2);
                end
                if (isnan(Data(Ihtm,3)))
                    HTM(Ihtm).son  = [];
                else
                    HTM(Ihtm).son  = Data(Ihtm,3:6);
                end
                HTM(Ihtm).PolesCoo = [Data(Ihtm,7:8); Data(Ihtm,9:10); Data(Ihtm,11:12)];
                
            end
            
        end
        
        function ID=search_htm_ind(FileName,VarName,Long,Lat,Radius)
            % A coordinate cone search in an HTM stored in HDF5 file.
            % Package: @catsHTM
            % Description: A coordinate cone search in an HTM stored in HDF5 file.
            %              See also: celestial.htm.htm_search_cone
            % Input  : - An HDF5 file name or an open HDF5 object, in which
            %            the HTM indices are stored.
            %          - Variable name. If empty, default is <CatName>_HTM.
            %          - Search longitude [radians].
            %          - Search latitude [radians].
            %          - Search radius [radians].
            % Example: ID=catsHTM.search_htm_ind('UCAC4_htm.hdf5',[],1,1,0.001)
            % Reliable: 2
            
            
            Check = true;
            if (isempty(VarName))
                Tmp = regexp(FileName,'_','split');
                VarName = sprintf('%s_HTM',Tmp{1});
            end
                     
            if (Check)
                DataHTM = HDF5.load_check(FileName,VarName);
            else
                DataHTM = HDF5.load(FileName,VarName);
            end
            
            ID=catsHTM.htm_search_cone(DataHTM,Long,Lat,Radius);
            
            % check that HTM contains sources
            ID = ID(DataHTM(ID,13)>0);

        end
       
        function ID=htm_search_cone(DataHTM,Long,Lat,Radius,Ind)
            % Search for all HTM leafs interscting a small circle (cone search)
            % Package: @catsHTM
            % Description: Search for all HTM leafs interscting a small circle
            %              (i.e., cone search).
            % Input  : - Either a table of HTM data or an open HDF5 object
            %            in which the HTM data is stored.
            %          - Longitude [radians] to search.
            %          - Latitude [radians] to search.
            %          - Search radius [radians].
            % Example:  [HTM,LevList]=celestial.htm.htm_build(4);
            %           ID=catsHTM.htm_search_cone(HTM,1,1,0.0001)
            % Reliable : 2
            
            Col.Father = 2;
            Col.Son    = [3 4 5 6];
            Col.PolesLong  = [7 9  11];
            Col.PolesLat   = [8 10 12];

            if (nargin<5)
                Ind = [];
            end


            if isempty(Ind)
                % first iteration
                Sons  = (1:1:8);
                %Nsons = 8;
            else
                Sons  = Ind;
                %Nsons = 4;
            end

            ID = [];
            Nsons = numel(Sons);
            PolesLong = zeros(3,Nsons);
            PolesLat  = zeros(3,Nsons);

            % DataHTM is the full HTM table
            for Isons=1:1:Nsons
                %CSon = Sons(Isons);
                PolesLong(:,Isons) = DataHTM(Sons(Isons),Col.PolesLong); %   HTM(Sons(Isons)).PolesCoo(:,1);
                PolesLat(:,Isons)  = DataHTM(Sons(Isons),Col.PolesLat);  % HTM(Sons(Isons)).PolesCoo(:,2);
            end
            Flag = celestial.htm.cone_in_polysphere(PolesLong,PolesLat,Long,Lat,Radius);


            for Isons=1:1:Nsons
                if (Flag(Isons))
                    % cone overlap HTM
                    CSon = Sons(Isons);
                    if isnan(DataHTM(CSon,Col.Son))
                        % arrived at last leaf
                        % return ID
                        ID = [ID, CSon];
                    else
                        Ind = DataHTM(CSon,Col.Son); % HTM(CSon).son;
                        ID  = [ID, catsHTM.htm_search_cone(DataHTM,Long,Lat,Radius,Ind)];
                        %ID = cat(2,ID,celestial.htm.htm_search_cone(HTM,Long,Lat,Radius,Ind));
                    end
                end
            end
            
        end
        
        
        function createSlimCopy(CatName, OutputPath, OutputCatName, Cols, Type)
            %
            % Example:
            % catsHTM.createSlimCopy('GAIADR3','/raid/eran/catsHTM/GAIA/DR3slim','GAIADR3slim',[1 2 3 6 8 10 18 19 27 29 31 35],@single)

            arguments
                CatName
                OutputPath
                OutputCatName
                Cols
                Type   = [];
            end

            cd(OutputPath);

            FileName = sprintf('%s_htm.hdf5',CatName);
            DataName = sprintf('%s_HTM',CatName);
            %HTM = catsHTM.load_htm_ind(FileName);
            Data = HDF5.load(FileName,DataName);

            MaxLevel = max(Data(:,1));

            IndFiles = find(Data(:,1)==MaxLevel);
            Nf = numel(IndFiles);
            for If=1:1:Nf
                IndFiles(If)

                [Cat,ColCell] = catsHTM.load_1htm(CatName,IndFiles(If));

                Cat = Cat(:,Cols);
                if ~isempty(Type)
                    Cat = Type(Cat);
                end
                [FileName,DataName] = catsHTM.get_file_var_from_htmid(OutputCatName, IndFiles(If), 100);
                catsHTM.save_cat(FileName, DataName, Cat, 2, 1000);

            end

        end


        function Result = addSource(CatName, NewCat, OutDir, Args)
            % Insert new sources into a catsHTM catalog (read-only source).
            % Package: @catsHTM
            % Description: Add sources to an existing catsHTM catalog. The
            %              source catalog at BaseDir is read but never
            %              modified. All affected data files are copied
            %              from BaseDir to OutDir, then their HTM cell
            %              datasets are rewritten with the new sources
            %              merged in. The index file is rewritten in full
            %              with updated Nsrc values.
            %
            %              Intended for catalogs on read-only mounts
            %              (e.g., /euclid/catsHTM): the modified files in
            %              OutDir must be copied back to BaseDir manually
            %              by someone with write access.
            %
            %              Useful for ForcedPhotList-style catalogs where
            %              new entries are added incrementally.
            %
            % Input  : - CatName : Catalog name (e.g., 'FIRST').
            %          - NewCat  : Either an AstroCatalog (preferred,
            %                      projected to ColCell order by name) or
            %                      a numeric matrix [N x Ncol] already in
            %                      ColCell order. RA/Dec must be in radians
            %                      (AstroCatalog with CooUnits='deg' is
            %                      converted automatically).
            %          - OutDir  : Writable directory where modified files
            %                      are placed. The catalog subdirectory
            %                      structure under BaseDir is mirrored.
            %                    * ...,key,val,... 
            %                      'BaseDir'         - Source catsHTM root, read-only.
            %                              Default: getenv('ASTROPACK_CATSHTM_PATH')
            %                              or '/euclid/catsHTM'.
            %                       'CatRelDir'       - Catalog subdirectory under
            %                              BaseDir (e.g., '/FIRST/'). If
            %                              empty, looked up from
            %                              catsHTM.catalogs by Name.
            %                       'SortCol'         - Column to sort by. Default 2.
            %                       'StepRows'        - save_cat index step. Default 30.
            %                       'NfilesInHDF'     - Cells per data file. Default 100.
            %                       'DuplicateRadius' - arcsec. >0 enables cone dedup
            %                              against existing sources.
            %                              Default 0 (no dedup).
            %                       'OnDuplicate'     - 'error'|'skip'|'replace'.
            %                              Default 'error'.
            %                       'DryRun'          - List affected files without
            %                              writing. Default false.
            %                       'Verbose'         - Print progress. Default false.
            % Output : - Result struct with fields:
            %            .OutDir          - target directory
            %            .ModifiedFiles   - cellstr of files written in OutDir
            %                               (relative to OutDir)
            %            .CellsTouched    - number of HTM cells modified
            %            .SourcesAdded    - number of new rows inserted
            %            .SourcesSkipped  - number rejected by dedup
            % Author : Dana Kovaleva (May 2026)
            % Example:
            %   AC = AstroCatalog;
            %   AC.Catalog  = [RA_rad, Dec_rad, Mag, MagErr];
            %   AC.ColNames = {'RA','Dec','Mag','MagErr'};
            %   AC.ColUnits = {'rad','rad','mag','mag'};
            %   R = catsHTM.addSource('ForcedPhotList', AC, '~/tmp/cats_mod');

            arguments
                CatName                   (1,:) char
                NewCat
                OutDir                    (1,:) char
                Args.BaseDir              (1,:) char    = ''
                Args.CatRelDir            (1,:) char    = ''
                Args.SortCol              (1,1) double  = 2
                Args.StepRows             (1,1) double  = 30
                Args.NfilesInHDF          (1,1) double  = 100
                Args.DuplicateRadius      (1,1) double  = 0
                Args.OnDuplicate          (1,:) char    {mustBeMember(Args.OnDuplicate,{'error','skip','replace'})} = 'error'
                Args.DryRun               (1,1) logical = false
                Args.Verbose              (1,1) logical = false
            end

            RAD = 180./pi;

            % --- Resolve BaseDir / CatRelDir from registry ----------
            [BaseDir, CatRelDir] = catsHTM.resolve_cat_paths(CatName, Args.BaseDir, Args.CatRelDir);
            SrcDir = fullfile(BaseDir, CatRelDir);
            DstDir = fullfile(OutDir, CatRelDir);

            if ~isfolder(SrcDir)
                error('catsHTM:addSource:NoSrcDir', ...
                    'Source catalog directory does not exist: %s', SrcDir);
            end
            if ~Args.DryRun && ~isfolder(DstDir)
                mkdir(DstDir);
            end

            % --- Project NewCat to ColCell ---------------------------
            [ColCell, ~] = catsHTM.load_colcell_from_dir(SrcDir, CatName);
            Ncol = numel(ColCell);
            NewMat = catsHTM.project_to_colcell(NewCat, ColCell);
            Nnew = size(NewMat, 1);
            if Nnew == 0
                if Args.Verbose
                    fprintf('addSource: nothing to add (NewCat is empty).\n');
                end
                Result = struct('OutDir', DstDir, 'ModifiedFiles', {{}}, ...
                    'CellsTouched', 0, 'SourcesAdded', 0, 'SourcesSkipped', 0);
                return;
            end

            % --- Load HTM index from source --------------------------
            [IndexFileName, IndexVarName] = catsHTM.get_index_filename(CatName);
            SrcIndex = fullfile(SrcDir, IndexFileName);
            if ~isfile(SrcIndex)
                error('catsHTM:addSource:NoIndex', ...
                    'Index file not found: %s', SrcIndex);
            end
            DataHTM = HDF5.load(SrcIndex, IndexVarName);
            Nhtm    = size(DataHTM, 1);
            Level   = celestial.htm.nhtm2level(Nhtm);
            HTM     = celestial.htm.htm_build(Level);

            % --- Find target HTM cell for each new source -----------
            CellPerRow = zeros(Nnew, 1);
            for Inew = 1:Nnew
                CellPerRow(Inew) = celestial.htm.htm_search_point(HTM, NewMat(Inew, [1 2]));
            end
            [UCells, ~, GroupIdx] = unique(CellPerRow);
            Ngroup = numel(UCells);

            ModifiedFiles  = {};
            DirtyFiles     = containers.Map('KeyType','char','ValueType','any');
            DirtyCells     = false(Nhtm, 1);
            NewNsrc        = DataHTM(:, 13);
            SourcesAdded   = 0;
            SourcesSkipped = 0;

            for Igroup = 1:Ngroup
                CellID = UCells(Igroup);
                Rows   = NewMat(GroupIdx == Igroup, :);

                [DataFileName, DataSetName] = ...
                    catsHTM.get_file_var_from_htmid(CatName, CellID, Args.NfilesInHDF);
                SrcFile = fullfile(SrcDir, DataFileName);
                DstFile = fullfile(DstDir, DataFileName);

                % Existing rows for this cell (if any)
                Existing = zeros(0, Ncol);
                if isfile(SrcFile)
                    try
                        Existing = HDF5.load(SrcFile, ['/' DataSetName]);
                    catch
                        Existing = zeros(0, Ncol);
                    end
                end

                % Optional duplicate check
                if Args.DuplicateRadius > 0 && ~isempty(Existing)
                    DupRadRad = Args.DuplicateRadius ./ (3600 .* RAD);
                    KeepRow = true(size(Rows,1), 1);
                    for Irow = 1:size(Rows,1)
                        D = celestial.coo.sphere_dist_fast( ...
                            Rows(Irow,1), Rows(Irow,2), ...
                            Existing(:,1), Existing(:,2));
                        IsDup = any(D <= DupRadRad);
                        if IsDup
                            switch Args.OnDuplicate
                                case 'error'
                                    error('catsHTM:addSource:Duplicate', ...
                                        ['Source at RA=%g, Dec=%g (rad) matches an ', ...
                                         'existing row in HTM cell %d within %g arcsec.'], ...
                                        Rows(Irow,1), Rows(Irow,2), CellID, Args.DuplicateRadius);
                                case 'skip'
                                    KeepRow(Irow) = false;
                                    SourcesSkipped = SourcesSkipped + 1;
                                case 'replace'
                                    Existing(D <= DupRadRad, :) = [];
                            end
                        end
                    end
                    Rows = Rows(KeepRow, :);
                end

                if isempty(Rows)
                    continue;
                end

                Combined = [Existing; Rows];
                Combined = sortrows(Combined, Args.SortCol);

                if Args.DryRun
                    DirtyFiles(DstFile) = true;
                    DirtyCells(CellID)  = true;
                    NewNsrc(CellID)     = size(Combined, 1);
                    SourcesAdded        = SourcesAdded + size(Rows, 1);
                    if Args.Verbose
                        fprintf('  [dry-run] cell %d: %d -> %d rows in %s\n', ...
                            CellID, size(Existing,1), size(Combined,1), DataFileName);
                    end
                    continue;
                end

                % Copy data file from BaseDir on first touch (preserves
                % the other 99 cells in the same file)
                if ~DirtyFiles.isKey(DstFile)
                    if isfile(SrcFile)
                        copyfile(SrcFile, DstFile);
                    end
                    DirtyFiles(DstFile) = true;
                end

                % Replace dataset and its companion _Ind in the local copy
                catsHTM.delete_dataset(DstFile, ['/' DataSetName]);
                catsHTM.delete_dataset(DstFile, ['/' DataSetName '_Ind']);
                catsHTM.save_cat(DstFile, DataSetName, Combined, Args.SortCol, Args.StepRows);

                DirtyCells(CellID) = true;
                NewNsrc(CellID)    = size(Combined, 1);
                SourcesAdded       = SourcesAdded + size(Rows, 1);

                if Args.Verbose
                    fprintf('  cell %d: %d -> %d rows in %s\n', ...
                        CellID, size(Existing,1), size(Combined,1), DataFileName);
                end
            end

            % --- Rewrite index file in OutDir with updated Nsrc -----
            DstIndex = fullfile(DstDir, IndexFileName);
            if any(DirtyCells)
                if ~Args.DryRun
                    if isfile(DstIndex)
                        delete(DstIndex);
                    end
                    DataHTM(:, 13) = NewNsrc;
                    HDF5.save(single(DataHTM), DstIndex, IndexVarName);
                end
                DirtyFiles(DstIndex) = true;
            end

            % --- Build result struct ---------------------------------
            FilesAbs = DirtyFiles.keys;
            ModifiedFiles = cell(numel(FilesAbs), 1);
            for If = 1:numel(FilesAbs)
                ModifiedFiles{If} = strrep(FilesAbs{If}, [OutDir filesep], '');
            end

            Result = struct( ...
                'OutDir',         OutDir, ...
                'ModifiedFiles',  {ModifiedFiles}, ...
                'CellsTouched',   sum(DirtyCells), ...
                'SourcesAdded',   SourcesAdded, ...
                'SourcesSkipped', SourcesSkipped);

            if Args.Verbose
                fprintf('addSource: %d source(s) added across %d cell(s)%s\n', ...
                    SourcesAdded, sum(DirtyCells), ...
                    repmat(' (dry-run)', 1, double(Args.DryRun)));
                fprintf('  Files written under %s:\n', OutDir);
                for If = 1:numel(ModifiedFiles)
                    fprintf('    %s\n', ModifiedFiles{If});
                end
            end
        end


        function Result = removeSource(CatName, RA, Dec, OutDir, Args)
            % Remove sources from a catsHTM catalog (read-only source).
            % Package: @catsHTM
            % Description: Remove sources from an existing catsHTM catalog
            %              by cone match. The source catalog at BaseDir is
            %              read but never modified. All affected data files
            %              are copied from BaseDir to OutDir and their HTM
            %              cell datasets rewritten without the matched
            %              rows. The index file is rewritten in full with
            %              updated Nsrc values. Cells that become empty
            %              have their dataset deleted (no zero-row dataset
            %              left behind); search_htm_ind already filters by
            %              Nsrc>0 so cone_search will skip them.
            %
            %              Intended for catalogs on read-only mounts: the
            %              modified files in OutDir must be copied back to
            %              BaseDir manually.
            %
            % Input  : - CatName : Catalog name.
            %          - RA      : Vector of right ascensions, radians,
            %                      sexagesimal string, or [H M S].
            %          - Dec     : Vector of declinations, radians,
            %                      sexagesimal string, or [sign D M S].
            %          - OutDir  : Writable directory mirroring BaseDir.
            %                    * ...,key,val,...
            %                      'BaseDir'         - Source catsHTM root.
            %                              Default ASTROPACK_CATSHTM_PATH
            %                              or '/euclid/catsHTM'.
            %                       'CatRelDir'       - Catalog subdir under BaseDir.
            %                              Default looked up from registry.
            %                       'SearchRadius'    - Match radius. Default 1.
            %                       'RadiusUnits'     - Default 'arcsec'.
            %                       'SortCol'         - Default 2.
            %                       'StepRows'        - Default 30.
            %                       'NfilesInHDF'     - Default 100.
            %                       'OnMultiMatch'    - 'error'|'first'|'all'.
            %                              Default 'error'.
            %                       'OnNoMatch'       - 'error'|'warn'|'silent'.
            %                              Default 'warn'.
            %                       'DryRun'          - List affected files, no writes.
            %                              Default false.
            %                       'Verbose'         - Print progress. Default false.
            % Output : - Result struct with fields:
            %            .OutDir          - target directory
            %            .ModifiedFiles   - cellstr of files written
            %            .CellsTouched    - HTM cells modified
            %            .SourcesRemoved  - number of rows dropped
            %            .NotFound        - logical vector, true where the
            %                               input position had no match
            % Author : Dana Kovaleva (May 2026)
            % Example:
            %   R = catsHTM.removeSource('ForcedPhotList', RA_rad, Dec_rad, '~/tmp/cats_mod');

            arguments
                CatName                   (1,:) char
                RA
                Dec
                OutDir                    (1,:) char
                Args.BaseDir              (1,:) char    = ''
                Args.CatRelDir            (1,:) char    = ''
                Args.SearchRadius         (1,1) double  = 1
                Args.RadiusUnits          (1,:) char    = 'arcsec'
                Args.SortCol              (1,1) double  = 2
                Args.StepRows             (1,1) double  = 30
                Args.NfilesInHDF          (1,1) double  = 100
                Args.OnMultiMatch         (1,:) char    {mustBeMember(Args.OnMultiMatch,{'error','first','all'})}  = 'error'
                Args.OnNoMatch            (1,:) char    {mustBeMember(Args.OnNoMatch,{'error','warn','silent'})}    = 'warn'
                Args.DryRun               (1,1) logical = false
                Args.Verbose              (1,1) logical = false
            end

            % --- Resolve paths ---------------------------------------
            [BaseDir, CatRelDir] = catsHTM.resolve_cat_paths(CatName, Args.BaseDir, Args.CatRelDir);
            SrcDir = fullfile(BaseDir, CatRelDir);
            DstDir = fullfile(OutDir, CatRelDir);
            if ~isfolder(SrcDir)
                error('catsHTM:removeSource:NoSrcDir', ...
                    'Source catalog directory does not exist: %s', SrcDir);
            end
            if ~Args.DryRun && ~isfolder(DstDir)
                mkdir(DstDir);
            end

            % --- Coerce RA/Dec to radian column vectors --------------
            if ischar(RA) || isstring(RA)
                RA = celestial.coo.convertdms(RA, 'SH', 'r');
            end
            if ischar(Dec) || isstring(Dec)
                Dec = celestial.coo.convertdms(Dec, 'SD', 'R');
            end
            RA  = RA(:);
            Dec = Dec(:);
            if numel(RA) ~= numel(Dec)
                error('catsHTM:removeSource:SizeMismatch', ...
                    'RA and Dec must have the same number of elements.');
            end
            Nq = numel(RA);
            SearchRadiusRad = convert.angular(Args.RadiusUnits, 'rad', Args.SearchRadius);

            % --- Load HTM index from source --------------------------
            [IndexFileName, IndexVarName] = catsHTM.get_index_filename(CatName);
            SrcIndex = fullfile(SrcDir, IndexFileName);
            if ~isfile(SrcIndex)
                error('catsHTM:removeSource:NoIndex', ...
                    'Index file not found: %s', SrcIndex);
            end
            DataHTM = HDF5.load(SrcIndex, IndexVarName);
            Nhtm    = size(DataHTM, 1);
            Level   = celestial.htm.nhtm2level(Nhtm);
            HTM     = celestial.htm.htm_build(Level);

            [ColCell, ~] = catsHTM.load_colcell_from_dir(SrcDir, CatName);
            Ncol = numel(ColCell);

            % --- For each query: find candidate cells via cone search
            % (use raw htm_search_cone, not search_htm_ind, to avoid the
            % Nsrc>0 filter in case Nsrc was stale)
            CellsPerQuery = cell(Nq, 1);
            AllCells = [];
            for Iq = 1:Nq
                Ids = catsHTM.htm_search_cone(DataHTM, RA(Iq), Dec(Iq), SearchRadiusRad);
                CellsPerQuery{Iq} = Ids(:);
                AllCells = [AllCells; Ids(:)]; %#ok<AGROW>
            end
            UCells = unique(AllCells);
            Ngroup = numel(UCells);

            DirtyFiles    = containers.Map('KeyType','char','ValueType','any');
            DirtyCells    = false(Nhtm, 1);
            NewNsrc       = DataHTM(:, 13);
            SourcesRemoved = 0;
            NotFound      = true(Nq, 1);   % flipped to false on any match

            for Igroup = 1:Ngroup
                CellID = UCells(Igroup);

                % Which queries touch this cell
                QueriesHere = false(Nq, 1);
                for Iq = 1:Nq
                    if any(CellsPerQuery{Iq} == CellID)
                        QueriesHere(Iq) = true;
                    end
                end
                IqList = find(QueriesHere);

                [DataFileName, DataSetName] = ...
                    catsHTM.get_file_var_from_htmid(CatName, CellID, Args.NfilesInHDF);
                SrcFile = fullfile(SrcDir, DataFileName);
                DstFile = fullfile(DstDir, DataFileName);

                Existing = zeros(0, Ncol);
                if isfile(SrcFile)
                    try
                        Existing = HDF5.load(SrcFile, ['/' DataSetName]);
                    catch
                        Existing = zeros(0, Ncol);
                    end
                end
                if isempty(Existing)
                    continue;
                end

                DropMask = false(size(Existing, 1), 1);
                CellHadMatch = false;

                for Ii = 1:numel(IqList)
                    Iq = IqList(Ii);
                    D  = celestial.coo.sphere_dist_fast( ...
                        RA(Iq), Dec(Iq), Existing(:,1), Existing(:,2));
                    Match = find(D <= SearchRadiusRad);
                    if isempty(Match)
                        continue;
                    end
                    if numel(Match) > 1
                        switch Args.OnMultiMatch
                            case 'error'
                                error('catsHTM:removeSource:MultiMatch', ...
                                    ['Query %d (RA=%g, Dec=%g rad) matches %d sources ', ...
                                     'in HTM cell %d within %g %s.'], ...
                                    Iq, RA(Iq), Dec(Iq), numel(Match), CellID, ...
                                    Args.SearchRadius, Args.RadiusUnits);
                            case 'first'
                                [~, Imin] = min(D(Match));
                                Match = Match(Imin);
                            case 'all'
                                % keep all matches
                        end
                    end
                    DropMask(Match) = true;
                    CellHadMatch = true;
                    NotFound(Iq) = false;
                end

                if ~CellHadMatch
                    continue;
                end

                Remaining = Existing(~DropMask, :);
                Ndropped  = sum(DropMask);
                SourcesRemoved = SourcesRemoved + Ndropped;

                if Args.DryRun
                    DirtyFiles(DstFile) = true;
                    DirtyCells(CellID)  = true;
                    NewNsrc(CellID)     = size(Remaining, 1);
                    if Args.Verbose
                        fprintf('  [dry-run] cell %d: drop %d, %d -> %d rows in %s\n', ...
                            CellID, Ndropped, size(Existing,1), size(Remaining,1), DataFileName);
                    end
                    continue;
                end

                % Copy source file on first touch
                if ~DirtyFiles.isKey(DstFile)
                    if isfile(SrcFile)
                        copyfile(SrcFile, DstFile);
                    end
                    DirtyFiles(DstFile) = true;
                end

                % Always delete old datasets first
                catsHTM.delete_dataset(DstFile, ['/' DataSetName]);
                catsHTM.delete_dataset(DstFile, ['/' DataSetName '_Ind']);

                if ~isempty(Remaining)
                    catsHTM.save_cat(DstFile, DataSetName, Remaining, Args.SortCol, Args.StepRows);
                end
                % If Remaining is empty: leave datasets deleted; Nsrc=0 is
                % enough for cone_search/search_htm_ind to skip the cell.

                DirtyCells(CellID) = true;
                NewNsrc(CellID)    = size(Remaining, 1);

                if Args.Verbose
                    fprintf('  cell %d: drop %d, %d -> %d rows in %s\n', ...
                        CellID, Ndropped, size(Existing,1), size(Remaining,1), DataFileName);
                end
            end

            % --- Handle queries with no match anywhere ---------------
            if any(NotFound)
                Msg = sprintf('%d of %d input position(s) had no match within %g %s.', ...
                    sum(NotFound), Nq, Args.SearchRadius, Args.RadiusUnits);
                switch Args.OnNoMatch
                    case 'error'
                        error('catsHTM:removeSource:NoMatch', '%s', Msg);
                    case 'warn'
                        warning('catsHTM:removeSource:NoMatch', '%s', Msg);
                    case 'silent'
                        % no-op
                end
            end

            % --- Rewrite index file in OutDir ------------------------
            DstIndex = fullfile(DstDir, IndexFileName);
            if any(DirtyCells)
                if ~Args.DryRun
                    if isfile(DstIndex)
                        delete(DstIndex);
                    end
                    DataHTM(:, 13) = NewNsrc;
                    HDF5.save(single(DataHTM), DstIndex, IndexVarName);
                end
                DirtyFiles(DstIndex) = true;
            end

            FilesAbs = DirtyFiles.keys;
            ModifiedFiles = cell(numel(FilesAbs), 1);
            for If = 1:numel(FilesAbs)
                ModifiedFiles{If} = strrep(FilesAbs{If}, [OutDir filesep], '');
            end

            Result = struct( ...
                'OutDir',         OutDir, ...
                'ModifiedFiles',  {ModifiedFiles}, ...
                'CellsTouched',   sum(DirtyCells), ...
                'SourcesRemoved', SourcesRemoved, ...
                'NotFound',       NotFound);

            if Args.Verbose
                fprintf('removeSource: %d source(s) removed across %d cell(s)%s\n', ...
                    SourcesRemoved, sum(DirtyCells), ...
                    repmat(' (dry-run)', 1, double(Args.DryRun)));
                fprintf('  Files written under %s:\n', OutDir);
                for If = 1:numel(ModifiedFiles)
                    fprintf('    %s\n', ModifiedFiles{If});
                end
            end
        end


        function Result = insertColumns(CatName, ColName, ColUnit, OutDir, Args)
            % Insert one or more new columns into every HTM cell of a catsHTM catalog.
            % Package: @catsHTM
            % Description: Adds one or more new columns to a catsHTM catalog
            %              by rewriting every htm_<id> dataset and updating
            %              the ColCell .mat file. The source catalog at
            %              BaseDir is read but never modified; modified
            %              files are written under OutDir. The HTM index
            %              file is unchanged (Nsrc per cell does not change).
            %
            %              Multiple columns are inserted as one contiguous
            %              block in a SINGLE pass over the catalog (every
            %              data file is copied/rewritten only once) - avoid
            %              calling this once per column when the per-row
            %              payload is large (e.g. spectral catalogs).
            %
            %              SortCol is auto-shifted if the new block is
            %              inserted at or before its current position.
            %
            % Input  : - CatName  : Catalog name (e.g., 'ForcedPhotList').
            %          - ColName  : Name for the new column (char), or a
            %                       cellstr of names for multi-column insert.
            %                       Each must be unique within the catalog.
            %          - ColUnit  : Unit string (char) applied to all new
            %                       columns, or a cellstr matching ColName
            %                       element-by-element (e.g. 'day','mag','').
            %          - OutDir   : Writable directory mirroring BaseDir.
            %                    * ...,key,val,...
            %                      'BaseDir'         - Source catsHTM root, read-only.
            %                              Default ASTROPACK_CATSHTM_PATH
            %                              or '/euclid/catsHTM'.
            %                       'CatRelDir'       - Catalog subdir under BaseDir.
            %                              Default looked up from registry.
            %                       'FillValue'       - Scalar fill (default 0,
            %                              broadcast to all new columns) OR a
            %                              function handle of the form
            %                              @(M) Block where M is the existing
            %                              [Nrows x Ncol] cell matrix and
            %                              Block is the [Nrows x K] new-column
            %                              block (K = number of names; a
            %                              [Nrows x 1] vector is accepted when
            %                              K==1).
            %                       'Position'        - 'end' (default) or numeric
            %                              insert index in 1..Ncol+1 (start of
            %                              the new block).
            %                       'SortCol'         - Existing SortCol (default 2 = Dec).
            %                              Auto-shifted on insert.
            %                       'StepRows'        - Default 30.
            %                       'NfilesInHDF'     - Default 100.
            %                       'SkipExisting'    - Resume mode. Skip a source
            %                              data file whose OutDir copy already has
            %                              every htm cell at the post-insert column
            %                              count; partially written or old-width
            %                              files are reprocessed. Default false.
            %                       'DryRun'          - List affected files, no writes.
            %                              Default false.
            %                       'Verbose'         - Print progress. Default false.
            % Output : - Result struct with fields:
            %            .OutDir          - target directory
            %            .ModifiedFiles   - cellstr of files written
            %            .CellsTouched    - HTM cells modified
            %            .RowsTouched     - total source rows touched
            %            .NewColCell      - updated ColCell cell array
            %            .NewSortCol      - SortCol position after insert
            % Author : Dana Kovaleva (May 2026)
            % Example:
            %   % single column (or use the insertColumn alias):
            %   R = catsHTM.insertColumns('ForcedPhotList', 'JD_Added', 'day', ...
            %                             '~/tmp/cats_mod', 'FillValue', 0);
            %   % block of columns from a per-cell function, one pass:
            %   R = catsHTM.insertColumns('GAIADR3spec', {'PMRA','PMDec'}, ...
            %                             {'mas/yr','mas/yr'}, '~/tmp/cats_mod', ...
            %                             'FillValue', @(M) lookupBlock(M(:,1:2)));

            arguments
                CatName            (1,:) char
                ColName
                ColUnit
                OutDir             (1,:) char
                Args.BaseDir       (1,:) char    = ''
                Args.CatRelDir     (1,:) char    = ''
                Args.FillValue                   = 0
                Args.Position                    = 'end'
                Args.SortCol       (1,1) double  = 2
                Args.StepRows      (1,1) double  = 30
                Args.NfilesInHDF   (1,1) double  = 100
                Args.SkipExisting  (1,1) logical = false
                Args.DryRun        (1,1) logical = false
                Args.Verbose       (1,1) logical = false
            end

            [BaseDir, CatRelDir] = catsHTM.resolve_cat_paths(CatName, Args.BaseDir, Args.CatRelDir);
            SrcDir = fullfile(BaseDir, CatRelDir);
            DstDir = fullfile(OutDir, CatRelDir);
            if ~isfolder(SrcDir)
                error('catsHTM:insertColumn:NoSrcDir', ...
                    'Source catalog directory does not exist: %s', SrcDir);
            end
            if ~Args.DryRun && ~isfolder(DstDir)
                mkdir(DstDir);
            end

            % Existing ColCell / ColUnits
            [ColCell, ColUnits] = catsHTM.load_colcell_from_dir(SrcDir, CatName);
            Ncol = numel(ColCell);

            % Normalize ColName/ColUnit to cellstr (accept a single char
            % name or a cellstr of names for multi-column insertion).
            if ischar(ColName)
                NewNames = {ColName};
            elseif isstring(ColName)
                NewNames = cellstr(ColName(:).');
            else
                NewNames = ColName(:).';   % assume cellstr
            end
            Knew = numel(NewNames);
            if ischar(ColUnit)
                NewUnits = repmat({ColUnit}, 1, Knew);
            elseif isstring(ColUnit)
                NewUnits = cellstr(ColUnit(:).');
            else
                NewUnits = ColUnit(:).';   % assume cellstr
            end
            if numel(NewUnits) ~= Knew
                error('catsHTM:insertColumn:UnitCountMismatch', ...
                    'ColUnit must be a single unit or match the %d column name(s).', Knew);
            end

            % Duplicate-name checks (vs existing columns, and within new set)
            for In = 1:Knew
                if any(strcmp(ColCell, NewNames{In}))
                    error('catsHTM:insertColumn:DuplicateName', ...
                        'Column "%s" already exists in catalog %s.', NewNames{In}, CatName);
                end
            end
            if numel(unique(NewNames)) ~= Knew
                error('catsHTM:insertColumn:DuplicateNewName', ...
                    'Duplicate names within the requested new columns.');
            end

            if isempty(ColUnits)
                ColUnits = repmat({''}, 1, Ncol);
            elseif numel(ColUnits) < Ncol
                ColUnits = [ColUnits(:).', repmat({''}, 1, Ncol - numel(ColUnits))];
            end

            % Position (start index of the inserted block)
            if (ischar(Args.Position) || isstring(Args.Position))
                if ~strcmpi(Args.Position, 'end')
                    error('catsHTM:insertColumn:BadPosition', ...
                        'Position must be ''end'' or a numeric index in 1..Ncol+1.');
                end
                Pos = Ncol + 1;
            else
                Pos = double(Args.Position);
                if Pos < 1 || Pos > Ncol + 1 || Pos ~= round(Pos)
                    error('catsHTM:insertColumn:BadPosition', ...
                        'Position %g out of range 1..%d.', Pos, Ncol + 1);
                end
            end

            NewColCell  = [ColCell(1:Pos-1), NewNames, ColCell(Pos:end)];
            NewColUnits = [ColUnits(1:Pos-1), NewUnits, ColUnits(Pos:end)];

            % Auto-shift SortCol if the new block is inserted at/before it
            NewSortCol = Args.SortCol;
            if Pos <= Args.SortCol
                NewSortCol = Args.SortCol + Knew;
            end

            % Iterate over all data files in BaseDir
            Files = dir(fullfile(SrcDir, sprintf('%s_htm_*.hdf5', CatName)));
            Nfiles = numel(Files);
            if Nfiles == 0
                error('catsHTM:insertColumn:NoFiles', ...
                    'No %s_htm_*.hdf5 files in %s.', CatName, SrcDir);
            end

            DirtyFiles   = containers.Map('KeyType','char','ValueType','any');
            CellsTouched = 0;
            RowsTouched  = 0;
            IsFunFill    = isa(Args.FillValue, 'function_handle');

            for If = 1:Nfiles
                SrcFile = fullfile(SrcDir, Files(If).name);
                DstFile = fullfile(DstDir, Files(If).name);
                Info = h5info(SrcFile);
                Names = {Info.Datasets.Name};
                IndH = find(cellfun(@numel, strfind(Names, '_')) == 1);
                Nih = numel(IndH);
                if Nih == 0
                    continue;
                end

                % Resume: skip a source file whose OutDir copy already has
                % every htm cell at the post-insert column count.
                if Args.SkipExisting && ~Args.DryRun && isfile(DstFile)
                    InfoD = h5info(DstFile);
                    NmD   = {InfoD.Datasets.Name};
                    IndHD = find(cellfun(@numel, strfind(NmD, '_')) == 1);
                    DoneF = ~isempty(IndHD);
                    for Iq = 1:numel(IndHD)
                        Sz = InfoD.Datasets(IndHD(Iq)).Dataspace.Size;
                        if ~any(Sz == numel(NewColCell))
                            DoneF = false;
                        end
                    end
                    if DoneF
                        DirtyFiles(DstFile) = true;   % keep so index is rewritten
                        if Args.Verbose
                            fprintf('  %s: already complete - skipped (resume)\n', Files(If).name);
                        end
                        continue;
                    end
                end

                % Write the OutDir file FRESH (no copyfile). Every htm cell
                % in the file is rewritten with the new column block, so
                % copying the source first (then overwriting every dataset)
                % is pure I/O waste - costly for fat catalogs on NFS. catsHTM
                % data files hold only htm_<id>/_Ind datasets (all rewritten
                % here), so nothing is lost. Start from a clean DstFile so
                % save_cat's H5D.create does not hit pre-existing datasets.
                if ~Args.DryRun && ~DirtyFiles.isKey(DstFile)
                    if isfile(DstFile)
                        delete(DstFile);
                    end
                    DirtyFiles(DstFile) = true;
                end

                for Iih = 1:Nih
                    DataSetName = Info.Datasets(IndH(Iih)).Name;
                    Cat = HDF5.load(SrcFile, ['/' DataSetName]);
                    Nrows = size(Cat, 1);

                    if IsFunFill
                        FillBlock = Args.FillValue(Cat);
                        if Knew == 1
                            FillBlock = FillBlock(:);
                        end
                        if ~isequal(size(FillBlock), [Nrows, Knew])
                            error('catsHTM:insertColumn:FillSizeMismatch', ...
                                'FillValue function returned a %dx%d block; expected %dx%d in %s.', ...
                                size(FillBlock,1), size(FillBlock,2), Nrows, Knew, DataSetName);
                        end
                    else
                        FillBlock = repmat(Args.FillValue, Nrows, Knew);
                    end

                    NewCat = [Cat(:, 1:Pos-1), FillBlock, Cat(:, Pos:end)];

                    if ~Args.DryRun
                        catsHTM.save_cat(DstFile, DataSetName, NewCat, NewSortCol, Args.StepRows);
                    end

                    CellsTouched = CellsTouched + 1;
                    RowsTouched  = RowsTouched + Nrows;
                end

                if Args.Verbose
                    fprintf('  %s: %d cell(s)%s\n', Files(If).name, Nih, ...
                        repmat(' (dry-run)', 1, double(Args.DryRun)));
                end
            end

            % ColCell .mat
            ColCellFile = fullfile(DstDir, sprintf('%s_htmColCell.mat', CatName));
            if ~Args.DryRun
                ColCell  = NewColCell;   %#ok<NASGU>
                ColUnits = NewColUnits;  %#ok<NASGU>
                save(ColCellFile, 'ColCell', 'ColUnits');
                DirtyFiles(ColCellFile) = true;
            end

            % HTM index .hdf5: unchanged by a column add (same Nsrc / HTM
            % structure), so copy it from BaseDir so OutDir is a complete,
            % queryable catalog.
            [IndexFileName, ~] = catsHTM.get_index_filename(CatName);
            SrcIndex = fullfile(SrcDir, IndexFileName);
            DstIndex = fullfile(DstDir, IndexFileName);
            if ~Args.DryRun && isfile(SrcIndex) && ~isfile(DstIndex)
                copyfile(SrcIndex, DstIndex);
                DirtyFiles(DstIndex) = true;
            end

            FilesAbs = DirtyFiles.keys;
            ModifiedFiles = cell(numel(FilesAbs), 1);
            for If = 1:numel(FilesAbs)
                ModifiedFiles{If} = strrep(FilesAbs{If}, [OutDir filesep], '');
            end

            Result = struct( ...
                'OutDir',         OutDir, ...
                'ModifiedFiles',  {ModifiedFiles}, ...
                'CellsTouched',   CellsTouched, ...
                'RowsTouched',    RowsTouched, ...
                'NewColCell',     {NewColCell}, ...
                'NewSortCol',     NewSortCol);

            if Args.Verbose
                fprintf('insertColumns: added %d column(s) [%s] at position %d across %d cell(s), %d row(s)%s\n', ...
                    Knew, strjoin(NewNames, ', '), Pos, CellsTouched, RowsTouched, ...
                    repmat(' (dry-run)', 1, double(Args.DryRun)));
                fprintf('  Files written under %s:\n', OutDir);
                for If = 1:numel(ModifiedFiles)
                    fprintf('    %s\n', ModifiedFiles{If});
                end
            end
        end


        function Result = insertColumn(CatName, ColName, ColUnit, OutDir, varargin)
            % Single-column alias for catsHTM.insertColumns (backward compatible).
            % Package: @catsHTM
            % Description: Thin wrapper kept for backward compatibility.
            %              insertColumns is the primary implementation and
            %              accepts either a single column (char name/unit)
            %              or a block (cellstr names/units) in one pass.
            % Input  : - See catsHTM.insertColumns. ColName/ColUnit are
            %            typically a single char here, but cellstr is also
            %            forwarded unchanged.
            % Output : - Result struct from catsHTM.insertColumns.
            % Example: R = catsHTM.insertColumn('ForcedPhotList','JD_Added','day','~/tmp/cats_mod');
            Result = catsHTM.insertColumns(CatName, ColName, ColUnit, OutDir, varargin{:});
        end


        function Result = renameCat(SrcName, DstName, CatDir, Args)
            % Rename a catsHTM catalog in place (file names + index variable).
            % Package: @catsHTM
            % Description: Rename all files of a catsHTM catalog located in a
            %              single directory from <SrcName>_* to <DstName>_*.
            %              Data files (<Src>_htm_<id>.hdf5) and the ColCell
            %              .mat are just renamed (their contents are catalog-
            %              name-independent: datasets are 'htm_<id>'/'_Ind',
            %              the .mat holds ColCell/ColUnits). The HTM index
            %              file holds an internal dataset '<Src>_HTM', so it
            %              is rewritten as '<Dst>_HTM' (and the old index
            %              removed). No /euclid-style registry update is done.
            % Input  : - Source catalog name (e.g., 'GAIADR3spec').
            %          - Destination catalog name (e.g., 'GAIADR3specplus').
            %          - Directory holding the <Src>_* files (e.g.
            %            '/home/dana/tmp/GAIADR3spec_merged/GAIA/DR3spec').
            %          * ...,key,val,...
            %            'DryRun'  - List actions without renaming. Default false.
            %            'Verbose' - Print progress. Default false.
            % Output : - Result struct: .DataFiles (count), .Index (logical),
            %            .ColCell (logical), .CatDir.
            % Author : Dana Kovaleva (Jun 2026)
            % Example:
            %   catsHTM.renameCat('GAIADR3spec','GAIADR3specplus', ...
            %       '/home/dana/tmp/GAIADR3spec_merged/GAIA/DR3spec','Verbose',true);
            arguments
                SrcName       (1,:) char
                DstName       (1,:) char
                CatDir        (1,:) char
                Args.DryRun   (1,1) logical = false
                Args.Verbose  (1,1) logical = false
            end

            if ~isfolder(CatDir)
                error('catsHTM:renameCat:NoDir', 'Directory not found: %s', CatDir);
            end

            % --- Data files: <Src>_htm_<id>.hdf5 -> <Dst>_htm_<id>.hdf5 -----
            Files  = dir(fullfile(CatDir, sprintf('%s_htm_*.hdf5', SrcName)));
            Ndata  = 0;
            for If = 1:numel(Files)
                OldF = fullfile(CatDir, Files(If).name);
                NewN = strrep(Files(If).name, [SrcName '_htm_'], [DstName '_htm_']);
                NewF = fullfile(CatDir, NewN);
                if ~Args.DryRun
                    movefile(OldF, NewF);
                end
                Ndata = Ndata + 1;
            end

            % --- ColCell .mat -----------------------------------------------
            SrcMat = fullfile(CatDir, sprintf('%s_htmColCell.mat', SrcName));
            DstMat = fullfile(CatDir, sprintf('%s_htmColCell.mat', DstName));
            HasMat = isfile(SrcMat);
            if HasMat && ~Args.DryRun
                movefile(SrcMat, DstMat);
            end

            % --- Index file: rewrite internal '<Src>_HTM' as '<Dst>_HTM' ----
            SrcIdx = fullfile(CatDir, sprintf('%s_htm.hdf5', SrcName));
            DstIdx = fullfile(CatDir, sprintf('%s_htm.hdf5', DstName));
            HasIdx = isfile(SrcIdx);
            if HasIdx && ~Args.DryRun
                DataHTM = HDF5.load(SrcIdx, sprintf('%s_HTM', SrcName));
                if isfile(DstIdx)
                    delete(DstIdx);
                end
                HDF5.save(DataHTM, DstIdx, sprintf('/%s_HTM', DstName));
                delete(SrcIdx);
            end

            Result = struct('DataFiles', Ndata, 'Index', HasIdx, ...
                            'ColCell', HasMat, 'CatDir', CatDir);

            if Args.Verbose
                fprintf('renameCat: %s -> %s in %s%s\n', SrcName, DstName, CatDir, ...
                    repmat(' (dry-run)', 1, double(Args.DryRun)));
                fprintf('  data files: %d, index: %d, colcell: %d\n', Ndata, HasIdx, HasMat);
            end
        end


        function Result = removeColumn(CatName, ColName, OutDir, Args)
            % Remove a column from every HTM cell of a catsHTM catalog.
            % Package: @catsHTM
            % Description: Drops a column (matched by name) from every
            %              htm_<id> dataset and updates the ColCell .mat
            %              file. The source catalog at BaseDir is read but
            %              never modified; modified files are written
            %              under OutDir. The HTM index file is unchanged.
            %
            %              Refuses to remove RA (column 1), Dec (column 2),
            %              or the current SortCol -- these are required
            %              for spatial lookup and the binary _Ind index.
            %
            %              SortCol is auto-shifted if a column to its left
            %              is removed.
            %
            % Input  : - CatName  : Catalog name.
            %          - ColName  : Name of the column to remove.
            %          - OutDir   : Writable directory mirroring BaseDir.
            %                    * ...,key,val,...
            %                      'BaseDir'         - Default ASTROPACK_CATSHTM_PATH
            %                              or '/euclid/catsHTM'.
            %                       'CatRelDir'       - Default looked up from registry.
            %                       'SortCol'         - Default 2 (Dec). Auto-shifted.
            %                       'StepRows'        - Default 30.
            %                       'NfilesInHDF'     - Default 100.
            %                       'DryRun'          - Default false.
            %                       'Verbose'         - Print progress. Default false.
            % Output : - Result struct (same shape as insertColumn) plus
            %            .RemovedAt       - position of removed column
            % Author : Dana Kovaleva (May 2026)
            % Example:
            %   R = catsHTM.removeColumn('ForcedPhotList', 'JD_Added', '~/tmp/cats_mod');

            arguments
                CatName            (1,:) char
                ColName            (1,:) char
                OutDir             (1,:) char
                Args.BaseDir       (1,:) char    = ''
                Args.CatRelDir     (1,:) char    = ''
                Args.SortCol       (1,1) double  = 2
                Args.StepRows      (1,1) double  = 30
                Args.NfilesInHDF   (1,1) double  = 100
                Args.DryRun        (1,1) logical = false
                Args.Verbose       (1,1) logical = false
            end

            [BaseDir, CatRelDir] = catsHTM.resolve_cat_paths(CatName, Args.BaseDir, Args.CatRelDir);
            SrcDir = fullfile(BaseDir, CatRelDir);
            DstDir = fullfile(OutDir, CatRelDir);
            if ~isfolder(SrcDir)
                error('catsHTM:removeColumn:NoSrcDir', ...
                    'Source catalog directory does not exist: %s', SrcDir);
            end
            if ~Args.DryRun && ~isfolder(DstDir)
                mkdir(DstDir);
            end

            [ColCell, ColUnits] = catsHTM.load_colcell_from_dir(SrcDir, CatName);
            Ncol = numel(ColCell);
            Pos  = find(strcmp(ColCell, ColName), 1);
            if isempty(Pos)
                error('catsHTM:removeColumn:NotFound', ...
                    'Column "%s" not found in catalog %s. Available: %s', ...
                    ColName, CatName, strjoin(ColCell, ', '));
            end
            if Pos == 1 || Pos == 2
                error('catsHTM:removeColumn:CoordColumn', ...
                    ['Refusing to remove RA/Dec (column %d). These are ', ...
                     'required for catsHTM spatial lookup.'], Pos);
            end
            if Pos == Args.SortCol
                error('catsHTM:removeColumn:SortColumn', ...
                    ['Refusing to remove the SortCol (column %d, "%s"). ', ...
                     'Re-sort the catalog by another column first.'], Pos, ColName);
            end
            if Ncol <= 2
                error('catsHTM:removeColumn:TooFew', ...
                    'Catalog has only %d column(s); cannot remove any more.', Ncol);
            end
            if isempty(ColUnits) || numel(ColUnits) < Ncol
                ColUnits = [ColUnits(:).', repmat({''}, 1, Ncol - numel(ColUnits))];
            end

            NewColCell  = ColCell;   NewColCell(Pos)  = [];
            NewColUnits = ColUnits;  NewColUnits(Pos) = [];

            NewSortCol = Args.SortCol;
            if Pos < Args.SortCol
                NewSortCol = Args.SortCol - 1;
            end

            Files = dir(fullfile(SrcDir, sprintf('%s_htm_*.hdf5', CatName)));
            Nfiles = numel(Files);
            if Nfiles == 0
                error('catsHTM:removeColumn:NoFiles', ...
                    'No %s_htm_*.hdf5 files in %s.', CatName, SrcDir);
            end

            DirtyFiles   = containers.Map('KeyType','char','ValueType','any');
            CellsTouched = 0;
            RowsTouched  = 0;

            for If = 1:Nfiles
                SrcFile = fullfile(SrcDir, Files(If).name);
                DstFile = fullfile(DstDir, Files(If).name);
                Info = h5info(SrcFile);
                Names = {Info.Datasets.Name};
                IndH = find(cellfun(@numel, strfind(Names, '_')) == 1);
                Nih = numel(IndH);
                if Nih == 0
                    continue;
                end

                if ~Args.DryRun && ~DirtyFiles.isKey(DstFile)
                    copyfile(SrcFile, DstFile);
                    DirtyFiles(DstFile) = true;
                end

                for Iih = 1:Nih
                    DataSetName = Info.Datasets(IndH(Iih)).Name;
                    Cat = HDF5.load(SrcFile, ['/' DataSetName]);
                    Nrows = size(Cat, 1);

                    NewCat = Cat;
                    NewCat(:, Pos) = [];

                    if ~Args.DryRun
                        catsHTM.delete_dataset(DstFile, ['/' DataSetName]);
                        catsHTM.delete_dataset(DstFile, ['/' DataSetName '_Ind']);
                        catsHTM.save_cat(DstFile, DataSetName, NewCat, NewSortCol, Args.StepRows);
                    end

                    CellsTouched = CellsTouched + 1;
                    RowsTouched  = RowsTouched + Nrows;
                end

                if Args.Verbose
                    fprintf('  %s: %d cell(s)%s\n', Files(If).name, Nih, ...
                        repmat(' (dry-run)', 1, double(Args.DryRun)));
                end
            end

            ColCellFile = fullfile(DstDir, sprintf('%s_htmColCell.mat', CatName));
            if ~Args.DryRun
                ColCell  = NewColCell;   %#ok<NASGU>
                ColUnits = NewColUnits;  %#ok<NASGU>
                save(ColCellFile, 'ColCell', 'ColUnits');
                DirtyFiles(ColCellFile) = true;
            end

            FilesAbs = DirtyFiles.keys;
            ModifiedFiles = cell(numel(FilesAbs), 1);
            for If = 1:numel(FilesAbs)
                ModifiedFiles{If} = strrep(FilesAbs{If}, [OutDir filesep], '');
            end

            Result = struct( ...
                'OutDir',         OutDir, ...
                'ModifiedFiles',  {ModifiedFiles}, ...
                'CellsTouched',   CellsTouched, ...
                'RowsTouched',    RowsTouched, ...
                'NewColCell',     {NewColCell}, ...
                'NewSortCol',     NewSortCol, ...
                'RemovedAt',      Pos);

            if Args.Verbose
                fprintf('removeColumn: removed "%s" (was column %d) from %d cell(s), %d row(s)%s\n', ...
                    ColName, Pos, CellsTouched, RowsTouched, ...
                    repmat(' (dry-run)', 1, double(Args.DryRun)));
                fprintf('  Files written under %s:\n', OutDir);
                for If = 1:numel(ModifiedFiles)
                    fprintf('    %s\n', ModifiedFiles{If});
                end
            end
        end


        function [BaseDir, CatRelDir] = resolve_cat_paths(CatName, BaseDirIn, CatRelDirIn)
            % Resolve BaseDir and catalog subdir for a catsHTM catalog.
            % Falls back to ASTROPACK_CATSHTM_PATH then '/euclid/catsHTM'
            % for BaseDir, and to catsHTM.catalogs for CatRelDir.
            % Author : Dana Kovaleva (May 2026)
            % Example: [B,R] = catsHTM.resolve_cat_paths('ForcedPhotList','','');
            BaseDir = BaseDirIn;
            if isempty(BaseDir)
                BaseDir = getenv('ASTROPACK_CATSHTM_PATH');
                if isempty(BaseDir)
                    BaseDir = '/euclid/catsHTM';
                end
            end
            CatRelDir = CatRelDirIn;
            if isempty(CatRelDir)
                Reg = catsHTM.catalogs;
                Found = false;
                for Ir = 1:numel(Reg)
                    if strcmp(Reg(Ir).Name, CatName)
                        CatRelDir = Reg(Ir).Dir;
                        Found = true;
                        break;
                    end
                end
                if ~Found
                    error('catsHTM:resolve_cat_paths:NotFound', ...
                        ['Catalog "%s" not found in catsHTM.catalogs registry. ', ...
                         'Pass CatRelDir explicitly.'], CatName);
                end
            end
            if ~startsWith(CatRelDir, '/'), CatRelDir = ['/' CatRelDir]; end
            if ~endsWith(CatRelDir, '/'),   CatRelDir = [CatRelDir '/']; end
        end


        function [ColCell, ColUnits] = load_colcell_from_dir(CatDir, CatName)
            % Load <CatName>_htmColCell.mat from a specific directory.
            % Used instead of catsHTM.load_colcell when reading from a
            % path that is not on the MATLAB path (e.g., a read-only
            % BaseDir like /euclid/catsHTM).
            % Author : Dana Kovaleva (May 2026)
            % Example: [C,U] = catsHTM.load_colcell_from_dir('/euclid/catsHTM/ForcedPhotList','ForcedPhotList');
            ColCellFile = fullfile(CatDir, sprintf('%s_htmColCell.mat', CatName));
            if ~isfile(ColCellFile)
                error('catsHTM:load_colcell_from_dir:NotFound', ...
                    'ColCell file not found: %s', ColCellFile);
            end
            S = load(ColCellFile);
            ColCell  = S.ColCell;
            if isfield(S, 'ColUnits')
                ColUnits = S.ColUnits;
            else
                ColUnits = {};
            end
        end


        function Mat = project_to_colcell(NewCat, ColCell)
            % Project an AstroCatalog or numeric matrix onto ColCell order.
            % AstroCatalog columns are matched by name (case-sensitive).
            % Missing columns become NaN. Extras trigger a warning.
            % Numeric input must already be in ColCell order.
            % RA/Dec assumed in radians; deg->rad conversion is applied if
            % NewCat.ColUnits says 'deg' for the RA or Dec column.
            % Author : Dana Kovaleva (May 2026)
            % Example:
            %   AC = AstroCatalog;
            %   AC.Catalog  = [1.0, 0.5, 17.5];
            %   AC.ColNames = {'RA','Dec','Mag'};
            %   AC.ColUnits = {'rad','rad','mag'};
            %   M = catsHTM.project_to_colcell(AC, {'RA','Dec','Mag','MagErr'});
            Ncol = numel(ColCell);
            if isnumeric(NewCat)
                if size(NewCat, 2) ~= Ncol
                    error('catsHTM:project_to_colcell:NcolMismatch', ...
                        'Numeric NewCat has %d columns, expected %d (ColCell order).', ...
                        size(NewCat, 2), Ncol);
                end
                Mat = NewCat;
                return;
            end

            if ~isa(NewCat, 'AstroCatalog') && ~isa(NewCat, 'AstroTable')
                error('catsHTM:project_to_colcell:BadType', ...
                    'NewCat must be numeric, AstroCatalog, or AstroTable.');
            end

            SrcCat   = NewCat.Catalog;
            SrcNames = NewCat.ColNames;
            Nrow     = size(SrcCat, 1);
            Mat = nan(Nrow, Ncol);

            Used = false(numel(SrcNames), 1);
            for Ic = 1:Ncol
                Ix = find(strcmp(SrcNames, ColCell{Ic}), 1);
                if ~isempty(Ix)
                    Mat(:, Ic) = SrcCat(:, Ix);
                    Used(Ix) = true;
                end
            end

            Extras = SrcNames(~Used);
            if ~isempty(Extras)
                warning('catsHTM:project_to_colcell:ExtraCols', ...
                    'Ignoring NewCat columns not in catalog ColCell: %s', ...
                    strjoin(Extras, ', '));
            end

            % Convert RA/Dec from deg to rad if ColUnits says so.
            % AstroCatalog/AstroTable carries per-column units in
            % NewCat.ColUnits; CooUnits is not a public property here.
            if isprop(NewCat, 'ColUnits') && ~isempty(NewCat.ColUnits)
                CU = NewCat.ColUnits;
                Ix = find(strcmp(SrcNames, ColCell{1}), 1);
                if ~isempty(Ix) && numel(CU) >= Ix && strcmpi(CU{Ix}, 'deg')
                    Mat(:, 1) = Mat(:, 1) .* (pi/180);
                end
                Iy = find(strcmp(SrcNames, ColCell{2}), 1);
                if ~isempty(Iy) && numel(CU) >= Iy && strcmpi(CU{Iy}, 'deg')
                    Mat(:, 2) = Mat(:, 2) .* (pi/180);
                end
            end
        end


    end % Static

    % utilities
    methods (Static)
        function Nsrc=get_nsrc(CatName)
            % Count number of sources over all HTM in HDF5 files
            % Package: @catsHTM
            % Input  : - Catalog name (e.g., 'APASSS')
            % Output : - A matrix of [HTM_index, Nsrc]
            % Example: Nsrc=catsHTM.get_nsrc(CatName);
            % Reliable: 2
            
            Dir = dir(sprintf('%s_htm_*.hdf5',CatName));
            Ndir = numel(Dir);
            Nsrc = zeros(100.*Ndir,3);
            K = 0;
            
            for Idir=1:1:Ndir
                Info = h5info(Dir(Idir).name);
                IndH = find(cellfun(@numel,strfind({Info.Datasets.Name},'_'))==1);
                Nih  = numel(IndH);
                for Iih=1:1:Nih
                    K = K + 1;
                    IndHTM = str2double(Info.Datasets(IndH(Iih)).Name(5:end));
                    Nsrc(K,:) = [IndHTM size(h5read(Dir(Idir).name,sprintf('/%s',Info.Datasets(IndH(Iih)).Name)),1),Idir];
                end
            end
            Nsrc = Nsrc(1:K,:);

        end

        function Nsrc = getNsrcMeta(CatName, Args)
            % Count sources per HTM cell from HDF5 metadata only.
            % Package: @catsHTM
            % Description: Same output as catsHTM.get_nsrc but reads each
            %              dataset's row count from h5info dataspace
            %              dimensions instead of loading the data with
            %              h5read. Orders of magnitude faster on large
            %              catalogs and uses negligible memory.
            %              HDF5.save writes datasets with fliplr, so
            %              h5info reports them in MATLAB column-major
            %              order [Nrows, Ncols] -- Nrows is index 1.
            %              The catalog directory is resolved on the MATLAB
            %              path via which('<Cat>_htmColCell.mat') (or the
            %              CatDir override), so the function is location-
            %              independent and need NOT be run from the catalog
            %              directory.
            % Input  : - Catalog base name (e.g., 'DECaLS10').
            %          * ...,key,val,...
            %            'CatDir' - Directory holding the catalog HDF5 files.
            %                   Default '' = resolve via which() on the path.
            % Output : - Matrix [HTM_index, Nsrc] with source count per cell.
            % Author : Dana Kovaleva (Mar 2026)
            % Example: Nsrc = catsHTM.getNsrcMeta('ForcedPhotList');
            arguments
                CatName
                Args.CatDir = '';
            end

            % Resolve the catalog directory. dir()/h5info with a bare name
            % only see the current directory; anchor to the catalog dir via
            % the colcell .mat (which() reliably finds .mat files, checking
            % the cwd too) so this works regardless of the current folder.
            CatDir = Args.CatDir;
            if isempty(CatDir)
                ColCellFull = which(sprintf('%s_htmColCell.mat', CatName));
                if isempty(ColCellFull)
                    error('catsHTM:getNsrcMeta:noCatalog', ...
                        'Cannot find %s_htmColCell.mat on the MATLAB path (add the catalog directory or pass CatDir).', ...
                        CatName);
                end
                CatDir = fileparts(ColCellFull);
            end

            Dir = dir(fullfile(CatDir, sprintf('%s_htm_*.hdf5', CatName)));
            Ndir = numel(Dir);
            Nsrc = zeros(100 .* Ndir, 2);
            K = 0;
            for Idir = 1:Ndir
                FullName = fullfile(Dir(Idir).folder, Dir(Idir).name);
                Info = h5info(FullName);
                IndH = find(cellfun(@numel, strfind({Info.Datasets.Name}, '_')) == 1);
                Nih = numel(IndH);
                for Iih = 1:Nih
                    K = K + 1;
                    IndHTM = str2double(Info.Datasets(IndH(Iih)).Name(5:end));
                    DsSize = Info.Datasets(IndH(Iih)).Dataspace.Size;
                    Nsrc(K, :) = [IndHTM, DsSize(1)];
                end
            end
            Nsrc = Nsrc(1:K, :);
        end

        function Sig = catalogSignature(CatName, Args)
            % Compute a lightweight version signature of a catsHTM catalog.
            % Package: @catsHTM
            % Description: A storage pointer (CellID,RowInCell) - and the scalar
            %              CatRowID derived from it - addresses ONE specific build
            %              of a catalog. Re-ingesting the catalog, or mutating it
            %              with addSource/removeSource, moves rows and silently
            %              invalidates stored pointers. This returns a compact
            %              signature that changes when the build changes, so
            %              persisted pointers can be validated before use
            %              (see catsHTM.checkCatalogSignature). It is CHEAP: the
            %              row-layout fingerprint is read from the single small
            %              index file (<Cat>_htm.hdf5, column 13 = per-cell Nsrc),
            %              NOT by scanning the data files. Three layered hashes,
            %              each mapping to a distinct kind of change:
            %                LayoutHash   - per-cell row counts. Changes iff rows
            %                               are added/removed/re-partitioned -> the
            %                               exact invariant pointer/CatRowID
            %                               validity depends on.
            %                ColHash      - column names + units. Changes on column
            %                               insert/rename (e.g. insertColumns);
            %                               row pointers still survive this.
            %                ChecksumHash - md5 of the deployment checksum list
            %                               (list*checksum* in the catalog dir), if
            %                               present. A strong content fingerprint
            %                               that also catches same-count in-cell
            %                               reordering. Empty when no list is found.
            % Input  : - Catalog base name (e.g., 'GAIADR3').
            %          * ...,key,val,...
            %            'CatDir' - Directory holding the catalog HDF5 files.
            %                   Default '' = resolve via which() on the path.
            %            'Checksum' - Include ChecksumHash from the checksum-list
            %                   file when exactly one is found. Default true.
            % Output : - Sig - struct with fields Name, CatDir, Ncell, Nsrc,
            %            LayoutHash, ColHash, ChecksumHash, ChecksumFile, Version
            %            (a short combined id) and StampedAt (timestamp string).
            % Author : Dana Kovaleva (Aug 2026)
            % See also: catsHTM.checkCatalogSignature, catsHTM.crossIDCatsHTM
            %           (stamps this into Summary.Signature), catsHTM.catRowID.
            % Example: Sig = catsHTM.catalogSignature('GAIADR3');
            arguments
                CatName
                Args.CatDir            = '';
                Args.Checksum logical  = true;
            end

            % resolve the catalog directory (which() checks the cwd too)
            CatDir = Args.CatDir;
            if isempty(CatDir)
                ColCellFull = which(sprintf('%s_htmColCell.mat', CatName));
                if isempty(ColCellFull)
                    error('catsHTM:catalogSignature:noCatalog', ...
                        'Cannot find %s_htmColCell.mat on the MATLAB path (add the catalog directory or pass CatDir).', ...
                        CatName);
                end
                CatDir = fileparts(ColCellFull);
            end

            % --- LayoutHash: per-cell row counts from the index (one small file)
            IndexFile = fullfile(CatDir, sprintf('%s_htm.hdf5', CatName));
            if exist(IndexFile, 'file') ~= 2
                error('catsHTM:catalogSignature:noIndex', ...
                    'Index file %s not found.', IndexFile);
            end
            % read the index matrix directly (col 13 = Nsrc); skip the HTM
            % struct build load_htm_ind would do - only the counts are needed.
            IndData    = HDF5.load(IndexFile, sprintf('%s_HTM', CatName));
            NsrcCol    = double(IndData(:, 13));   % column 13 = Nsrc per cell
            % non-leaf / empty cells store NaN here; treat as 0 so the total
            % and the hash are finite and match getNsrcMeta's leaf-only count.
            NsrcCol(~isfinite(NsrcCol)) = 0;
            Ncell      = sum(NsrcCol > 0);
            NsrcTotal  = sum(NsrcCol);
            LayoutHash = localHashBytes(typecast(NsrcCol(:).', 'uint8'));

            % --- ColHash: column names + units
            [ColCell, ColUnits] = catsHTM.load_colcell_from_dir(CatDir, CatName);
            ColStr  = strjoin([ColCell(:); ColUnits(:)], newline);
            ColHash = localHashBytes(uint8(ColStr));

            % --- ChecksumHash: md5-list file if present (best effort, non-fatal)
            ChecksumHash = '';
            ChecksumFile = '';
            if Args.Checksum
                D = dir(fullfile(CatDir, 'list*checksum*'));
                D = D(~[D.isdir]);
                if numel(D) == 1
                    ChecksumFile = fullfile(D(1).folder, D(1).name);
                    ChecksumHash = localHashBytes(localReadBytes(ChecksumFile));
                end
            end

            Version = localHashBytes(uint8([LayoutHash ColHash]));
            Sig = struct();
            Sig.Name         = CatName;
            Sig.CatDir       = CatDir;
            Sig.Ncell        = Ncell;
            Sig.Nsrc         = NsrcTotal;
            Sig.LayoutHash   = LayoutHash;
            Sig.ColHash      = ColHash;
            Sig.ChecksumHash = ChecksumHash;
            Sig.ChecksumFile = ChecksumFile;
            Sig.Version      = Version(1:16);
            Sig.StampedAt    = char(datetime('now', 'Format', 'yyyy-MM-dd''T''HH:mm:ss'));
        end

        function [Ok, Report] = checkCatalogSignature(CatName, Stored, Args)
            % Validate a stored catsHTM signature against the catalog's current state.
            % Package: @catsHTM
            % Description: Compare a signature captured when pointers were stamped
            %              (catsHTM.catalogSignature, e.g. from Summary.Signature)
            %              against the catalog on disk NOW, and classify any change
            %              so a caller knows whether stored (CellID,RowInCell) /
            %              CatRowID pointers are still safe to dereference:
            %                'valid'           - identical; pointers valid.
            %                'columns-changed' - only columns changed (insertColumns);
            %                                    row pointers still valid.
            %                'stale-layout'    - row counts changed (re-ingest /
            %                                    add/removeSource); pointers INVALID.
            %                'stale-suspect'   - counts match but content checksum
            %                                    differs (possible in-cell reorder);
            %                                    pointers unreliable, deep-verify.
            %              Cheap: only re-reads the index + colcell (+ checksum list).
            % Input  : - Catalog base name (e.g., 'GAIADR3').
            %          - Stored - the previously captured signature struct.
            %          * ...,key,val,...
            %            'CatDir' - Directory holding the catalog HDF5 files.
            %                   Default '' = resolve via which() on the path.
            %            'Checksum' - Recompute/compare ChecksumHash. Default true.
            %            'Warn' - Emit a warning when not Ok. Default true.
            % Output : - Ok - true iff row pointers remain valid (Status 'valid'
            %            or 'columns-changed').
            %          - Report - struct with Status, Ok, LayoutMatch, ColMatch,
            %            ChecksumMatch ([] if undeterminable), Message, Stored, Current.
            % Author : Dana Kovaleva (Aug 2026)
            % See also: catsHTM.catalogSignature, catsHTM.gatherByPointer (ValidateSig).
            % Example: [Ok, Rep] = catsHTM.checkCatalogSignature('GAIADR3', S.Signature.GAIADR3);
            arguments
                CatName
                Stored struct
                Args.CatDir            = '';
                Args.Checksum logical  = true;
                Args.Warn logical      = true;
            end

            Current = catsHTM.catalogSignature(CatName, 'CatDir', Args.CatDir, ...
                'Checksum', Args.Checksum);

            LayoutMatch = strcmp(Current.LayoutHash, Stored.LayoutHash);
            ColMatch    = strcmp(Current.ColHash,    Stored.ColHash);
            HaveChk     = ~isempty(Current.ChecksumHash) && isfield(Stored, 'ChecksumHash') ...
                          && ~isempty(Stored.ChecksumHash);
            if HaveChk
                ChecksumMatch = strcmp(Current.ChecksumHash, Stored.ChecksumHash);
            else
                ChecksumMatch = [];   % undeterminable
            end

            if ~LayoutMatch
                Status  = 'stale-layout';
                Message = sprintf(['catsHTM catalog %s layout changed since the pointers ', ...
                    'were stamped (rows added/removed or re-ingested). Stored ', ...
                    '(CellID,RowInCell)/CatRowID pointers are INVALID - re-run crossIDCatsHTM.'], CatName);
            elseif ~ColMatch
                Status  = 'columns-changed';
                Message = sprintf(['catsHTM catalog %s columns changed since stamping ', ...
                    '(e.g. insertColumns); row pointers remain VALID. Confirm column ', ...
                    'names before gathering by name.'], CatName);
            elseif ~isempty(ChecksumMatch) && ~ChecksumMatch
                Status  = 'stale-suspect';
                Message = sprintf(['catsHTM catalog %s layout counts match but its content ', ...
                    'checksum changed; possible in-cell reordering. Pointers may be ', ...
                    'unreliable - deep-verify recommended.'], CatName);
            else
                Status  = 'valid';
                Message = sprintf('catsHTM catalog %s matches the stored signature.', CatName);
            end

            Ok = any(strcmp(Status, {'valid', 'columns-changed'}));

            Report = struct('Status', Status, 'Ok', Ok, 'LayoutMatch', LayoutMatch, ...
                'ColMatch', ColMatch, 'ChecksumMatch', ChecksumMatch, ...
                'Message', Message, 'Stored', Stored, 'Current', Current);

            if Args.Warn && ~Ok
                warning('catsHTM:checkCatalogSignature:stale', '%s', Message);
            end
        end

        function [CatRowID, Offset] = catRowID(CatName, CellID, RowInCell, Args)
            % Map a (CellID, RowInCell) storage pointer to a single scalar id.
            % Package: @catsHTM
            % Description: Collapse the two-part storage address returned by
            %              catsHTM.sourcePointer into ONE contiguous 1-based
            %              index over the whole catalog. Cells are ordered by
            %              ascending HTM id and each cell's rows occupy a
            %              contiguous block, so
            %                 CatRowID = Offset(CellID) + RowInCell
            %              where Offset(CellID) is the number of sources in all
            %              lower-id cells. Like the pointer pair this is unique
            %              within the catalog and query-independent; unlike it,
            %              it is a single integer (convenient as a join key).
            %              NOTE: it is version-bound - re-ingesting the catalog
            %              renumbers the blocks - so stamp persisted ids with
            %              the catalog version.
            % Input  : - Catalog base name (e.g., 'PS1').
            %          - CellID    - HTM leaf-cell id(s) (from sourcePointer).
            %          - RowInCell - Row within htm_<CellID> (from sourcePointer).
            %          * ...,key,val,...
            %            'CatDir' - Directory holding the catalog HDF5 files.
            %                   Default '' = resolve via which() on the path.
            %            'Nsrc' - Precomputed [CellID Nsrc] table (from
            %                   catsHTM.getNsrcMeta) to skip the metadata scan
            %                   when mapping many pointers. Default [] = scan.
            % Output : - CatRowID - contiguous scalar id per source (NaN where
            %                   CellID/RowInCell is NaN or the cell is absent).
            %          - Offset   - [CellID, BlockStart] table (0-based start of
            %                   each cell's block) - useful for inspection or
            %                   repeated mapping.
            % Author : Dana Kovaleva (Jul 2026)
            % Example: [Cid,Row] = catsHTM.sourcePointer('APASS', 1, 1);
            %          Gid        = catsHTM.catRowID('APASS', Cid, Row);
            arguments
                CatName
                CellID
                RowInCell
                Args.CatDir = '';
                Args.Nsrc   = [];
            end

            % per-cell source counts, ordered by ascending HTM cell id
            NsrcTable = Args.Nsrc;
            if isempty(NsrcTable)
                NsrcTable = catsHTM.getNsrcMeta(CatName, 'CatDir', Args.CatDir);
            end
            NsrcTable = sortrows(NsrcTable, 1);
            Cells     = NsrcTable(:,1);
            Counts    = NsrcTable(:,2);
            % block start (0-based) of each cell = sources in all earlier cells
            OffCol    = [0; cumsum(Counts(1:end-1))];
            Offset    = [Cells, OffCol];

            CellID    = CellID(:);
            RowInCell = RowInCell(:);
            Npt       = numel(CellID);
            OffPer    = nan(Npt, 1);
            [Tf, Loc] = ismember(CellID, Cells);     % Tf is false for NaN CellID
            OffPer(Tf) = OffCol(Loc(Tf));
            CatRowID  = OffPer + RowInCell;          % NaN if cell absent or row NaN
        end

        function [CellID, RowInCell] = catRowID2Pointer(CatName, CatRowID, Args)
            % Invert a scalar CatRowID back to a (CellID, RowInCell) pointer.
            % Package: @catsHTM
            % Description: The exact inverse of catsHTM.catRowID. Because the
            %              global id lays each cell's rows out as contiguous,
            %              non-overlapping blocks ordered by ascending HTM id
            %              (tiling 1..Ntot with no gaps), a scalar id resolves
            %              deterministically to its storage address: find the
            %              block it falls in, then subtract the block start.
            %              Round-trips exactly with catRowID against the SAME
            %              catalog version (the block layout is version-bound).
            % Input  : - Catalog base name (e.g., 'PS1').
            %          - CatRowID - scalar id(s) from catsHTM.catRowID
            %            (NaN or out-of-range values map to NaN pointers).
            %          * ...,key,val,...
            %            'CatDir' - Directory holding the catalog HDF5 files.
            %                   Default '' = resolve via which() on the path.
            %            'Nsrc' - Precomputed [CellID Nsrc] table (from
            %                   catsHTM.getNsrcMeta) to skip the metadata scan.
            %                   Default [] = scan.
            % Output : - CellID    - HTM leaf-cell id per id (NaN if invalid).
            %          - RowInCell - Row within htm_<CellID> per id (NaN if
            %                        invalid).
            % Author : Dana Kovaleva (Jul 2026)
            % Example: Gid          = catsHTM.catRowID('APASS', 12, 3);
            %          [Cid,Row]     = catsHTM.catRowID2Pointer('APASS', Gid);
            arguments
                CatName
                CatRowID
                Args.CatDir = '';
                Args.Nsrc   = [];
            end

            % per-cell source counts, ordered by ascending HTM cell id
            NsrcTable = Args.Nsrc;
            if isempty(NsrcTable)
                NsrcTable = catsHTM.getNsrcMeta(CatName, 'CatDir', Args.CatDir);
            end
            NsrcTable = sortrows(NsrcTable, 1);
            Cells     = NsrcTable(:,1);
            Counts    = NsrcTable(:,2);
            % block edges: id g in cell k iff Edges(k) < g <= Edges(k+1)
            Edges     = [0; cumsum(Counts)];
            Ntot      = Edges(end);

            CatRowID  = CatRowID(:);
            Npt       = numel(CatRowID);
            CellID    = nan(Npt, 1);
            RowInCell = nan(Npt, 1);
            % valid ids are integers in 1..Ntot
            Valid     = ~isnan(CatRowID) & CatRowID >= 1 & CatRowID <= Ntot & ...
                        (CatRowID == round(CatRowID));
            if any(Valid)
                Bin              = discretize(CatRowID(Valid), Edges, 'IncludedEdge','right');
                CellID(Valid)    = Cells(Bin);
                RowInCell(Valid) = CatRowID(Valid) - Edges(Bin);
            end
        end

        function [Data, ColNames] = gatherByPointer(CatName, CellID, RowInCell, Args)
            % Read catalog rows addressed by (CellID, RowInCell) pointers.
            % Package: @catsHTM
            % Description: Given storage pointers (from catsHTM.sourcePointer,
            %              or from catsHTM.catRowID2Pointer on a stored scalar
            %              id), fetch the actual source rows from the catsHTM
            %              HDF5 files WITHOUT a cone_search and without keeping
            %              the catalog in memory. Pointers are grouped by cell
            %              so each htm_<CellID> dataset is loaded at most once.
            %              This is what lets crossIDCatsHTM / gatherCrossIDData
            %              re-fetch ANY column later from just the stored
            %              pointer. Rows with a NaN pointer come back as the
            %              FillValue.
            % Input  : - Catalog base name (e.g., 'PS1').
            %          - CellID    - HTM leaf-cell id(s).
            %          - RowInCell - Row within htm_<CellID> (same size).
            %          * ...,key,val,...
            %            'Columns' - Which columns to return: a cellstr of
            %                   column names, a numeric vector of indices, or {}
            %                   for ALL columns. Default {} (all).
            %            'CatDir' - Directory holding the catalog HDF5 files.
            %                   Default '' = resolve via which() on the path.
            %            'NfilesInHDF' - Datasets per HDF5 file. Default is 100.
            %            'FillValue' - Value for NaN-pointer rows. Default NaN.
            %            'Signature' - A stored catsHTM.catalogSignature struct
            %                   (e.g. Summary.Signature.<Cat>) captured when the
            %                   pointers were made. When given and ValidateSig is
            %                   true, the catalog is checked before dereferencing.
            %                   Default [] = no check.
            %            'ValidateSig' - When a Signature is supplied, validate the
            %                   catalog against it: error on a changed row layout
            %                   (pointers invalid), warn on a suspect content
            %                   change. Default true. Has no effect without a
            %                   Signature (so existing callers are unaffected).
            % Output : - Data     - [Npt x Ncol] matrix of the requested columns
            %                        in input (pointer) order.
            %          - ColNames - cellstr of the returned column names.
            % Author : Dana Kovaleva (Jul 2026)
            % See also: catsHTM.catalogSignature, catsHTM.checkCatalogSignature.
            % Example: [Cid,Row] = catsHTM.sourcePointer('APASS', RA, Dec);
            %          [D,Cols]   = catsHTM.gatherByPointer('APASS', Cid, Row, ...
            %                           'Columns',{'RA','Dec','Mag_V'});
            arguments
                CatName
                CellID
                RowInCell
                Args.Columns          = {};
                Args.CatDir           = '';
                Args.NfilesInHDF      = 100;
                Args.FillValue        = NaN;
                Args.Signature        = [];
                Args.ValidateSig logical = true;
            end

            % resolve the catalog directory (which() checks the cwd too), then
            % read the column names from the colcell in that same directory.
            CatDir = Args.CatDir;
            if isempty(CatDir)
                ColCellFull = which(sprintf('%s_htmColCell.mat', CatName));
                if isempty(ColCellFull)
                    error('catsHTM:gatherByPointer:noCatalog', ...
                        'Cannot find %s_htmColCell.mat on the MATLAB path (add the catalog directory or pass CatDir).', ...
                        CatName);
                end
                CatDir = fileparts(ColCellFull);
            end

            % validate stored pointers against the catalog's current build: a
            % changed row layout means the pointers now address different sources.
            if Args.ValidateSig && ~isempty(Args.Signature)
                [SigOk, SigRep] = catsHTM.checkCatalogSignature(CatName, Args.Signature, ...
                    'CatDir', CatDir, 'Warn', false);
                if strcmp(SigRep.Status, 'stale-layout')
                    error('catsHTM:gatherByPointer:staleSignature', '%s', SigRep.Message);
                elseif ~SigOk
                    warning('catsHTM:gatherByPointer:suspectSignature', '%s', SigRep.Message);
                end
            end

            AllCols = catsHTM.load_colcell_from_dir(CatDir, CatName);
            AllCols = AllCols(:).';

            % resolve requested columns to indices (into the full column set)
            if isempty(Args.Columns)
                ColIdx = 1:numel(AllCols);
            elseif isnumeric(Args.Columns)
                ColIdx = Args.Columns(:).';
            else
                Wanted = Args.Columns;
                if ischar(Wanted) || isstring(Wanted)
                    Wanted = cellstr(Wanted);
                end
                [Tf, Loc] = ismember(Wanted, AllCols);
                if ~all(Tf)
                    error('catsHTM:gatherByPointer:badColumn', ...
                        'Unknown column(s) in %s: %s', CatName, strjoin(Wanted(~Tf), ', '));
                end
                ColIdx = Loc(:).';
            end
            ColNames = AllCols(ColIdx);

            CellID    = CellID(:);
            RowInCell = RowInCell(:);
            Npt       = numel(CellID);
            Data      = repmat(Args.FillValue, Npt, numel(ColIdx));

            % group pointers by cell so each dataset is read once
            Good      = find(~isnan(CellID) & ~isnan(RowInCell));
            UniqCells = unique(CellID(Good));
            for Ic = 1:1:numel(UniqCells)
                CID      = UniqCells(Ic);
                FileID   = floor(CID./Args.NfilesInHDF).*Args.NfilesInHDF;
                FileName = fullfile(CatDir, sprintf('%s_htm_%06d.hdf5', CatName, FileID));
                DataName = sprintf('htm_%06d', CID);
                % catsHTM stores cells rows=sources; HDF5.load already returns
                % [Nsrc x Ncol] (see catsHTM.load_cat) - do NOT transpose.
                CellData = HDF5.load(FileName, DataName);   % [Nsrc x Ncol]
                Rows     = Good(CellID(Good) == CID);
                Data(Rows, :) = CellData(RowInCell(Rows), ColIdx);
            end
        end

        function [CellID, RowInCell, Dist, CatRowID] = sourcePointer(CatName, RA, Dec, Args)
            % Stable per-source storage pointer in a catsHTM catalog.
            % Package: @catsHTM
            % Description: For each input coordinate return the intrinsic
            %              storage address of the source in a catsHTM catalog:
            %              the HTM leaf-cell id and the row index within that
            %              cell's dataset (htm_<CellID>). Unlike a cone_search
            %              row index (relative to a particular query), this pair
            %              is unique within the catalog, stable (the HDF5 files
            %              are static) and independent of any query - a genuine
            %              pointer to the source. Native source-id columns are
            %              unreliable across catsHTM (all columns are stored as
            %              double), so this is the recommended stable key.
            % Input  : - Catalog name (e.g., 'PS1').
            %          - J2000 R.A. [radians] (vector).
            %          - J2000 Dec. [radians] (vector).
            %          * ...,key,val,...
            %            'SearchRadius' - Radius used to locate the containing
            %                   HTM cell(s). Default is 2.
            %            'SearchRadiusUnits' - Default is 'arcsec'.
            %            'MaxDist' - Maximum allowed source-match distance;
            %                   beyond it the pointer is NaN. Default is 2.
            %            'MaxDistUnits' - Default is 'arcsec'.
            %            'NfilesInHDF' - Datasets per HDF5 file. Default is 100.
            %            'ColRA' - RA column index in the catalog. Default is 1.
            %            'ColDec' - Dec column index in the catalog. Default 2.
            % Output : - CellID    - HTM leaf-cell id per source (NaN if no
            %                        source found within MaxDist).
            %          - RowInCell - Row index within htm_<CellID> per source.
            %          - Dist      - Match distance [arcsec] (NaN if none).
            %          - CatRowID  - Optional 4th output: the (CellID,RowInCell)
            %                        pair collapsed to a single contiguous
            %                        catalog-wide scalar id (via
            %                        catsHTM.catRowID). Requesting it triggers
            %                        a one-off metadata scan of the whole
            %                        catalog, so it is computed only when asked.
            % Author : Dana Kovaleva (Jul 2026)
            % Example: [Cid,Row] = catsHTM.sourcePointer('APASS', 1, 1);
            %          [Cid,Row,~,Gid] = catsHTM.sourcePointer('APASS', 1, 1);
            arguments
                CatName
                RA
                Dec
                Args.SearchRadius        = 2;
                Args.SearchRadiusUnits   = 'arcsec';
                Args.MaxDist             = 2;
                Args.MaxDistUnits        = 'arcsec';
                Args.NfilesInHDF         = 100;
                Args.ColRA               = 1;
                Args.ColDec              = 2;
            end

            RA  = RA(:);
            Dec = Dec(:);
            Npt = numel(RA);

            % Anchor everything to the catalog directory. H5F.open does NOT
            % search the MATLAB path, so resolve the dir via the colcell .mat
            % (which() reliably finds .mat files) and build full paths from it;
            % the index and data HDF5 files live in that same directory.
            ColCellFull = which(sprintf('%s_htmColCell.mat', CatName));
            if isempty(ColCellFull)
                error('catsHTM:sourcePointer:noCatalog', ...
                    'Cannot find %s_htmColCell.mat on the MATLAB path (add the catalog directory).', ...
                    CatName);
            end
            CatDir = fileparts(ColCellFull);
            % search_htm_ind derives the var name by splitting the filename on
            % '_', which breaks for a full path, so pass it explicitly.
            [IndexFileName, IndexVarName] = catsHTM.get_index_filename(CatName);
            IndexFull = fullfile(CatDir, IndexFileName);

            SearchRad = convert.angular(Args.SearchRadiusUnits, 'rad', Args.SearchRadius);
            MaxDistR  = convert.angular(Args.MaxDistUnits,      'rad', Args.MaxDist);

            CellID    = nan(Npt,1);
            RowInCell = nan(Npt,1);
            BestDist  = inf(Npt,1);
            Cache     = containers.Map('KeyType','double','ValueType','any');

            for Ipt = 1:1:Npt
                Cands = catsHTM.search_htm_ind(IndexFull, IndexVarName, RA(Ipt), Dec(Ipt), SearchRad);
                for Ic = 1:1:numel(Cands)
                    CID = Cands(Ic);
                    if ~isKey(Cache, CID)
                        FileID   = floor(CID./Args.NfilesInHDF).*Args.NfilesInHDF;
                        FileName = fullfile(CatDir, sprintf('%s_htm_%06d.hdf5', CatName, FileID));
                        DataName = sprintf('htm_%06d', CID);
                        % catsHTM stores cells rows=sources; HDF5.load already
                        % returns [Nsrc x Ncol] (see catsHTM.load_cat) - do NOT
                        % transpose, or the RA/Dec columns get scrambled.
                        Cache(CID) = HDF5.load(FileName, DataName);   % [Nsrc x Ncol]
                    end
                    Data = Cache(CID);
                    if ~isempty(Data)
                        D = celestial.coo.sphere_dist_fast(RA(Ipt), Dec(Ipt), ...
                                Data(:,Args.ColRA), Data(:,Args.ColDec));
                        [Dmin, Imin] = min(D);
                        if Dmin < BestDist(Ipt)
                            BestDist(Ipt)  = Dmin;
                            CellID(Ipt)    = CID;
                            RowInCell(Ipt) = Imin;
                        end
                    end
                end
            end

            Bad            = BestDist > MaxDistR;
            CellID(Bad)    = NaN;
            RowInCell(Bad) = NaN;
            Dist           = convert.angular('rad', 'arcsec', BestDist);
            Dist(Bad)      = NaN;

            % Optional scalar id. Reuse the already-resolved CatDir so
            % catRowID skips a second which() lookup. Computed only when
            % requested (it scans the whole catalog's metadata once).
            if nargout >= 4
                CatRowID = catsHTM.catRowID(CatName, CellID, RowInCell, 'CatDir', CatDir);
            end
        end

        function [Nsrc,SumN]=nsrc(CatName)
            % Count sources in the HDF5/HTM index file
            % Package: @catsHTM
            % Description: Count sources in the HDF5/HTM index file
            % Input  : - Catalog name (e.g., 'SDSSDR10').
            % Output : - Matrix of [HTMindex, Nsrc].
            %          - Total number of sources in catalog.
            % Example: [Nsrc,SumN]=catsHTM.nsrc('SDSSDR10');
            % Reliable: 2
            
            FileName = sprintf('%s_htm.hdf5',CatName);
            DataName = sprintf('%s_HTM',CatName);
            %HTM = catsHTM.load_htm_ind(FileName);
            Data = HDF5.load(FileName,DataName);
            
            Nsrc = Data(:,[2 13]);
            SumN = nansum(Nsrc(:,2));
            
        end
        
        function Ref=reference(CatName)
            % Get references for an HDF5/HTM catalog
            % Package: @catsHTM
            % Description: Get references for an HDF5/HTM catalog
            % Input  : - Catalog base name (e.g., 'GAIADR1').
            % Output : - Structure containing reference and acknowledgment
            %            for the catalog.
            % Example: catsHTM.reference('SDSSDR10')
            %
            
            switch lower(CatName)
                case 'SDSSDR10'
                    Ref.CatName = 'SDSSDR10';
                    Ref.Name    = 'SDSS-DR10 sources';
                    Ref.Ref{1}  = 'Ahn et al. (2014)';
                    Ref.Link{1} = 'http://adsabs.harvard.edu/abs/2014ApJS..211...17A';
                    Ref.Ack     = 'http://www.sdss3.org/collaboration/boiler-plate.php';
                case 'GAIADR1'
                case 'GALEX'
                case 'DECaLS'
                case 'TMASS'
                case 'WISE'
                case 'FIRST'
                case 'NVSS'
                case 'APASS'
                case 'UCAC4'
                case 'XMM'
                    
                otherwise
                    error('Unknown CatName option');
            end
            Ref.CatAck  = 'HDF5/HTM catalog from Ofek (2018)';
            
        end
        
    end
    
    % search
    methods (Static)
        function [Cat,ColCell,ColUnits,D]=cone_search(CatName,RA,Dec,Radius,Args)
            % Cone earch on local HDF5/HTM catalog
            % Package: @catsHTM
            % Description: Perform a cone search around RA/Dec on a local catalog in
            %              HDF5 format soNrted into HTM.
            % Input  : - Catalog name (e.g., 'GAIADR1').
            %            see VO.search.htmcat_names for options.
            %          - J2000.0 R.A. [radians, [H M S], or sexagesimal string].
            %          - J2000.0 Dec. [radians, [sign D M S], or sexagesimal string].
            %          - Search radius [arcsec].
            %          * Arbitrary number of pairs of arguments: ...,keyword,value,...
            %            where keyword are one of the followings:
            %            'Con'         - A cell array of additional
            %                            constraints to apply to output catalog.
            %                            Each cell contains a two element
            %                            cell array in which the first
            %                            element is a column name on which
            %                            to apply the constraint. The
            %                            second element is either a two
            %                            element vector of [min, max] range
            %                            to select, or a function handle
            %                            that get the column and return
            %                            logical.
            %                            E.g., {{'Mag_G',[15 16]},{'Plx',@(x) ~isnan(x)}}
            %                            will select sources with mag
            %                            between 15 and 16 and not NaN
            %                            parallax.
            %            'RadiusUnits' - Radius units. Default is 'arcsec'.
            %            'IndexFileTemplate' - Index Catalog name template.
            %                            Default is '%s_htm.hdf5'.
            %            'CatFileTemplate' - Catalog name template.
            %                            Default is '%s_htm_%06d.hdf5'.
            %            'htmTemplate' - HTM dataset template name.
            %                            Default is 'htm_%06d'.
            %            'NcatInFile'  - Number of Datasets in file.
            %                            Default is 100.
            %            'IndexVarName' - Default is [].
            %            'UseIndex'    - A logical indicating if to use
            %                            the index HDF file.
            %                            For very big catalogs, will be
            %                            faster to use true.
            %                            Default is false.
            %            'ColRA'       - Default is 1.
            %            'ColDec'      - Default is2.
            %            'OnlyCone'    - Return only sources within cone.
            %                            If false will return also some
            %                            objects outside cone.
            %                            Default is true.
            %            'ColCellFile' - Default is '%s_htmColCell.mat'.
            %            'OutType'     - Output type {'mat'|'astcat'|'catcl'|'astrocatalog'|'table'}.
            %                            Default is 'mat'.
            % Output : - Catalog of source within cone.
            %          - Cell array of column names.
            %          - Cell array of units.
            %          - Vector of distances of sources to search
            %            coordinates [radians].
            % License: GNU general public license version 3
            %     By : Eran O. Ofek                    Dec 2017
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Example: Cat=catsHTM.cone_search('UCAC4',1,1,10);
            %          Cat=catsHTM.cone_search('GAIADR1',1,1,10);
            %          Cat=catsHTM.cone_search('GALEX',1,1,10);
            % Reliable: 2
            %--------------------------------------------------------------------------

            arguments
                CatName
                RA
                Dec
                Radius
                Args.Con                  = {};
                Args.RadiusUnits          = 'arcsec';  % do not change this default!
                Args.IndexFileTemplate    = '%s_htm.hdf5';
                Args.CatFileTemplate      = '%s_htm_%06d.hdf5';
                Args.htmTemplate          = 'htm_%06d';
                Args.NcatInFile           = 100;
                Args.IndexVarName         = [];
                Args.UseIndex             = false;
                Args.ColRA                = 1;
                Args.ColDec               = 2;
                Args.OnlyCone             = true;
                Args.ColCellFile          = '%s_htmColCell.mat';
                Args.OutType              = 'mat';
            end
            
            RAD = 180./pi;

            %if nargin<5
            %    Radius = Radius./(RAD.*3600);  % arcsec to [radians]
            %else
            Radius = convert.angular(Args.RadiusUnits,'rad',Radius);  % [radians]
            %end

            if (ischar(RA))
                RA = celestial.coo.convertdms(RA,'SH','r');
            end
            if (ischar(Dec))
                Dec = celestial.coo.convertdms(Dec,'SD','R');
            end
            
            Args.ColCellFile = sprintf(Args.ColCellFile,CatName);

            io.files.load1(Args.ColCellFile);
            Ncol  = numel(ColCell);

            % number of additional constraints
            Ncon  = numel(Args.Con);

            MinDec = Dec - Radius;
            MaxDec = Dec + Radius;

            IndexFileName = sprintf(Args.IndexFileTemplate,CatName);
            ID     = catsHTM.search_htm_ind(IndexFileName,Args.IndexVarName,RA,Dec,Radius);
            FileID = floor(ID./Args.NcatInFile).*Args.NcatInFile;
            Nid = numel(ID);
            Cat = zeros(0,Ncol);
            C = tools.struct.struct_def({'Cat'},Nid,1);
            for Iid=1:1:Nid

                %FileID    = floor(ID(Iid)./Args.NcatInFile).*Args.NcatInFile;
                FileName  = sprintf(Args.CatFileTemplate,CatName,FileID(Iid));
                DataName  = sprintf(Args.htmTemplate,ID(Iid));
                %Cat = [Cat; catsHTM.load_cat(FileName,DataName,[MinDec, MaxDec],Ncol)];
                if Args.UseIndex
                    C(Iid).Cat = catsHTM.load_cat(FileName,DataName,[MinDec, MaxDec],Ncol).';
                else
                    C(Iid).Cat = HDF5.load(FileName,DataName).';
                end
                
                if ~isempty(Args.Con)
                    Flag = true(1,size(C(Iid).Cat,2));
                    for Icon=1:1:Ncon
                        ColInd = strcmp(Args.Con{Icon}{1},ColCell);
                        if isa(Args.Con{Icon}{2},'function_handle')
                            Flag = Flag & Args.Con{Icon}{2}(C(Iid).Cat(ColInd,:));
                        else
                            Flag = Flag & C(Iid).Cat(ColInd,:)>=Args.Con{Icon}{2}(1) & C(Iid).Cat(ColInd,:)<=Args.Con{Icon}{2}(2);
                        end
                    end
                    C(Iid).Cat = C(Iid).Cat(:,Flag);
                end
                

                
%                 if (Iid==1)
%                     if Args.UseIndex
%                         Cat = catsHTM.load_cat(FileName,DataName,[MinDec, MaxDec],Ncol);
%                     else
%                         Cat = HDF5.load(FileName,DataName);
%                     end
%
%                     %Ncol = size(Cat,2);
%                 else
%                     if Args.UseIndex
%                         Cat = [Cat; catsHTM.load_cat(FileName,DataName,[MinDec, MaxDec],Ncol)];
%                     else
%                         Cat = [Cat; HDF5.load(FileName,DataName)];
%                     end
%                 end

                %C(Iid).Cat = catsHTM.load_cat(FileName,DataName,[MinDec, MaxDec],Ncol).';

            end

            Cat = [C.Cat]';

            % select only sources in Cone
            if (Args.OnlyCone && ~isempty(Cat))
                D = celestial.coo.sphere_dist_fast(RA,Dec,Cat(:,Args.ColRA),Cat(:,Args.ColDec));
                Cat = Cat(D<Radius,:);
                if nargout>3
                    D   = D(D<Radius);
                end
            else
                D = NaN;
            end



            switch lower(Args.OutType)
                case 'mat'
                    % do nothing
                case 'astrocatalog'
                    AstC = AstroCatalog;
                    %AstC.CooType  = 'sphere';
                    %AstC.CooUnits = 'rad';
                    AstC.Catalog  = Cat;
                    AstC.ColNames = ColCell;
                    AstC.ColUnits = ColUnits;
                    Cat = AstC;
                case 'table'
                    Cat = array2table(Cat);
                    if isempty(ColUnits)
                        ColUnits = {};
                    end
                    if isempty(ColCell)
                        ColCell = {};
                    end
                    Cat.Properties.VariableNames = ColCell;                    
                    Cat.Properties.VariableUnits = ColUnits;
                case 'catcl'
                    AstC = catCl;
                    AstC.Cat = Cat;
                    AstC.ColCell  = ColCell;
                    AstC.ColUnits = ColUnits;
                    Cat = AstC;
                case 'astcat'
                    AstC = AstCat;
                    AstC.Cat = Cat;
                    AstC.ColCell = ColCell;
                    AstC = colcell2col(AstC);
                    Cat  = AstC;
                otherwise
                    error('Unknown OutType option');
            end
            
        end

        
  
        
        
        function CatM=sources_match(CatName,Cat,Args)
            % Match sources in an input catalog with catsHTM catalog
            % Package: @catsHTM
            % Description: Given a catalog of sources with their RA/Dec,
            %              match each one of them to a source in an
            %              catsHTM catalog.
            % Input  : - catsHTM catalog name (e.g., 'UCAC4').
            %          - An AstCat object with sources.
            %          * Arbitrary number of key,val pairs:
            %            'ConeSearchPar' - A cell array of additional
            %                       arguments to pass to cone_search.m.
            %                       E.g.,  {{'Mag_G',[15 16]},{'Plx',@(x) ~isnan(x)}}
            %                       Default is {}.
            %            'OutType' - Output catalog type {'mat'|'astcat'}.
            %                       Default is 'AstCat'.
            %            'SearchRadius' - Search radius. Default is 2.
            %            'SearchRadiusUnits' - Search radius units.
            %                       Default is 'arcsec'.
            %            'ColCell' - Default is {}.
            %            'ColRA' - Default is {'RA','ALPHAWIN_J2000'}.
            %            'ColDec' - Default is {'Dec','DELTAWIN_J2000'}.
            %            'CooUnits' - Input catalog coordinates units.
            %                       Default is 'rad'.
            %            'ColDecHTM' - Default is 2.
            %            'ColRAHTM'  - Default is 1.
            % Output : - A matched catalog.
            % Example: CatM=catsHTM.sources_match('GAIADR2',CoaddSim);
            
            arguments
                CatName
                Cat
                Args.ConeSearchPar         = {};
                Args.OutType               = 'AstCat';
                Args.SearchRadius          = 2;
                Args.SearchRadiusUnits     = 'arcsec';
                Args.ColCell               = {};
                Args.ColRA                 = {'RA','ALPHAWIN_J2000'};
                Args.ColDec                = {'Dec','DELTAWIN_J2000'};
                Args.CooUnits              = 'rad';  % in the AstCat object
                Args.ColDecHTM             = 2;
                Args.ColRAHTM              = 1;
            end
            
            CatField     = AstCat.CatField;
            ColCellField = AstCat.ColCellField;
            
            Args.SearchRadius = convert.angular(Args.SearchRadiusUnits,'rad',Args.SearchRadius);  % [rad]
            
            % Convert input catalog to an AstCat object
            if (~AstCat.isastcat(Cat))
                Tmp = Cat;
                Cat = AstCat;
                Cat.(CatField)     = Tmp;
                Cat.(ColCellField) = Args.ColCell;
                Cat                = colcell2col(Cat);
            end
            % RA/Dec columns
            [~,Col.RA,~]     = select_exist_colnames(Cat,Args.ColRA(:));
            [~,Col.Dec,~]    = select_exist_colnames(Cat,Args.ColDec(:));
            
            RA  = Cat.(CatField)(:,Col.RA);
            Dec = Cat.(CatField)(:,Col.Dec);
            % convert to radians;
            ConvCoo = convert.angular(Args.CooUnits,'rad');
            RA      = RA.*ConvCoo;
            Dec     = Dec.*ConvCoo;
            
            MedRA   = nanmedian(RA);
            MedDec  = nanmedian(Dec);
            D       = celestial.coo.sphere_dist_fast(MedRA,MedDec,RA,Dec);
            Radius  = max(D).*(1+10.*eps);  % [rad]
            Radius  = convert.angular('rad','arcsec',Radius); % [arcsec]
            
            [CatH,ColCellH] = catsHTM.cone_search(CatName,MedRA,MedDec,Radius,Args.ConeSearchPar{:});
            
            
            CatH = sortrows(CatH,Args.ColDecHTM);
            
            Nsrc = size(Cat.(CatField),1);
            CatM.Match  = nan(Nsrc,numel(ColCellH));
            CatM.Dist   = nan(Nsrc,1);
            CatM.Nmatch = zeros(Nsrc,1);
            if (~isempty(CatH))
                for Isrc=1:1:Nsrc
                    % search match for Cat.Cat(Isrc,:)
                    Ind = VO.search.search_sortedlat(CatH,RA(Isrc),Dec(Isrc),Args.SearchRadius);

                    if (~isempty(Ind))
                        Dist = celestial.coo.sphere_dist_fast(RA(Isrc),Dec(Isrc),CatH(Ind,Args.ColRAHTM),CatH(Ind,Args.ColDecHTM));
                        Nmatch = numel(Ind);
                        if (Nmatch>1)
                            [Dist,MinInd] = min(Dist);
                            Ind = Ind(MinInd);
                        end

                        CatM.Match(Isrc,:) = CatH(Ind,:);
                        CatM.Dist(Isrc)    = Dist;
                        CatM.Nmatch(Isrc)  = Nmatch;
                    end
                end
            end
            CatM.ColCell = ColCellH;
            
            switch lower(Args.OutType)
                case 'astcat'
                    Cat = AstCat;
                    Cat.(CatField) = CatM.Match;
                    Cat.(ColCellField) = CatM.ColCell;
                    Cat = col_insert(Cat,[CatM.Nmatch],numel(CatM.ColCell)+1,'Nmatch');
                    Cat = col_insert(Cat,[CatM.Dist],  numel(CatM.ColCell)+2,'Dist');
                    CatM = Cat;
                case 'mat'
                    % do nothing
                otherwise
                    error('Unknown OutType option');
            end
            
        end
        
        function [ColCell,ConcatRes]=serial_search(CatName,Fun,Args)
            % Execute a function on entire HDF5/HTM catalog
            % Package: @catsHTM
            % Description: Execute a function on entire HDF5/HTM catalog.
            %              This can be used for selection of sources based
            %              on any parameters.
            % Input  : - Catalog name (e.g., 'GAIADR1').
            %          - Function name to execute:
            %            Fun(Cat,FunPar{:})
            %          * Arbitrary number of pairs of arguments: ...,keyword,value,...
            %            where keyword are one of the followings:
            %            'Concat' - Concat results to previous results.
            %                       Default is true.
            %                       Concat result will be outputed as
            %                       second output argument.
            %            'FunPar' - Cell array of additional parameters to
            %                       pass to the function.
            %            'NparPool' - Number of parallel processes to run.
            %                       Default is 24.
            %            'Xmatch'   - A logical flag indicating if to
            %                       prepare a list of all sources in the
            %                       current HTM and neighbooring HTMs.
            %                       This will be used by FunX.
            %                       Default is false.
            %            'FunX'     - A function to call if Xmatch is true.
            %                       Default is [].
            %                       FunX(Cat,CatNeigh,varargin)
            %            'FunXPar'  - A cell array of key,val atruments to
            %                       pass to FunX as additional parameters.
            %            'SearchRadius' - Search radius for FunX. Default
            %                       is 100 arcsec.
            %            'SearchRadiusUnits' - Default is 'arcsec'.
            %            'Verbose' - Default is true.
            % Output : - Cell array of column names in catalog.
            %          - Optional concat results.
            % Example: [ColCell]=catsHTM.serial_search('APASS',@sin)
            % Reliable: 2
            
            arguments
                CatName
                Fun
                Args.Concat                = true;
                Args.FunPar                = {};
                Args.NparPool              = 24;
                Args.Xmatch                = false;
                Args.FunX                  = [];
                Args.FunXPar               = {};
                Args.SearchRadius         = 100;  % [arcsec]
                Args.SearchRadiusUnits    = 'arcsec';
                Args.ColDec               = 2;
                Args.Verbose              = true;
            end
                        
            SearchRadius = convert.angular(Args.SearchRadiusUnits,'rad',Args.SearchRadius);  % [rad]

            % load HTM data for Cat1
            [IndexFileName,IndexVarName] = catsHTM.get_index_filename(CatName);
            % HTM1 is the HTM index file
            [HTM,DataHTM] = catsHTM.load_htm_ind(IndexFileName,IndexVarName);
            % Level, Father, Son1, Son2, Son3, Son4, Poles 1 long,
            % poles 1 lat, ..., Nsrc

            %
            Nhtm = numel(HTM);
            
            L = celestial.htm.nhtm2level(Nhtm);
            
            [HTM,Level] = celestial.htm.htm_build(L);
            Level = Level(L);
            Nh    = numel(Level.ptr);
            
            [ColCell] = catsHTM.load_colcell(CatName);
            Ncol      = numel(ColCell);
            
            % number of parallel processes
            %parpool(Args.NparPool);
            
            %parfor Ih=1:1:Nh
            %Sum{1}=0;
            %Sum{2}=0;
            %tic;
            First = true;
             for Ih=1:1:Nh
                 %Nh
                 %Ih
%                  if (Ih./1000)==floor(Ih./1000)
%                     Ih
%                     toc
%                     tic;
%                  end
                % for each HTM in Cat1
                %Cat1    = [];
                Ihtm   = Level.ptr(Ih);
                
                % if HTM in Cat1 contain sources
                if (DataHTM(Ihtm,13)>0)
                    % load Cat
      
                    Cat = catsHTM.load_cat(CatName,Ihtm);
                    
                   
                    if (~isempty(Fun))
                        if (Args.Concat)
                            CR = Fun(Cat,Ihtm,Args.FunPar{:});
                            if (Args.Verbose)
                                fprintf('HTM index: %d    Number of objects: %d\n',Ih,size(CR,1));
                            end
                            if ~isempty(CR)
                                if (First)
                                    First = false;
                                    ConcatRes(1).Cat = CR.';
                                else
                                    ConcatRes(end+1).Cat = CR.';
                                end
                            end
                        else
                            Fun(Cat,Args.FunPar{:});
                        end
                    end
                    
                end
            end
            
        end
        
        
        function [ColCell,ConCat]=serial_search_x(CatName,Fun,Args)
            % Execute a function on entire HDF5/HTM catalog
            % Package: @catsHTM
            % Description: Execute a function on entire HDF5/HTM catalog.
            %              This can be used for selection of sources based
            %              on any parameters.
            % Input  : - Catalog name (e.g., 'GAIADR1').
            %          - Function name to execute:
            %            Fun(Cat,FunPar{:})
            %          * Arbitrary number of pairs of arguments: ...,keyword,value,...
            %            where keyword are one of the followings:
            %            'Istart' - Default is 1.
            %            'Iend'   - Default is inf.
            %            'FunPar' - Cell array of additional parameters to
            %                       pass to the function.
            %            'NparPool' - Number of parallel processes to run.
            %                       Default is 24.
            %            'Xmatch'   - A logical flag indicating if to
            %                       prepare a list of all sources in the
            %                       current HTM and neighbooring HTMs.
            %                       This will be used by FunX.
            %                       Default is false.
            %            'FunX'     - A function to call if Xmatch is true.
            %                       Default is [].
            %                       FunX(Cat,CatNeigh,varargin)
            %            'FunXPar'  - A cell array of key,val atruments to
            %                       pass to FunX as additional parameters.
            %            'SearchRadius' - Search radius for FunX. Default
            %                       is 100 arcsec.
            %            'SearchRadiusUnits' - Default is 'arcsec'.
            % Output : - Cell array of column names in catalog.
            % Example: catsHTM.serial_search_x('GAIADR2',[],'FunX',@search_allml,'Xmatch',true)
            %          [~,ConCat]=catsHTM.serial_search_x('LAMOSTDR6',[],'FunX',@search_duplicate,'Xmatch',true)
            % Reliable: 2
            
            arguments
                CatName
                Fun
                Args.Istart                = 1;
                Args.Iend                  = Inf;
                Args.FunPar                = {};
                Args.NparPool              = 24;
                Args.Xmatch                = false;
                Args.FunX                  = [];
                Args.FunXPar               = {};
                Args.SearchRadius         = 100;  % [arcsec]
                Args.SearchRadiusUnits    = 'arcsec';
                Args.ColDec               = 2;
            end
            
            SearchRadius = convert.angular(Args.SearchRadiusUnits,'rad',Args.SearchRadius);  % [rad]

            % load HTM data for Cat1
            [IndexFileName,IndexVarName] = catsHTM.get_index_filename(CatName);
            % HTM1 is the HTM index file
            [HTM,DataHTM] = catsHTM.load_htm_ind(IndexFileName,IndexVarName);
            % Level, Father, Son1, Son2, Son3, Son4, Poles 1 long,
            % poles 1 lat, ..., Nsrc

            %
            Nhtm = numel(HTM);
            
            L = celestial.htm.nhtm2level(Nhtm);
            
            [HTM,Level] = celestial.htm.htm_build(L);
            Level = Level(L);
            Nh    = numel(Level.ptr);
            
            [ColCell] = catsHTM.load_colcell(CatName);
            Ncol      = numel(ColCell);
            
            % number of parallel processes
            %parpool(Args.NparPool);
            
            %parfor Ih=1:1:Nh
            %Sum{1}=0;
            %Sum{2}=0;
            %ResML = [];
            
            if (isinf(Args.Iend))
                % do nothing - use Nh
            else
                Nh = Args.Iend;
            end
            
            
            tic;
            ConCat = [];
             for Ih=Args.Istart:1:Nh
                 %Ih
                 if (Ih./1000)==floor(Ih./1000)
                    Ih
                    toc
                    tic;
                 end
                % for each HTM in Cat1
                %Cat1    = [];
                Ihtm   = Level.ptr(Ih);
                
                % if HTM in Cat1 contain sources
                if (DataHTM(Ihtm,13)>0)
                    % load Cat
      
                    Cat = catsHTM.load_cat(CatName,Ihtm);
                    
                    if (Args.Xmatch)
                        
                        % search for all HTMs in Cat2 that may opverlap with
                        % Cat1 current HTM
                        MeanRA  = mean(HTM(Ihtm).coo(:,1));
                        MeanDec = mean(HTM(Ihtm).coo(:,2));
                        MinDec  = min(HTM(Ihtm).coo(:,2))-SearchRadius;
                        MaxDec  = max(HTM(Ihtm).coo(:,2))+SearchRadius;

                        D = celestial.coo.sphere_dist_fast(MeanRA,MeanDec,HTM(Ihtm).coo(:,1),HTM(Ihtm).coo(:,2));
                        CircRadius = max(D) + SearchRadius; % [rad]

                        ID2 = celestial.htm.htm_search_cone(HTM,MeanRA,MeanDec,CircRadius);

                        % load all ID2 from HTM2
                        Nid2 = numel(ID2);
                        for Iid2=1:1:Nid2
                            if (Iid2==1)
                                [Cat2,Ind2]   = catsHTM.load_cat(CatName,ID2(Iid2),[MinDec MaxDec],Ncol);
                                N2     = size(Cat2,1);
                                Cat2ID = [ID2(Iid2).*ones(N2,1), Ind2-1+(1:1:N2)'];
                            else
                                [Cat2tmp, Ind2] = catsHTM.load_cat(CatName,ID2(Iid2),[MinDec MaxDec],Ncol);
                                Cat2   = [Cat2; Cat2tmp];
                                N2     = size(Cat2,1);
                                Cat2ID = [Cat2ID; [ID2(Iid2).*ones(N2,1), Ind2-1+(1:1:N2)']];
                            end
                        end
   
                        if (isempty(Cat2))
                            % if Cat2 is empty - skip
                        else

                            % sort Cat2 and Cat2ID
                            [Cat2,SI] = sortrows(Cat2,Args.ColDec);
                            %Cat2ID = Cat2ID(SI,:);

                            % cross match Cat1 and Cat2
                            %[Match,Ind,IndCatMinDist] = VO.search.match_cats(Cat2,Cat1,'Radius',SearchRadius,'RadiusUnits','rad');

                            ConCat=Args.FunX(Cat,Cat2,ConCat,ColCell,Args.FunXPar{:});
                            
                        end
                        
                    end
                    
                    %
                    if (~isempty(Fun))
                        Fun(Cat,ColCell,Args.FunPar{:});
                    end
                    
                end
             end   % end of for=Ih loop
             %save Sum.mat Sum
             %save ResML.mat ResML
             
        end
    end
    
    % cross matching
    methods (Static)
        function xmatch_2cats(CatName1,CatName2,Args)
            % Cross match two HDF5/HTM catalogs
            % Package: @catsHTM
            % Description: Cross match two HDF5/HTM catalogs. For each
            %              source in the first catalog the index of the
            %              nearest source, within some distance, in the
            %              second catalog is saved.
            % Input  : - Catalog base name.
            %          - Catalog base name.
            %          * Arbitrary number of pairs of arguments: ...,keyword,value,...
            %            where keyword are one of the followings:
            %            'SearchRadius' - Search radius. Default is 2''.
            %            'SearchRadiusUnits' - Search radius units.
            %                       Default is 'arcsec'.
            %            'QueryFun'    - Optional function handle to
            %                       execute on the matched catalog.
            %                       Syntax:
            %                       Flag = QueryFun(Cat1,Cat2matched,QueryFunPar{:});
            %                       where Flag is a vector logicals
            %                       indicating the selected rows to be
            %                       saved.
            %            'QueryFunPar' - Cell array of additional arguments
            %                       to pass to QueryFun.
            %                       Default is {}.
            %            'SaveFun' - Optional function handle to execute on
            %                       the queried matched catalog.
            %                       SaveFun(Cat1,Cat2matched,SaveFunPar{:});
            %                       E.g., to save the data.
            %            'SaveFunPar' - Cell array of additional arguments
            %                       to pass to SaveFun.
            %                       Default is {}.
            %            'Cat2_ColDec' - Declination column in second
            %                       catalog. Default is 2.
            %            'NparPool' - Number of parallel processes to run.
            %                       Default is 24.
            %            'DeleteParPool' - Delete existing parpool.
            %                       Default is false.
            % Output : null [output is written as an HDF5/HTM catalog].
            % Example: catsHTM.xmatch_2cats('APASS','APASS')
            % Reliable: 2
            
            arguments
                CatName1
                CatName2
                Args.SearchRadius         = 2;  % [arcsec]
                Args.SearchRadiusUnits    = 'arcsec';
                Args.SelfMatch            = false;
                Args.QueryAllFun          = [];
                Args.QueryAllFunPar       = {};
                Args.QueryFun             = [];
                Args.QueryFunPar          = {};
                Args.SaveFun              = [];
                Args.SaveFunPar           = {};
                Args.Cat2_ColDec          = 2;
                Args.NparPool             = 24;
                Args.DeleteParPool        = false;
            end
            
            SearchRadius = convert.angular(Args.SearchRadiusUnits,'rad',Args.SearchRadius);  % [rad]
            
            % load HTM data for Cat1
            [IndexFileName1,IndexVarName1] = catsHTM.get_index_filename(CatName1);
            % HTM1 is the HTM index file
            [HTM1,DataHTM1] = catsHTM.load_htm_ind(IndexFileName1,IndexVarName1);
            % Level, Father, Son1, Son2, Son3, Son4, Poles 1 long,
            % poles 1 lat, ..., Nsrc

            % load HTM data for Cat2
            [IndexFileName2,IndexVarName2] = catsHTM.get_index_filename(CatName2);
            % HTM2 is the HTM index file
            [HTM2,DataHTM2] = catsHTM.load_htm_ind(IndexFileName2,IndexVarName2);
            % Level, Father, Son1, Son2, Son3, Son4, Poles 1 long,
            % poles 1 lat, ..., Nsrc

            %
            Nhtm1 = numel(HTM1);
            Nhtm2 = numel(HTM2);
            
            L1 = celestial.htm.nhtm2level(Nhtm1);
            L2 = celestial.htm.nhtm2level(Nhtm2);
            LMax = max(L1,L2);
            
            [HTM,Level] = celestial.htm.htm_build(LMax);
            Level1 = Level(L1);
            Level2 = Level(L2);
            Nh1    = numel(Level1.ptr);
            Nh2    = numel(Level2.ptr);
            
            [ColCell1] = catsHTM.load_colcell(CatName1);
            [ColCell2] = catsHTM.load_colcell(CatName2);
            Ncol2      = numel(ColCell2);
            
            % number of parallel processes
            if (Args.DeleteParPool)
                delete(gcp('nocreate'));
            end
            
            % comment out if needed
            %parpool(Args.NparPool);
            
            % replace parfor with for if needed
            Nh1
            
            Istart=1;
            for Ih1=Istart:1:Nh1
                Ih1
                % for each HTM in Cat1
                Cat1    = [];
                Cat2    = [];
                Ihtm1   = Level1.ptr(Ih1);
                
                % if HTM in Cat1 contain sources
                if (DataHTM1(Ihtm1,13)>0)
                    % load Cat1
      
                    Cat1 = catsHTM.load_cat(CatName1,Ihtm1);

                    %[Cat2,EdgeOK] = catsHTM.load_cat_edge(CatName2,Ihtm1);
                    %if (~EdgeOk)
                    
                    % search for all HTMs in Cat2 that may opverlap with
                    % Cat1 current HTM
                    MeanRA  = mean(HTM(Ihtm1).coo(:,1));
                    MeanDec = mean(HTM(Ihtm1).coo(:,2));
                    MinDec  = min(HTM(Ihtm1).coo(:,2))-SearchRadius;
                    MaxDec  = max(HTM(Ihtm1).coo(:,2))+SearchRadius;

                    %%
                    %if ((MeanDec.*180./pi)>-30)
                    
                    D = celestial.coo.sphere_dist_fast(MeanRA,MeanDec,HTM(Ihtm1).coo(:,1),HTM(Ihtm1).coo(:,2));
                    CircRadius = max(D) + SearchRadius; % [rad]

                    ID2 = celestial.htm.htm_search_cone(HTM2,MeanRA,MeanDec,CircRadius);
      
                    % load all ID2 from HTM2
                    Nid2 = numel(ID2);
                    for Iid2=1:1:Nid2
                        if (Iid2==1)
                            [Cat2,Ind2]   = catsHTM.load_cat(CatName2,ID2(Iid2),[MinDec MaxDec],Ncol2);
                            N2     = size(Cat2,1);
                            Cat2ID = [ID2(Iid2).*ones(N2,1), Ind2-1+(1:1:N2)'];
                        else
                            [Cat2tmp, Ind2] = catsHTM.load_cat(CatName2,ID2(Iid2),[MinDec MaxDec],Ncol2);
                            Cat2   = [Cat2; Cat2tmp];
                            N2     = size(Cat2,1);
                            Cat2ID = [Cat2ID; [ID2(Iid2).*ones(N2,1), Ind2-1+(1:1:N2)']];
                        end
                    end
   
                    if (isempty(Cat2))
                        % if Cat2 is empty - skip
                    else
                        
                        % sort Cat2 and Cat2ID
                        [Cat2,SI] = sortrows(Cat2,Args.Cat2_ColDec);
                        %Cat2ID = Cat2ID(SI,:);

                        % cross match Cat1 and Cat2
                        % return list of size of Cat1
                        [Match,Ind,IndCatMinDist] = VO.search.match_cats(Cat2,Cat1,'Radius',SearchRadius,'RadiusUnits','rad');

                        % self match
                        % match Cat1 with itself
                        if (Args.SelfMatch)
                            [MatchS,IndS,IndCatMinDistS] = VO.search.match_cats(Cat2,Cat2,'Radius',SearchRadius,'RadiusUnits','rad');
                            % adding column to Cat2 with number of
                            % additional sources in the search radius
                            Cat2 = [Cat2, MatchS.Nfound-1];
                        end
                            
                            

                        if (~isempty(Args.QueryAllFun))
                            % execute Args.QueryAllFun
                            %  QueryAllFun(Cat1,Ind,Cat2,varargin)
                            if (Ih1==Istart)
                                Data = [];
                            end
                            
                            
                            Data = Args.QueryAllFun(Cat1,Ind,Cat2,IndCatMinDist,Args.QueryAllFunPar{:},'Data',Data,'Ih1',Ih1,'Nh1',Nh1,'SearchRadius',Args.SearchRadius);
                        end
                        
                        %Cat2(IndCatMinDist,:)
                        IsN = isnan(IndCatMinDist);
                        IndCatMinDist(IsN) = 1;

                        %DataInd = Cat2ID(IndCatMinDist,:);
                        DataInd = Cat2ID(SI(IndCatMinDist),:);
                        %DataInd(1:2,:)
                        DataInd(IsN,:) = NaN;

                        Cat2matched        = Cat2(IndCatMinDist,:);
                        Cat2matched(IsN,:) = NaN;
                        if (~isempty(Args.QueryFun))
                            % execute Args.QueryFun
                            % QueryFun can select specific sources (by some
                            % attributes) from the matched Cat1 and Cat2
%Args.QueryFunPar{1} = Ihtm1;

                            FlagSelected       = Args.QueryFun(Cat1,Cat2matched,Ihtm1,Args.QueryFunPar{:});
                            % what to do with FlagSelected?
                            Cat1        = Cat1(FlagSelected,:);
                            Cat2matched = Cat2matched(FlagSelected,:);

                        end

                        if (~isempty(Args.SaveFun))
                            % execute Args.SaveFun
                            % Fun(Cat1,Cat2matched)
                            Args.SaveFun(Cat1,Cat2matched,Args.SaveFunPar{:});
                        end
                    end
                    %%
                    %end
                end
            end
            
            
            save(sprintf('Data_%d.mat',Ih1),'Data');
     
            
        end
        
        
        function mergeCats(CatNames, Args)
            % Merge multiple catsHTM catalogs into a single catsHTM cat.
            % Example: catsHTM.mergeCats
            
            arguments
                %CatNames cell    = {'GAIAEDR3','unWISE','TMASS','GLADE','PGC','SDSSDR10','PS1','DECaLS','GALEX','FIRST','NVSS','LAMOST_DR4','NEDz','SpecSDSSDR17','ROSATfsc','XMM','ztfDR1var','WDEDR3','QSO1M'};  % 16 bit
                %Args.CatRadius   = [2,         3,       3,      10,     10,   2,         2,    2,       4,       5,      15,    2,           10,    10,        30,        10,   2,         2,       3];
                CatNames cell    = {'GAIADR3','unWISE','TMASS','GLADEp','PGC','SDSSDR10','PS1','DECaLS','GALEX','FIRST','NVSS','VLASSep1','LAMOST_DR4','NEDz','SpecSDSSDR17','ROSATfsc','XMM','ztfDR1var','WDEDR3','QSO1M','GAIADR3extraGal'};  % 32 bit
                Args.CatRadius   = [2,         3,       3,      10,     10,   2,         2,    2,       4,       5,      15,    5,         2,           10,    10,            30,        10,   2,         2,       3,      10];
                Args.Nbit        = 21;
                Args.NewCatName  = 'MergedCat';
                Args.SaveInd     = true;
                
            end
            
        
            Ncats = numel(CatNames);
            
            % load HTM data for Cat1
            [IndexFileName,IndexVarName] = catsHTM.get_index_filename(CatNames{1});
            % HTM1 is the HTM index file
            [HTM,DataHTM] = catsHTM.load_htm_ind(IndexFileName,IndexVarName);
            % Level, Father, Son1, Son2, Son3, Son4, Poles 1 long,
            % poles 1 lat, ..., Nsrc

            %
            Nhtm = numel(HTM);
            
            L = celestial.htm.nhtm2level(Nhtm);
            
            [HTM,Level] = celestial.htm.htm_build(L);
            Level = Level(L);
            Nh    = numel(Level.ptr);
            
            [ColCell] = catsHTM.load_colcell(CatNames{1});
            Ncol      = numel(ColCell);
            
            for Ih=1:1:Nh
                [Ih, Nh]
                Ihtm   = Level.ptr(Ih);
                [FileName,DataName]=HDF5.get_file_var_from_htmid(Args.NewCatName, Ihtm);
                Exist = false;
                if java.io.File(FileName).exists
                    InfoH5 = h5info(FileName);
                    if any(strcmp({InfoH5.Datasets.Name},DataName))
                        Exist = true;
                    end
                end
                
                if ~Exist
                    % if HTM in Cat1 contain sources
                    if (DataHTM(Ihtm,13)>0)
                        % load Cat
                        Cat = catsHTM.load_cat(CatNames{1},Ihtm);
                        Nlines = size(Cat,1);
                        Bit = bitset(0,1).*ones(Nlines,1);
                        Cat = [Cat(:,1:2), Bit, Args.CatRadius(1).*ones(Nlines,1)];
                    else
                        Cat = zeros(0,3);
                    end
                    
                    %class(Cat)

                    % calculate center of HTM
                    % Corners = [DataHTM(Ihtm, [7, 9, 11]).', DataHTM(Ihtm, [8
                    % 10 12]).']; % BUG - likely in the construction of the
                    % DataHTM...

                    MeanCD = mean(HTM(Ihtm).cosd, 1);
                    [MeanRA, MeanDec] = celestial.coo.cosined2coo(MeanCD(1), MeanCD(2), MeanCD(3));
                    Corners = HTM(Ihtm).coo;

                    %[CD1,CD2,CD3] = celestial.coo.coo2cosined(Corners(:,1), Corners(:,2));
                    %[MeanRA, MeanDec] = celestial.coo.cosined2coo(mean(CD1), mean(CD2), mean(CD3));
                    %Radius = celestial.coo.sphere_dist_fast(DataHTM(Ihtm, 7), DataHTM(Ihtm, 8), DataHTM(Ihtm, 9), DataHTM(Ihtm, 10));

                    % search for corresponding HTMs in all other catalogs
                    for Icat=2:1:Ncats

                        try
                            CatC = catsHTM.cone_search(CatNames{Icat}, MeanRA, MeanDec, Level.side, 'RadiusUnits','rad');
                        catch
                            CatC = [];
                            'a'
                        end
                        %Icat
                        %class(CatC)
                        % select sources in HTM
                        if isempty(CatC)
                            CatC = zeros(0,2);
                        else
                            Flag = celestial.htm.in_polysphere(CatC(:,1:2), Corners);
                            CatC = CatC(Flag,:);
                        end
                        Nlines = size(CatC,1);
                        Bit = bitset(0,Icat).*ones(Nlines,1);
                        Cat  = [Cat; [CatC(:,1:2), Bit, Args.CatRadius(Icat).*ones(Nlines,1)]];
                    end


                    if size(Cat,1)>0
                        % sort Cat
                        Cat = sortrows(Cat, 2);

                        % save HTM 
                        [FileName,DataName]=HDF5.get_file_var_from_htmid(Args.NewCatName, Ihtm);

                        catsHTM.save_cat(FileName,DataName,Cat,2,30);
                    end

                end  % ~Exist
            end % for
            
            
            if Args.SaveInd
                IndFileName = sprintf('%s_htm.hdf5',Args.NewCatName);
                delete(IndFileName);
                Nsrc=HDF5.get_nsrc(Args.NewCatName);
                HDF5.save_htm_ind(HTM,IndFileName,[],{},Nsrc)

                ColCell = {'RA','Dec','CatBit','CatRadius'};
                ColUnits = {'rad','rad','','arcsec'};
                HDF5.save_cat_colcell(Args.NewCatName,ColCell,ColUnits);
            end
        end
    
       
    end
   
    
    
    methods (Static)  % prepare some sub/merged catalogs
    
        function prep_mergedSuperCat
            %
           
            arguments
                
            end
            
            % load GAIA Ind
            
            % for each triangle
            % go over all other catalogs and load all sources in triangle
            
            
        end
        
        
        
        
        
    end
    
    % plots
    methods (Static)
        function [H,Table]=plot_density(CatName,Args)
            % Plot a catsHTM catalog surface density
            % Package: @catsHTM
            % Description: Plot a catsHTM catalog surface density in
            %              sources per deg^2 or sources per HTM on a
            %              celestial sphere map.
            % Input  : - Catalog name (e.g., 'NVSS');
            %          * Arbitrary number of pairs of arguments: ...,keyword,value,...
            %            where keyword are one of the followings:
            %            'PerDeg2'  - plot density per deg^2.
            %                         Default is true.
            %                         Otherwise plot per HTM.
            %            'Step'     - Interpolation step in deg.
            %                         Default is 0.3 deg.
            %            'PlotType' - Options are: 'trisurf'|'scatterm'
            %                         Default is 'scatterm'
            %            'MarkerSize'- Marker size for scatterm.
            %                         Default is 5.
            %            'Projection'- Map projection.
            %                         Default is 'aitoff'.
            %            'LogN'     - Plot log10 number of sources.
            %                         Default is false.
            % Output : - Plot handle.
            % Example: H=catsHTM.plot_density('SDSSDR10')
            
            arguments
                CatName
                Args.PerDeg2              = true;  % otherwise per HTM
                Args.Step                 = 0.3;  % [deg]
                Args.PlotType             = 'scatterm';
                Args.MarkerSize           = 5;
                Args.Projection           = 'aitoff';
                Args.LogN                 = false;
            end
            
            RAD = 180./pi;
            
            Col.Level    = 1;
            Col.PolesLon = [7 9 11];
            Col.PolesLat = [8 10 12];
            Col.Nsrc     = 13;
            [IndexFileName,IndexVarName]=catsHTM.get_index_filename(CatName);
            [HTM,Data]=catsHTM.load_htm_ind(IndexFileName,IndexVarName);
            F = Data(:,Col.Level) == max(Data(:,Col.Level));
            Data1 = Data(F,:);
            
            Level = celestial.htm.nhtm2level(size(Data,1));
            [HTM,LevelList] = celestial.htm.htm_build(Level);
            Nhtm = numel(LevelList(end).ptr);
            for Ihtm=1:1:Nhtm
                IndHTM  = LevelList(end).ptr(Ihtm);
                MeanRA  = mean(HTM(IndHTM).coo(:,1));
                MeanDec = mean(HTM(IndHTM).coo(:,2));
                Table(Ihtm,:) = [MeanRA, MeanDec, Data1(Ihtm,Col.Nsrc)];
            end
            if (Args.PerDeg2)
                % Area of HTM triangle [deg^2]
                Area = 4.*pi.*RAD.^2./Nhtm;
                Table(:,3) = Table(:,3)./Area;  % convert to sources per deg^2
            end
            Table(:,1:2) = Table(:,1:2).*RAD;
            
            if (Args.LogN)
                Table(:,3) = log10(Table(:,3));
            end
            
            switch Args.PlotType
                case 'trisurf'
                    Tri = delaunay(Table(:,1),Table(:,2));
                    H=trisurf(Tri,Table(:,1), Table(:,2), Table(:,3));
                    view(0,90);
                    shading interp
                    colorbar
                case 'scatterm'
                    axesm(Args.Projection);
                    framem
                    H=scatterm(Table(:,2),Table(:,1),Args.MarkerSize,Table(:,3),'filled');
                    colorbar
                otherwise
                    error('Unknown PlotType option');
            end
            
%             F = scatteredInterpolant(Table(:,2).*RAD,Table(:,1).*RAD,Table(:,3));
%             Lon = (-180:Args.Step:180);
%             Lat = (-90:Args.Step:90);
%             [MLon,MLat] = meshgrid(Lon,Lat);
%
%             F.Method = 'nearest';
%             Ninterp = F(MLat,MLon);
%             surface(Lat,Lon,Ninterp');
%             shading interp
%             colorbar
%
   

        end
    end
    
    % retrieve catalog data
    methods (Static)
        
        function [ExtCat, ExtCatNames] = getExtCatData(T, Args)
            % for each catalog source retrieve relevant data from external catalogs according to the source's MergedCat mask
            % Input  : - a source table (usually, the output of a DB query)
            %          * ...,key,val,... 
            %        'MaskColumn' - the name of the mask column
            %        'SearchRad'  - the search radius
            %        'RadUnits'   - the search radius units
            % Output : - a M (num. of lines in T) x N (number of non-zero MergedCat masks) cell array of catalog strings
            %          - a M (num. of lines in T) x N (number of non-zero MergedCat masks) cell array of catalog names
            % Author : A.M. Krassilchtchikov (2025 Aug) 
            % Example: T = DB.query('select top 5 * from visit_src');
            %          [ExtCat, ExtCatNames] = catsHTM.getExtCatData(T,'SearchRad',3);           
            arguments
                T
                Args.MaskColumn = 'mergedcat';
                Args.SearchRad  = 5;
                Args.RadUnits   = 'arcsec';
            end
            %
            RAD = 180/pi;
            BD  = BitDictionary('BitMask.MergedCat.Default');
            %
            Mask = T.(Args.MaskColumn);            
            ExtCat = {}; 
            ExtCatNames = {};
            for Ln = 1:height(T)
                if Mask(Ln) > 0
                    RA = T.ra(Ln)/RAD; Dec = T.dec(Ln)/RAD;
                    CatNames = BD.bitdec2name(Mask(Ln));
                    for Icat = 1:numel(CatNames{1})
                        [Cat,~]= catsHTM.cone_search(CatNames{1}{Icat},RA,Dec,Args.SearchRad,...
                            'RadiusUnits',Args.RadUnits,'OutType','table');
                        ExtCat{Ln,Icat} = Cat;
                        ExtCatNames{Ln,Icat} = CatNames{1}{Icat};
                    end
                end
            end
        end

    end

    % HTM index (fast, analytic)
    methods (Static)

        function saveHTMIndexFast(Level, FileName, VarName, Attrib, Nsrc)
            % Save HTM index HDF5 file computed analytically (fast, no struct build)
            % Package: @catsHTM
            % Description: Fast alternative to HDF5.save_htm_ind that computes the
            %              13-column HTM index Data matrix directly from the level
            %              number using vectorized level-by-level subdivision.
            %              Avoids building the HTM struct array (htm_build)
            %              and the O(N^2) Nsrc lookup of save_htm_ind.
            %              Output is identical to save_htm_ind.
            % Input  :   - Level: HTM level (integer). Tree has levels 0..Level-1.
            %                     Level L has 8*4^L cells. Total nodes = 8*(4^Level - 1)/3.
            %            - FileName: HDF5 file name for output.
            %            - VarName: Variable name for the HTM dataset.
            %                       Default is derived from FileName (e.g., 'PS1DR2_HTM').
            %            - Attrib: Cell array of attributes {Key,Val} for 'ColNames'
            %                      dataset. Default is {}.
            %            - Nsrc: Matrix [IndHTM, Nsrc] with source counts per leaf cell.
            %                    Default is [].
            % Output :   null
            % Author : Dana Kovaleva (Feb 2026)
            % Example: catsHTM.saveHTMIndexFast(7, 'PS1DR2_htm.hdf5', 'PS1DR2_HTM', {}, Nsrc)

            %------------------------------------------------------------------
            % Handle default arguments
            %------------------------------------------------------------------
            if nargin < 3 || isempty(VarName)
                Tmp = regexp(FileName, '[/\\]', 'split');
                BaseName = Tmp{end};
                Parts = regexp(BaseName, '_', 'split');
                VarName = sprintf('%s_HTM', Parts{1});
            end
            if nargin < 4, Attrib = {}; end
            if nargin < 5, Nsrc = []; end

            % Total number of HTM nodes across all levels 0..Level-1
            % Level L (0-indexed depth) has 8*4^L nodes
            Nhtm = round(8 * (4^Level - 1) / 3);

            fprintf('  Building HTM index analytically (%d nodes, %d levels)...\n', Nhtm, Level);
            Tic = tic;

            % Pre-allocate output: [level, father, son1..4, poles1..6, nsrc]
            Data = nan(Nhtm, 13);

            % Build indexed Nsrc lookup for O(1) access per node
            % (avoids the O(N) linear scan per node in save_htm_ind)
            NsrcLookup = nan(Nhtm, 1);
            if ~isempty(Nsrc)
                ValidMask = Nsrc(:,1) >= 1 & Nsrc(:,1) <= Nhtm;
                NsrcLookup(Nsrc(ValidMask, 1)) = Nsrc(ValidMask, 2);
            end

            %------------------------------------------------------------------
            % Level 0: 8 root triangles (hardcoded geometry from htm_build)
            %------------------------------------------------------------------

            % North hemisphere (4 triangles): equatorial base + north pole vertex
            % South hemisphere (4 triangles): equatorial base + south pole vertex
            % Vertex coordinates in [Long, Lat] radians
            RootLong = zeros(8, 3);
            RootLat  = zeros(8, 3);
            for I = 1:4
                % North: [0,0], [pi/2,0], [0,pi/2] rotated by pi/2*(I-1)
                RootLong(I,:) = [0, pi/2, 0] + pi/2 * (I - 1);
                RootLat(I,:)  = [0, 0, pi/2];
            end
            for I = 1:4
                % South: [0,0], [0,-pi/2], [pi/2,0] rotated by pi/2*(I-1)
                RootLong(I+4,:) = [0, 0, pi/2] + pi/2 * (I - 1);
                RootLat(I+4,:)  = [0, -pi/2, 0];
            end

            % Convert root vertices to cosine directions (8 triangles x 3 components)
            [CD1_1, CD2_1, CD3_1] = celestial.coo.coo2cosined(RootLong(:,1), RootLat(:,1));
            [CD1_2, CD2_2, CD3_2] = celestial.coo.coo2cosined(RootLong(:,2), RootLat(:,2));
            [CD1_3, CD2_3, CD3_3] = celestial.coo.coo2cosined(RootLong(:,3), RootLat(:,3));

            CurrV1 = [CD1_1, CD2_1, CD3_1];  % 8x3 cosine directions of vertex 1
            CurrV2 = [CD1_2, CD2_2, CD3_2];  % 8x3 cosine directions of vertex 2
            CurrV3 = [CD1_3, CD2_3, CD3_3];  % 8x3 cosine directions of vertex 3

            % Compute poles for root triangles (vectorized)
            [PL1, PL2, PL3, PL4, PL5, PL6] = computeHTMPolesVec(CurrV1, CurrV2, CurrV3);

            % Fill Data for level 0
            Data(1:8, 1) = 0;       % level depth
            Data(1:8, 2) = NaN;     % father (roots have no parent)
            if Level > 1
                % Children start at index 9 (8 roots + 1)
                SonBase = 8 + (0:7)' * 4 + 1;
                Data(1:8, 3:6) = [SonBase, SonBase+1, SonBase+2, SonBase+3];
            end
            Data(1:8, 7)  = PL1;   Data(1:8, 8)  = PL2;
            Data(1:8, 9)  = PL3;   Data(1:8, 10) = PL4;
            Data(1:8, 11) = PL5;   Data(1:8, 12) = PL6;
            Data(1:8, 13) = NsrcLookup(1:8);

            %------------------------------------------------------------------
            % Levels 1..Level-1: vectorized subdivision
            %------------------------------------------------------------------
            PrevStartIdx = 1;

            for L = 1:Level-1
                Nprev  = size(CurrV1, 1);
                Nchild = Nprev * 4;
                StartIdx = round(1 + 8 * (4^L - 1) / 3);
                IsLeaf = (L == Level - 1);

                % Midpoints on great circles (normalized average of endpoints)
                Mid12 = CurrV1 + CurrV2;
                Mid12 = Mid12 ./ sqrt(sum(Mid12.^2, 2));
                Mid23 = CurrV2 + CurrV3;
                Mid23 = Mid23 ./ sqrt(sum(Mid23.^2, 2));
                Mid31 = CurrV3 + CurrV1;
                Mid31 = Mid31 ./ sqrt(sum(Mid31.^2, 2));

                % Assemble 4 children per parent (same vertex order as htm_build_son)
                % Child 1: V1, Mid12, Mid31
                % Child 2: V2, Mid23, Mid12
                % Child 3: V3, Mid31, Mid23
                % Child 4: Mid12, Mid23, Mid31
                NewV1 = zeros(Nchild, 3);
                NewV2 = zeros(Nchild, 3);
                NewV3 = zeros(Nchild, 3);

                NewV1(1:4:end,:) = CurrV1;   NewV2(1:4:end,:) = Mid12;  NewV3(1:4:end,:) = Mid31;
                NewV1(2:4:end,:) = CurrV2;   NewV2(2:4:end,:) = Mid23;  NewV3(2:4:end,:) = Mid12;
                NewV1(3:4:end,:) = CurrV3;   NewV2(3:4:end,:) = Mid31;  NewV3(3:4:end,:) = Mid23;
                NewV1(4:4:end,:) = Mid12;    NewV2(4:4:end,:) = Mid23;  NewV3(4:4:end,:) = Mid31;

                % Free memory from previous level before computing poles
                clear CurrV1 CurrV2 CurrV3 Mid12 Mid23 Mid31;

                % Compute poles for all children (vectorized)
                [PL1, PL2, PL3, PL4, PL5, PL6] = computeHTMPolesVec(NewV1, NewV2, NewV3);

                % Fill Data rows for this level
                Indices = StartIdx:(StartIdx + Nchild - 1);
                Data(Indices, 1) = L;  % level depth

                % Father: each group of 4 consecutive children shares one parent
                ParentIdx = repelem((PrevStartIdx:(PrevStartIdx + Nprev - 1))', 4);
                Data(Indices, 2) = ParentIdx;

                % Son indices (NaN for leaf nodes, computed for internal nodes)
                if ~IsLeaf
                    NextStart = round(1 + 8 * (4^(L+1) - 1) / 3);
                    ChildPos = (0:Nchild-1)';
                    SonBase = NextStart + ChildPos * 4;
                    Data(Indices, 3:6) = [SonBase, SonBase+1, SonBase+2, SonBase+3];
                end

                % Poles
                Data(Indices, 7)  = PL1;   Data(Indices, 8)  = PL2;
                Data(Indices, 9)  = PL3;   Data(Indices, 10) = PL4;
                Data(Indices, 11) = PL5;   Data(Indices, 12) = PL6;

                % Source counts
                Data(Indices, 13) = NsrcLookup(Indices);

                % Prepare cosine directions for next level
                CurrV1 = NewV1;
                CurrV2 = NewV2;
                CurrV3 = NewV3;
                PrevStartIdx = StartIdx;

                fprintf('    Level %d: %d cells done (%.1f s)\n', L, Nchild, toc(Tic));
            end

            %------------------------------------------------------------------
            % Save to HDF5 (same format as HDF5.save_htm_ind)
            %------------------------------------------------------------------
            AttribHTM = {'Table.Col.1','Level'; ...
                      'Table.Col.2','Father'; ...
                      'Table.Col.3','Son1'; ...
                      'Table.Col.4','Son2'; ...
                      'Table.Col.5','Son3'; ...
                      'Table.Col.6','Son4'; ...
                      'Table.Col.7', 'Poles1Lon';...
                      'Table.Col.8', 'Poles1Lat';...
                      'Table.Col.9', 'Poles2Lon';...
                      'Table.Col.10','Poles2Lat';...
                      'Table.Col.11','Poles3Lon';...
                      'Table.Col.12','Poles3Lat';...
                      'Table.Col.13','Nsrc'};
            HDF5.save(single(Data), FileName, VarName, AttribHTM);
            % Save column names dataset (for compatibility with load_htm_ind)
            HDF5.save([], FileName, 'ColNames', Attrib);

            fprintf('  HTM index saved: %s (%.1f s)\n', FileName, toc(Tic));

        end

    end

    methods (Static)

        function [XidTable, Cats_cone, Summary] = crossIDCatsHTM(RA, Dec, Radius, Args)
            % Build a cross-identification index table between one anchor catsHTM
            % catalog and all (or selected) other catsHTM catalogs in a field.
            %   For a given field (RA, Dec, search radius) the function cone-searches
            %   an anchor catalog (default GAIADR3) and every other catsHTM catalog,
            %   cross-matches each catalog against a running "master" source list and
            %   records, per master source: the row index of its match in each
            %   catalog and the number of matches within the matching radius.
            %   Sources present in a catalog but absent from the anchor are appended
            %   to the master list with a fresh global index (see 'OrphanHandling'),
            %   so the master list is the union of all sources in the field.
            %   The field is specified like catsHTM.cone_search: positional RA, Dec
            %   [radians] and Radius [arcsec by default].
            % Input  : - Field centre J2000 R.A. [radians] (or in 'CooUnits').
            %            Default is 254 deg (in radians).
            %          - Field centre J2000 Dec. [radians] (or in 'CooUnits').
            %            Default is +64 deg (in radians).
            %          - Field (cone) search radius [arcsec] (or in 'RadiusUnits'). A
            %            deliberately small, safe default of 60 (=1 arcmin); enlarge
            %            for a wider field, e.g. crossIDCatsHTM(RA,Dec,10,...
            %            'RadiusUnits','deg').
            %          * ...,key,val,...
            %            'CooUnits' - Units of the RA/Dec inputs. Default is 'rad'.
            %            'RadiusUnits' - Units of the Radius input. Default is 'arcsec'.
            %            'RefCat' - Anchor catalog name (seeds the master list and its
            %                   global indices). Default is 'GAIADR3'.
            %            'CatList' - Cellstr of catalog names to cross-match against
            %                   the anchor. If empty, all available (Status==true)
            %                   catsHTM catalogs are used. Default is {}.
            %            'SkipCats' - Cellstr of catalog names to exclude. Default {}.
            %            'MatchRadius' - Default matching radius applied to every
            %                   catalog pair. Default is 2.
            %            'MatchRadiusUnits' - Matching radius units. Default 'arcsec'.
            %            'RadiusPerCat' - Per-catalog matching-radius overrides, as an
            %                   Nx2 cell {CatName, Radius; ...} in 'MatchRadiusUnits'.
            %                   Catalogs not listed use 'MatchRadius'. Default is {}.
            %            'OrphanHandling' - How to treat catalog sources with no anchor
            %                   match:
            %                   'growing' - append them to the master list; later
            %                        catalogs can match a previous catalog's orphan
            %                        (union of all sources; order-dependent). Default.
            %                   'append'  - each orphan becomes its own global source;
            %                        catalogs are matched only against the anchor seed
            %                        (no cross-catalog orphan merging).
            %                   'none'    - orphans are dropped (anchor sources only).
            %            'Con' - Cell array of per-catalog cone_search constraints
            %                   (see catsHTM.cone_search). Applied to every catalog.
            %                   Default is {}.
            %            'AddDistCol' - Add a Dist_<Cat> [arcsec] column per catalog.
            %                   Default is true.
            %            'KeepExtraMatches' - When a catalog has SEVERAL sources within
            %                   the match radius of a master source, keep the full list.
            %                   The result stays one row per master source: Ind_<Cat>,
            %                   Nmatch_<Cat>, Dist_<Cat> are for the NEAREST match and
            %                   Nmatch_<Cat> is the true count; the additional (non
            %                   nearest) catalog indices, ordered by distance, are kept
            %                   per row as a VECTOR. These vectors are always returned
            %                   in Summary.ExtraMatches.<Cat> (an Nglobal-by-1 cell;
            %                   each entry is a native-index row vector, or NaN when
            %                   there are 0 or 1 matches). With OutType='table' they are
            %                   also added as an IndExtra_<Cat> cell column. Uses the
            %                   heavier multi-match; NOT supported with TableToDisk, and
            %                   the IndExtra_ cell columns are not written to CSV.
            %                   Default is false.
            %            'AddPointer' - Add a STABLE, query-independent pointer to each
            %                   source: two numeric columns CellID_<Cat> (HTM leaf-cell
            %                   id) and RowInCell_<Cat> (row within htm_<CellID>), the
            %                   intrinsic storage address in the catalog (see
            %                   catsHTM.sourcePointer). Unlike Ind_<Cat> (a cone_search
            %                   row index, valid only for this query) this pair is
            %                   reproducible across sessions and independent of the
            %                   field, and being numeric it is present in BOTH the table
            %                   and the astrocatalog output. Costs a second pass of
            %                   cone-cell reads per catalog, and is SKIPPED (with a note)
            %                   when TableToDisk=true. Default is true.
            %            'AddCatRowID' - Also add CatRowID_<Cat>: the (CellID,RowInCell)
            %                   pair collapsed to ONE contiguous catalog-wide scalar id
            %                   (see catsHTM.catRowID), a compact single-number key
            %                   that inverts back to a pointer via
            %                   catsHTM.catRowID2Pointer. Requires AddPointer (it is
            %                   derived from the pair) and costs one metadata scan
            %                   (catsHTM.getNsrcMeta) per catalog, so it is opt-in.
            %                   Default is false.
            %            'IdExtras' - Also stamp the ADDITIONAL (non-nearest) matches
            %                   with the same id layers the main match gets: when
            %                   AddPointer, adds CellIDExtra_<Cat>/RowInCellExtra_<Cat>;
            %                   when AddCatRowID, also CatRowIDExtra_<Cat>. Each is a
            %                   ragged cell-of-vectors aligned with IndExtra_<Cat> (one
            %                   value per extra match, NaN when there are 0 or 1
            %                   matches), so - like IndExtra_ - it lives only in a table
            %                   and in Summary (ExtraCellID/ExtraRowInCell/ExtraCatRowID),
            %                   never in a numeric AstroCatalog or CSV. Requires
            %                   KeepExtraMatches and AddPointer, and costs extra
            %                   sourcePointer reads for the additional matches.
            %                   Default is false.
            %            'OutType' - 'table' (MATLAB table; includes the OriginCat
            %                   column and, when KeepExtraMatches=true, the IndExtra_
            %                   cell columns) or 'astrocatalog' (numeric-column
            %                   AstroCatalog - the text OriginCat and any cell columns
            %                   are NOT included; OriginCat is kept in
            %                   Summary.OriginCat, row-aligned, and extra matches in
            %                   Summary.ExtraMatches). Default is 'table'. Access
            %                   AstroCatalog columns with getCol, e.g.
            %                   Ind = getCol(XidCat,'Ind_PS1'). Ignored when
            %                   TableToDisk=true.
            %            'OutFile' - Output file base/full name. If non-empty, results
            %                   are written to disk (see 'OutFileFormat'). The '.mat'
            %                   stores variable XidTable (the SAME type as the returned
            %                   output - a table by default, or an AstroCatalog) plus
            %                   Cats_cone and Summary (Summary.OriginCat carries provenance);
            %                   the '.csv' is the flat index table and always includes
            %                   the OriginCat column. Default is '' (no file written).
            %            'OutFileFormat' - Cellstr subset of {'mat','csv','json'}
            %                   controlling which files are written when 'OutFile'
            %                   is set. 'mat' bundles XidTable+Cats_cone+Summary;
            %                   'csv' is the flat index table; 'json' writes a lean
            %                   <OutFile>_signature.json sidecar holding just
            %                   Summary.Signature (the per-catalog version stamps),
            %                   so a csv/DB export of the pointer table stays
            %                   validatable via catsHTM.checkCatalogSignature
            %                   without the full .mat. 'json' is skipped (with a
            %                   note) when StampSignature is off. Default {'mat','csv'}.
            %            'CatsToDisk' - If true, each catalog's cone_search result is
            %                   written to its own .mat file as soon as it has been
            %                   used, then cleared from memory, so peak memory is set
            %                   by the single largest catalog rather than the sum of
            %                   all of them. Use for large fields where the in-memory
            %                   Cats_cone struct would be huge. The returned Cats_cone then holds
            %                   file PATHS (char) instead of AstroCatalog objects; load
            %                   one with L=load(Cats_cone.<Cat>); L.Cat is the AstroCatalog.
            %                   Ind_<Cat> still indexes that (native-order) catalog.
            %                   Default is false.
            %            'CatsDir' - Target directory for the per-catalog .mat files
            %                   when CatsToDisk=true. Default is the 'OutFile'
            %                   directory if given, otherwise the current directory.
            %            'TableToDisk' - If true, the cross-id table is written to a
            %                   v7.3 .mat column-by-column (via matfile) instead of
            %                   being built in memory, so the wide Nglobal-by-(3*Ncat)
            %                   table never lives in RAM at once (peak memory is set by
            %                   a single column, not the whole table). In this mode the
            %                   FIRST output (XidTable) is the FILE PATH (char), not a
            %                   table; load columns lazily, e.g. matfile(path).Ind_PS1
            %                   or load(path,'Ind_PS1'). The .mat also stores MasterID,
            %                   RA, Dec, OriginCat, Summary and Cats_cone. This mode skips
            %                   the CSV output and ignores OutType='astrocatalog'.
            %                   Combine with CatsToDisk=true for end-to-end low memory.
            %                   Default is false.
            %            'TableFile' - Target .mat path when TableToDisk=true. Default
            %                   is <OutFile>_xidtable.mat if OutFile is given, else
            %                   ./xidTable.mat.
            %            'Verbose' - Print progress. Default is true.
            % Output : - XidTable: the cross-id table. Columns: MasterID, RA, Dec
            %            [deg], OriginCat, then Ind_<Cat> (row index into Cats_cone.<Cat>,
            %            NaN if no match), Nmatch_<Cat> (number of catalog sources
            %            within the matching radius), optionally Dist_<Cat> [arcsec],
            %            and (when AddPointer=true) CellID_<Cat> / RowInCell_<Cat> - the
            %            stable per-source storage pointer - and (when AddCatRowID=true)
            %            CatRowID_<Cat>, that pointer collapsed to a single catalog-wide
            %            scalar id. By default (OutType='table')
            %            this is a MATLAB table
            %            including OriginCat (and the IndExtra_<Cat> cell columns when
            %            KeepExtraMatches=true, plus CellIDExtra_/RowInCellExtra_/
            %            CatRowIDExtra_<Cat> when IdExtras=true); with OutType='astrocatalog' it is an
            %            AstroCatalog with the numeric columns only (OriginCat is in
            %            Summary.OriginCat, extra matches in Summary.ExtraMatches). If
            %            'TableToDisk'=true this output is instead the .mat file PATH
            %            (char) the table was streamed to (see 'TableToDisk').
            %            Ind_<Cat> is the source identifier: since native SourceID
            %            columns are unreliable in many catsHTM catalogs, a source is
            %            identified unambiguously by (Cats_cone.<Cat>, Ind_<Cat>). The
            %            index is in NATIVE cone_search order, so it also reproduces a
            %            fresh catsHTM.cone_search(<Cat>, RA, Dec, Radius, 'Con',Con)
            %            over the field recorded in Summary.Field.
            %          - Cats_cone: a struct with one AstroCatalog per catalog (keyed by
            %            catalog name), holding the cone-search results the Ind_<Cat>
            %            columns index into, in native cone_search order. This is the
            %            lookup table for the indices and is saved with the '.mat'
            %            output so (index -> source) is self-contained and portable.
            %            If 'CatsToDisk'=true, Cats_cone instead holds the file PATH (char)
            %            of each catalog's own .mat file (see 'CatsToDisk').
            %          - Summary: struct with .Field, .RefCat, .Nref, .Nglobal,
            %            .OriginCat, .Failed and a .PerCat table (one row per catalog,
            %            anchor first) with columns:
            %            Ncone - number of sources the catalog cone_search returned.
            %            Nmatched - how many of those Ncone sources matched a master
            %              source (<= Ncone).
            %            Norphan - how many of those Ncone sources had no master match
            %              (appended as new global sources unless OrphanHandling='none').
            %              Nmatched + Norphan = Ncone.
            %            MatchRadiusArcsec - match radius used for that catalog.
            %            If KeepExtraMatches=true, Summary also has .ExtraMatches - a
            %            struct with an Nglobal-by-1 cell per catalog holding the
            %            additional (non-nearest) native match indices per master row.
            %            If IdExtras=true, Summary also has .ExtraCellID / .ExtraRowInCell
            %            (and .ExtraCatRowID when AddCatRowID) - the same cell-of-vectors
            %            layout holding the storage pointer / scalar id of each extra
            %            match.
            % Note   : The FIRST output is a projection of the full result onto what the
            %          chosen container can represent, so its content depends on the
            %          format - but no information is lost, because Summary is the
            %          canonical record (Summary.OriginCat always holds provenance and,
            %          with KeepExtraMatches, Summary.ExtraMatches always holds the extra
            %          vectors). What each format carries:
            %            OutType='table' (default) - MasterID, RA, Dec, OriginCat, then
            %              Ind_/Nmatch_/Dist_ (and CellID_/RowInCell_ when AddPointer,
            %              CatRowID_ when AddCatRowID) per catalog, plus IndExtra_<Cat>
            %              cell columns when KeepExtraMatches=true (and, when IdExtras,
            %              CellIDExtra_/RowInCellExtra_/CatRowIDExtra_ cell columns).
            %              The fullest form.
            %            OutType='astrocatalog' - the numeric columns only (this DOES
            %              include CellID_/RowInCell_/CatRowID_, which are numeric). The
            %              text OriginCat and the ragged IndExtra_/*Extra_ CANNOT live in
            %              a numeric AstroCatalog, so they are dropped from the object and
            %              read instead from Summary (OriginCat / ExtraMatches /
            %              ExtraCellID / ExtraRowInCell / ExtraCatRowID).
            %            OutFile .mat - stores XidTable as the SAME type as the returned
            %              output (so it carries whatever that type carries), plus Cats_cone
            %              and Summary.
            %            OutFile .csv - MasterID, RA, Dec, OriginCat, Ind_/Nmatch_/Dist_
            %              (and CellID_/RowInCell_/CatRowID_ when enabled); the ragged
            %              IndExtra_/*Extra_ cell columns are NOT written (not
            %              serialisable to CSV).
            %            TableToDisk .mat - MasterID, RA, Dec, OriginCat and the
            %              Ind_/Nmatch_/Dist_ columns as separate variables, plus Summary
            %              and Cats_cone; KeepExtraMatches and AddPointer are not supported in
            %              this mode.
            %          The differences are forced by container limits: a numeric
            %          AstroCatalog cannot hold text or ragged columns, and CSV cannot
            %          hold ragged columns.
            % See also: catsHTM.gatherCrossIDData (STEP 2 - turns this index into a
            %           per-source DATA table, from Cats_cone or straight from catsHTM via
            %           the CellID_/RowInCell_ pointer), catsHTM.sourcePointer /
            %           catsHTM.gatherByPointer (the pointer machinery AddPointer uses).
            % Author : Dana Kovaleva (Jul 2026)
            % Example:
            %  RAD = 180./pi;   % positional RA,Dec are in RADIANS (like cone_search)
            %  % The default output T is a MATLAB table (T.Ind_PS1 etc.). Pass
            %  % 'OutType','astrocatalog' for an AstroCatalog (read with getCol).
            %
            %  % (1) defaults: field RA=254, Dec=+64 deg, R=60"=1 arcmin, 2" matching,
            %  %     anchor GAIADR3 vs ALL available catsHTM catalogs:
            %  [T, Cats_cone, S] = catsHTM.crossIDCatsHTM;
            %
            %  % (2) EXPLICIT catalog list (only these, in this order; skips the
            %  %     'all catalogs' enumeration), field (254,64) deg over a 1 deg cone:
            %  [T, Cats_cone, S] = catsHTM.crossIDCatsHTM(254/RAD, 64/RAD, 1, ...
            %               'RadiusUnits','deg', 'CatList',{'PS1DR2','APASS','GALEXAIS','TMASS'});
            %
            %  % (3) different field + anchor + per-catalog match radii, 600" cone,
            %  %     written to disk (.mat keeps Cats_cone, .csv is the flat table):
            %  [T, Cats_cone, S] = catsHTM.crossIDCatsHTM(180/RAD, -30/RAD, 600, ...
            %               'RefCat','GAIADR3','CatList',{'PS1','FIRST','NVSS'},...
            %               'MatchRadius',2,'RadiusPerCat',{'FIRST',5;'NVSS',5},...
            %               'OutFile','~/tmp/xid_field');
            %
            %  % (4) you may also give the field in degrees via 'CooUnits', and take
            %  %     all catalogs EXCEPT a few, dropping the orphan appending:
            %  [T, Cats_cone, S] = catsHTM.crossIDCatsHTM(254, 64, 60, 'CooUnits','deg',...
            %               'SkipCats',{'DECaLS10','unWISE'}, 'OrphanHandling','none');
            %
            %  % (5) resolve an index (default table output): the PS1 source matched
            %  %     to global source g:
            %  g = 7;  row = T.Ind_PS1(g);
            %  if ~isnan(row), src = Cats_cone.PS1.Catalog(row,:); end
            %
            %  % (6) LARGE field: stream each catalog to disk to keep memory low.
            %  %     Cats_cone then holds file paths; load one to resolve indices:
            %  [T, Cats_cone, S] = catsHTM.crossIDCatsHTM(254/RAD, 64/RAD, 10, ...
            %               'RadiusUnits','deg', 'CatsToDisk',true,'CatsDir','~/tmp/xidcats');
            %  Lp = load(Cats_cone.PS1);  ps1 = Lp.Cat;   % AstroCatalog for PS1
            %  src = ps1.Catalog(T.Ind_PS1(g),:);
            %
            %  % (7) keep the list of additional matches when a catalog has several
            %  %     sources within the radius of one master source (default table):
            %  [T, Cats_cone, S] = catsHTM.crossIDCatsHTM(254/RAD, 64/RAD, 360, ...
            %               'KeepExtraMatches',true);
            %  T.Ind_PS1(g)         % nearest PS1 source; T.Nmatch_PS1(g) = actual count
            %  T.IndExtra_PS1{g}    % vector of the OTHER PS1 indices (NaN if <=1 match)
            %  S.ExtraMatches.PS1{g}% same list (also the only home under 'astrocatalog')
            %
            %  % (8) STEP 2 - materialize the actual per-source DATA from the index T
            %  %     with catsHTM.gatherCrossIDData (the companion gatherer):
            %  [T, Cats_cone] = catsHTM.crossIDCatsHTM(254/RAD, 64/RAD, 360);
            %  D = catsHTM.gatherCrossIDData(T, Cats_cone);                 % from the snapshot
            %  D = catsHTM.gatherCrossIDData(T, [], 'Source','pointer');% from catsHTM, no Cats_cone

            arguments
                RA                             = 254.*pi./180;   % [rad] default field centre
                Dec                            = 64.*pi./180;    % [rad]
                Radius                         = 360;             % [arcsec] (0.1 deg)
                Args.CooUnits                  = 'rad';
                Args.RadiusUnits               = 'arcsec';
                Args.RefCat                    = 'GAIADR3';
                Args.CatList                   = {};
                Args.SkipCats                  = {};
                Args.MatchRadius               = 2;
                Args.MatchRadiusUnits          = 'arcsec';
                Args.RadiusPerCat              = {};
                Args.OrphanHandling            = 'growing';
                Args.Con                       = {};
                Args.AddDistCol logical        = true;
                Args.KeepExtraMatches logical  = true;
                Args.AddPointer logical        = true;
                Args.AddCatRowID logical       = false;
                Args.IdExtras logical          = false;
                Args.StampSignature logical    = true;
                Args.OutType                   = 'table';
                Args.OutFile                   = '';
                Args.OutFileFormat             = {'mat','csv'};
                Args.CatsToDisk logical        = false;
                Args.CatsDir                   = '';
                Args.TableToDisk logical       = false;
                Args.TableFile                 = '';
                Args.Verbose logical           = true;
            end

            OrphanHandling = validatestring(Args.OrphanHandling, {'growing','append','none'});
            UseGrown  = strcmp(OrphanHandling, 'growing');
            DoAppend  = ~strcmp(OrphanHandling, 'none');
            Stream    = Args.TableToDisk;
            KeepExtra = Args.KeepExtraMatches;
            if KeepExtra && Stream
                error('crossIDCatsHTM:extraStream', ...
                    'KeepExtraMatches is not supported together with TableToDisk.');
            end
            % Stable per-source pointers add a second pass of cone-cell reads, so they
            % are computed only for the in-memory path (skipped when streaming).
            AddPointer = Args.AddPointer && ~Stream;
            if Args.AddPointer && Stream && Args.Verbose
                fprintf('crossIDCatsHTM: AddPointer skipped in TableToDisk mode.\n');
            end
            % The scalar catalog-wide id is derived from the (CellID,RowInCell) pair,
            % so it needs AddPointer; it also costs one metadata scan per catalog.
            AddCatRowID = Args.AddCatRowID && AddPointer;
            if Args.AddCatRowID && ~AddPointer && Args.Verbose
                fprintf('crossIDCatsHTM: AddCatRowID requires AddPointer; skipped.\n');
            end
            % IdExtras mirrors the main-match id layers (CellID_/RowInCell_, and
            % CatRowID_ when AddCatRowID) onto the additional (non-nearest) matches, as
            % ragged cell-of-vector columns; needs KeepExtraMatches + AddPointer.
            IdExtras = Args.IdExtras && KeepExtra && AddPointer;
            if Args.IdExtras && ~(KeepExtra && AddPointer) && Args.Verbose
                fprintf('crossIDCatsHTM: IdExtras needs KeepExtraMatches and AddPointer; skipped.\n');
            end

            % field centre in radians (cone_search convention)
            RA_rad  = convert.angular(Args.CooUnits, 'rad', RA);
            Dec_rad = convert.angular(Args.CooUnits, 'rad', Dec);
            RA_deg  = convert.angular(Args.CooUnits, 'deg', RA);
            Dec_deg = convert.angular(Args.CooUnits, 'deg', Dec);

            % resolve the list of catalogs to cross-match
            CatList = localBuildCatList(Args.RefCat, Args.CatList, Args.SkipCats, Args.Verbose);

            % where to stream per-catalog catalogs, if requested
            CatsDir = '';
            Prefix  = '';
            if Args.CatsToDisk
                [CatsDir, Prefix] = localCatsTarget(Args.CatsDir, Args.OutFile);
                if ~isfolder(CatsDir)
                    mkdir(CatsDir);
                end
            end

            % where to stream the table columns, if requested
            TableFile = '';
            ColTmpDir = '';
            if Stream
                TableFile = localTableTarget(Args.TableFile, Args.OutFile);
                ColTmpDir = tempname;
                mkdir(ColTmpDir);
            end

            % ---- seed the master list with the anchor catalog --------------------
            if Args.Verbose
                fprintf('crossIDCatsHTM: anchor %s cone_search (R=%g %s)...\n', ...
                    Args.RefCat, Radius, Args.RadiusUnits);
            end
            RefCatH = catsHTM.cone_search(Args.RefCat, RA_rad, Dec_rad, Radius, ...
                'RadiusUnits', Args.RadiusUnits, 'Con', Args.Con, 'OutType', 'astrocatalog');
            [seedRA, seedDec] = getLonLat(RefCatH, 'deg');
            seedRA  = seedRA(:);
            seedDec = seedDec(:);
            Nref    = numel(seedRA);
            if Nref == 0
                error('crossIDCatsHTM:emptyAnchor', ...
                    'Anchor catalog %s returned no sources in the field.', Args.RefCat);
            end

            % stable storage pointers for the anchor sources (before RefCatH is freed)
            if AddPointer
                [aRA, aDec] = getLonLat(RefCatH, 'rad');
                [AnchorCID, AnchorRIC] = catsHTM.sourcePointer(Args.RefCat, aRA, aDec, ...
                    'MaxDist', Args.MatchRadius, 'MaxDistUnits', Args.MatchRadiusUnits);
            end

            % seedRA/seedDec already extracted, so the anchor catalog can be streamed
            % to disk (and freed) right away in CatsToDisk mode.
            Cats_cone = struct();
            if Args.CatsToDisk
                Cats_cone.(Args.RefCat) = localWriteCat(CatsDir, Prefix, Args.RefCat, RefCatH, Args.Verbose);
                RefCatH = []; %#ok<NASGU>  free the anchor catalog from memory
            else
                Cats_cone.(Args.RefCat) = RefCatH;
            end

            % master (output) list — grows as orphans are appended
            L         = Nref;
            mRA       = seedRA;
            mDec      = seedDec;
            OriginCat = repmat({Args.RefCat}, L, 1);

            % per-catalog columns: kept in memory (structs) or streamed to temp files
            % (ColFiles{k} <-> ColNames{k}) when TableToDisk=true.
            Ind       = struct();
            Nmatch    = struct();
            Dist      = struct();
            Extra     = struct();     % only used when KeepExtra (never with Stream)
            CellID    = struct();     % only used when AddPointer (never with Stream)
            RowInCell = struct();
            CellIDExtra    = struct(); % only used when IdExtras (cell-of-vectors)
            RowInCellExtra = struct();
            ColFiles  = {};
            AnchorInd  = (1:L).';
            AnchorNm   = ones(L,1);
            AnchorDist = zeros(L,1);
            if Stream
                ColFiles{1} = localWriteCol(ColTmpDir, 1, AnchorInd, AnchorNm, AnchorDist);
            else
                Ind.(Args.RefCat)    = AnchorInd;
                Nmatch.(Args.RefCat) = AnchorNm;
                Dist.(Args.RefCat)   = AnchorDist;
                if KeepExtra
                    Extra.(Args.RefCat) = repmat({NaN}, L, 1);   % anchor has no extras
                end
                if AddPointer
                    CellID.(Args.RefCat)    = AnchorCID;
                    RowInCell.(Args.RefCat) = AnchorRIC;
                end
                if IdExtras
                    CellIDExtra.(Args.RefCat)    = repmat({NaN}, L, 1);  % anchor: no extras
                    RowInCellExtra.(Args.RefCat) = repmat({NaN}, L, 1);
                end
            end

            % per-catalog summary accumulators (anchor first)
            ColNames  = {Args.RefCat};
            StatNcone = Nref;
            StatNmat  = Nref;
            StatNorph = 0;
            StatRad   = NaN;      % anchor is not "matched" with a radius
            Failed    = {};

            % ---- cross-match every other catalog ---------------------------------
            for Icat = 1:1:numel(CatList)
                Name    = CatList{Icat};
                RadMat  = localResolveRadius(Name, Args.MatchRadius, Args.RadiusPerCat);
                Success = true;

                if Args.Verbose
                    fprintf('  [%d/%d] %s (match R=%g %s)...', Icat, numel(CatList), ...
                        Name, RadMat, Args.MatchRadiusUnits);
                end

                try
                    CatH = catsHTM.cone_search(Name, RA_rad, Dec_rad, Radius, ...
                        'RadiusUnits', Args.RadiusUnits, 'Con', Args.Con, 'OutType', 'astrocatalog');
                catch ME
                    Success = false;
                    Failed{end+1} = Name; %#ok<AGROW>
                    if Args.Verbose
                        fprintf(' FAILED (%s)\n', ME.message);
                    end
                end

                if Success
                    Ncone = CatH.sizeCatalog;

                    % full-length columns for the current output master (length L)
                    FullInd  = nan(L,1);
                    FullNm   = zeros(L,1);
                    FullDist = nan(L,1);
                    if KeepExtra
                        FullExtra = repmat({NaN}, L, 1);
                    end
                    Norphan  = 0;

                    if Ncone > 0
                        % match against the master (grown or seed-only)
                        if UseGrown
                            MatchRA  = mRA;
                            MatchDec = mDec;
                        else
                            MatchRA  = seedRA;
                            MatchDec = seedDec;
                        end
                        MasterCat          = AstroCatalog;
                        MasterCat.Catalog  = [MatchRA, MatchDec];
                        MasterCat.ColNames = {'RA','Dec'};
                        MasterCat.ColUnits = {'deg','deg'};

                        % matchReturnIndices sorts its first argument by Dec; match on
                        % a sorted COPY so CatH keeps native order, and capture the
                        % permutation SI (SortedRow -> native row) to remap indices.
                        DecColInd       = colnameDict2ind(CatH, CatH.DefNamesDec);
                        SortedCat       = CatH.copy;
                        [SortedCat, SI] = sortrows(SortedCat, DecColInd);

                        ML = numel(MatchRA);
                        if KeepExtra
                            % all matches per master row (nearest + additional list)
                            [IndVec, NmVec, DistVec, ExtraML, OrphSorted] = ...
                                localMultiMatch(SortedCat, MasterCat, RadMat, ...
                                    Args.MatchRadiusUnits, SI, Ncone, ML);
                            FullExtra(1:ML) = ExtraML;
                        else
                            % Obj1 = this catalog (sorted copy), Obj2 = master, so the
                            % per-master-row fields are exactly what we need.
                            M = imProc.match.matchReturnIndices(SortedCat, MasterCat, ...
                                    'CooType','sphere', 'Radius',RadMat, 'RadiusUnits',Args.MatchRadiusUnits);
                            IndSorted  = M.Obj2_IndInObj1(:);
                            IndVec     = nan(ML,1);
                            Gd         = ~isnan(IndSorted);
                            IndVec(Gd) = SI(IndSorted(Gd));    % remap to native CatH rows
                            NmVec      = M.Obj2_NmatchObj1(:); % counts are order-independent
                            DistVec    = convert.angular('rad', 'arcsec', M.Obj2_Dist(:));
                            OrphSorted = find(isnan(M.Obj1_IndInObj2(:)));
                        end

                        FullInd(1:ML)  = IndVec;
                        FullNm(1:ML)   = NmVec;
                        FullDist(1:ML) = DistVec;

                        % orphans: catalog rows not matched to any master source
                        OrphNative = SI(OrphSorted);   % native rows
                        Norphan    = numel(OrphNative);

                        if DoAppend && Norphan > 0
                            [oRA, oDec] = getLonLat(CatH, 'deg');   % native order
                            NewIdx = (L+1):(L+Norphan);
                            mRA(NewIdx,1)       = oRA(OrphNative);
                            mDec(NewIdx,1)      = oDec(OrphNative);
                            OriginCat(NewIdx,1) = {Name};
                            % grow this catalog's own columns onto the new rows
                            FullInd(NewIdx,1)  = OrphNative;
                            FullNm(NewIdx,1)   = 1;
                            FullDist(NewIdx,1) = 0;
                            if KeepExtra
                                FullExtra(NewIdx,1) = repmat({NaN}, Norphan, 1);
                            end
                            L = L + Norphan;
                        end
                    end

                    % stable storage pointers (cell id, row-in-cell) for the sources
                    % this catalog contributes; computed while CatH is still available.
                    FullCellID     = nan(L,1);
                    FullRowInCell  = nan(L,1);
                    CellIDExtraCol    = {};
                    RowInCellExtraCol = {};
                    if AddPointer && Ncone > 0
                        % native rows referenced by the nearest matches...
                        RefMain = FullInd(~isnan(FullInd));
                        % ...and, when IdExtras, by the additional (non-nearest) matches
                        RefExtra = [];
                        if IdExtras
                            HasEx    = ~cellfun(@(v) all(isnan(v)), FullExtra);
                            RefExtra = [FullExtra{HasEx}];   % concat of native-index row vecs
                            RefExtra = RefExtra(~isnan(RefExtra));
                        end
                        Ref = unique([RefMain(:); RefExtra(:)]);
                        [srcRA, srcDec] = getLonLat(CatH, 'rad');
                        [CidRef, RicRef] = catsHTM.sourcePointer(Name, srcRA(Ref), srcDec(Ref), ...
                            'MaxDist', RadMat, 'MaxDistUnits', Args.MatchRadiusUnits);
                        CidMap      = nan(Ncone,1);
                        RicMap      = nan(Ncone,1);
                        CidMap(Ref) = CidRef;
                        RicMap(Ref) = RicRef;
                        Gp                = ~isnan(FullInd);
                        FullCellID(Gp)    = CidMap(FullInd(Gp));
                        FullRowInCell(Gp) = RicMap(FullInd(Gp));
                        % map the extra-match native indices through the same pointer map
                        if IdExtras
                            CellIDExtraCol    = cellfun(@(v) localMapVec(v, CidMap), FullExtra, ...
                                'UniformOutput', false);
                            RowInCellExtraCol = cellfun(@(v) localMapVec(v, RicMap), FullExtra, ...
                                'UniformOutput', false);
                        end
                    end

                    % CatH is no longer needed: keep it in the native-order Cats_cone struct
                    % (in memory), or stream it to disk and free it. Either way
                    % Ind_<Cat> indexes this native-order catalog.
                    if Args.CatsToDisk
                        Cats_cone.(Name) = localWriteCat(CatsDir, Prefix, Name, CatH, Args.Verbose);
                        CatH = [];
                    else
                        Cats_cone.(Name) = CatH;
                    end

                    if Stream
                        ColFiles{end+1} = localWriteCol(ColTmpDir, numel(ColNames)+1, ...
                            FullInd, FullNm, FullDist); %#ok<AGROW>
                    else
                        Ind.(Name)    = FullInd;
                        Nmatch.(Name) = FullNm;
                        Dist.(Name)   = FullDist;
                        if KeepExtra
                            Extra.(Name) = FullExtra;
                        end
                        if AddPointer
                            CellID.(Name)    = FullCellID;
                            RowInCell.(Name) = FullRowInCell;
                        end
                        if IdExtras
                            CellIDExtra.(Name)    = CellIDExtraCol;
                            RowInCellExtra.(Name) = RowInCellExtraCol;
                        end
                    end

                    ColNames{end+1}  = Name;         %#ok<AGROW>
                    StatNcone(end+1) = Ncone;        %#ok<AGROW>
                    % Nmatched = cone sources that matched a master source, so that
                    % Nmatched + Norphan = Ncone (both are catalog-source counts).
                    StatNmat(end+1)  = Ncone - Norphan; %#ok<AGROW>
                    StatNorph(end+1) = Norphan;      %#ok<AGROW>
                    StatRad(end+1)   = convert.angular(Args.MatchRadiusUnits,'arcsec',RadMat); %#ok<AGROW>

                    if Args.Verbose
                        fprintf(' %d src, %d matched, %d orphan\n', Ncone, StatNmat(end), Norphan);
                    end
                end
            end

            Nglobal = L;

            % ---- summary ---------------------------------------------------------
            Summary = struct();
            Summary.Field     = struct('RA',RA_deg,'Dec',Dec_deg,'Radius',Radius,'RadiusUnits',Args.RadiusUnits);
            Summary.RefCat    = Args.RefCat;
            Summary.Nref      = Nref;
            Summary.Nglobal   = Nglobal;
            Summary.OriginCat = OriginCat;
            Summary.Failed    = Failed;
            Summary.PerCat    = table(ColNames(:), StatNcone(:), StatNmat(:), StatNorph(:), StatRad(:), ...
                'VariableNames', {'Catalog','Ncone','Nmatched','Norphan','MatchRadiusArcsec'});

            % version-stamp each catalog so persisted pointers can be validated
            % later (catsHTM.checkCatalogSignature). Cheap - reads only the index
            % + colcell. Non-fatal: a signature failure never breaks the cross-id.
            if Args.StampSignature
                Summary.Signature = struct();
                for Icol = 1:1:numel(ColNames)
                    try
                        Summary.Signature.(ColNames{Icol}) = ...
                            catsHTM.catalogSignature(ColNames{Icol});
                    catch ME
                        if Args.Verbose
                            fprintf('crossIDCatsHTM: signature for %s failed (%s)\n', ...
                                ColNames{Icol}, ME.message);
                        end
                    end
                end
            end

            % additional (non-nearest) matches per master row, as an Nglobal-by-1 cell
            % per catalog (NaN entries where there are 0 or 1 matches).
            if KeepExtra
                Summary.ExtraMatches = struct();
                for Icol = 1:1:numel(ColNames)
                    Summary.ExtraMatches.(ColNames{Icol}) = localPadCell(Extra.(ColNames{Icol}), Nglobal);
                end
            end

            % per-catalog getNsrcMeta offset tables, computed once and reused by the
            % main and extra CatRowID mappings (only when AddCatRowID).
            NsrcCache = containers.Map('KeyType','char','ValueType','any');

            % storage pointers / scalar ids for the extra matches (IdExtras): ragged
            % cell-of-vector columns, mirroring the main-match id layers.
            if IdExtras
                Summary.ExtraCellID    = struct();
                Summary.ExtraRowInCell = struct();
                if AddCatRowID
                    Summary.ExtraCatRowID = struct();
                end
                for Icol = 1:1:numel(ColNames)
                    Nm   = ColNames{Icol};
                    ECid = localPadCell(CellIDExtra.(Nm),    Nglobal);
                    ERic = localPadCell(RowInCellExtra.(Nm), Nglobal);
                    Summary.ExtraCellID.(Nm)    = ECid;
                    Summary.ExtraRowInCell.(Nm) = ERic;
                    if AddCatRowID
                        NsrcTab        = catsHTM.getNsrcMeta(Nm);
                        NsrcCache(Nm)  = NsrcTab;
                        Summary.ExtraCatRowID.(Nm) = cellfun(@(c,r) ...
                            localExtraCatRowID(Nm, c, r, NsrcTab), ECid, ERic, 'UniformOutput', false);
                    end
                end
            end

            if Stream
                % ---- stream the table to a v7.3 .mat, one column at a time -------
                localAssembleTableFile(TableFile, ColNames, ColFiles, Nglobal, ...
                    mRA, mDec, OriginCat, Args.AddDistCol, Summary, Cats_cone, Args.Verbose);
                if isfolder(ColTmpDir)
                    rmdir(ColTmpDir, 's');
                end
                XidTable = TableFile;
            else
                % ---- assemble the output table (in memory) ----------------------
                % pad every catalog column to the final global length
                VarData = {(1:Nglobal).', mRA, mDec, OriginCat};
                VarName = {'MasterID','RA','Dec','OriginCat'};
                for Icol = 1:1:numel(ColNames)
                    Name = ColNames{Icol};
                    VarData{end+1} = localPad(Ind.(Name),    Nglobal, NaN); %#ok<AGROW>
                    VarName{end+1} = ['Ind_' Name];                          %#ok<AGROW>
                    VarData{end+1} = localPad(Nmatch.(Name), Nglobal, 0);    %#ok<AGROW>
                    VarName{end+1} = ['Nmatch_' Name];                       %#ok<AGROW>
                    if Args.AddDistCol
                        VarData{end+1} = localPad(Dist.(Name), Nglobal, NaN); %#ok<AGROW>
                        VarName{end+1} = ['Dist_' Name];                      %#ok<AGROW>
                    end
                    if AddPointer
                        PadCID = localPad(CellID.(Name),    Nglobal, NaN);
                        PadRIC = localPad(RowInCell.(Name), Nglobal, NaN);
                        VarData{end+1} = PadCID;                                    %#ok<AGROW>
                        VarName{end+1} = ['CellID_' Name];                         %#ok<AGROW>
                        VarData{end+1} = PadRIC;                                    %#ok<AGROW>
                        VarName{end+1} = ['RowInCell_' Name];                      %#ok<AGROW>
                        if AddCatRowID
                            % collapse the pointer pair to one contiguous catalog-wide
                            % scalar (NaN where the pair is NaN); one getNsrcMeta scan,
                            % reused from NsrcCache if IdExtras already built it.
                            if isKey(NsrcCache, Name)
                                NsrcTab = NsrcCache(Name);
                            else
                                NsrcTab = catsHTM.getNsrcMeta(Name);
                                NsrcCache(Name) = NsrcTab;
                            end
                            VarData{end+1} = catsHTM.catRowID(Name, PadCID, PadRIC, ...
                                'Nsrc', NsrcTab); %#ok<AGROW>
                            VarName{end+1} = ['CatRowID_' Name];                        %#ok<AGROW>
                        end
                    end
                end
                TableForm = table(VarData{:}, 'VariableNames', VarName);

                % ---- select return type -----------------------------------------
                if strcmpi(Args.OutType, 'astrocatalog')
                    % numeric-only AstroCatalog (text OriginCat -> Summary.OriginCat)
                    Numeric = TableForm;
                    Numeric.OriginCat = [];
                    AC          = AstroCatalog;
                    AC.Catalog  = table2array(Numeric);
                    AC.ColNames = Numeric.Properties.VariableNames;
                    XidTable    = AC;
                else
                    XidTable = TableForm;
                    % add the additional-matches cell columns (table output only)
                    if KeepExtra
                        for Icol = 1:1:numel(ColNames)
                            Nm = ColNames{Icol};
                            XidTable.(['IndExtra_' Nm]) = Summary.ExtraMatches.(Nm);
                            if IdExtras
                                XidTable.(['CellIDExtra_' Nm])    = Summary.ExtraCellID.(Nm);
                                XidTable.(['RowInCellExtra_' Nm]) = Summary.ExtraRowInCell.(Nm);
                                if AddCatRowID
                                    XidTable.(['CatRowIDExtra_' Nm]) = Summary.ExtraCatRowID.(Nm);
                                end
                            end
                        end
                    end
                end

                % ---- optional file output ---------------------------------------
                % .mat stores the SAME object as the return value (AstroCatalog by
                % default); .csv is always built from the full table so it keeps the
                % OriginCat column. Summary (incl. Summary.OriginCat) is saved too.
                if ~isempty(Args.OutFile)
                    localWriteOut(Args.OutFile, Args.OutFileFormat, XidTable, TableForm, ...
                        Cats_cone, Summary, Args.Verbose);
                end
            end
        end

        function [Data, OutFile] = gatherCrossIDData(T, Cats_cone, Args)
            % Materialize a crossIDCatsHTM index table into a per-source data table.
            %   Given the cross-id table T produced by catsHTM.crossIDCatsHTM, gather
            %   for EACH global source (row of T) the actual column values of its
            %   matched source in each catalog. Two data sources, chosen by 'Source':
            %     'cats'    - read from the Cats_cone snapshot (in-memory AstroCatalogs, or
            %                 the local .mat paths of the CatsToDisk form) using the
            %                 Ind_<Cat> index. Fast, but limited to the columns the
            %                 cone_search returned.
            %     'pointer' - read straight from the on-disk catsHTM store using the
            %                 CellID_<Cat>/RowInCell_<Cat> pointer (via
            %                 catsHTM.gatherByPointer). Needs no Cats_cone and can fetch ANY
            %                 catalog column, at the cost of reading the HDF5 files.
            %   Catalog columns are prefixed by catalog name (e.g. PS1_RA) to avoid
            %   collisions. Rows with no match get FillValue. Optionally writes the
            %   result to a .mat and/or .csv file.
            % Input  : - T: the cross-id result (first output of crossIDCatsHTM) -
            %            either an AstroCatalog or a MATLAB table. For 'cats' it must
            %            contain the Ind_<Cat> columns; for 'pointer' the
            %            CellID_<Cat>/RowInCell_<Cat> columns (crossIDCatsHTM with
            %            AddPointer=true). For an AstroCatalog the text OriginCat column
            %            is absent (it is in Summary.OriginCat) and is simply omitted.
            %          - Cats_cone: the struct of per-catalog catalogs (second output of
            %            crossIDCatsHTM). Each field is either an AstroCatalog or a
            %            char path to a .mat holding it under variable 'Cat' (the
            %            CatsToDisk form) - both are handled. Required for Source='cats';
            %            ignored (may be omitted or []) for Source='pointer'.
            %          * ...,key,val,...
            %            'Source' - 'cats' | 'pointer' | 'auto'. 'auto' (default) picks
            %                   'cats' when Cats_cone is provided (non-empty), else 'pointer'.
            %            'CatList' - Cellstr of catalog names to gather. Default {} =
            %                   all catalogs found as Ind_<Cat> columns in T.
            %            'Columns' - Which catalog columns to pull. Default {} = ALL
            %                   columns of each catalog. A cellstr applies the same
            %                   column list to every catalog (names absent from a given
            %                   catalog are skipped). A struct with fields named after
            %                   catalogs gives a per-catalog cellstr.
            %            'ColPrefix' - Prefix each gathered column name with '<Cat>_'.
            %                   Default is true (recommended; avoids name collisions).
            %            'IncludeGlobal' - Prepend MasterID, RA, Dec, OriginCat from T.
            %                   Default is true.
            %            'FillValue' - Value for rows with no match. Default is NaN.
            %            'CatDir' - Directory holding the catsHTM files, passed through
            %                   to catsHTM.gatherByPointer for Source='pointer'.
            %                   Default '' = resolve via which() on the path.
            %            'Signature' - Per-catalog signature struct (Summary.Signature
            %                   from crossIDCatsHTM). For Source='pointer', each cat
            %                   with a matching field is validated against the catalog
            %                   before its rows are read. Default [] = no check.
            %            'ValidateSig' - Honour 'Signature' (error on a stale row
            %                   layout, warn on a suspect change). Default true; no
            %                   effect without 'Signature'.
            %            'OutFile' - Output file base/full name. If non-empty, the
            %                   gathered table is written (see 'OutFormat'). Default ''.
            %            'OutFormat' - Cellstr subset of {'mat','csv'} to write when
            %                   OutFile is set. Default is {'mat'}.
            %            'Verbose' - Print progress. Default is true.
            % Output : - Data: the gathered per-source table (Nglobal rows). Columns:
            %            [MasterID RA Dec OriginCat] then <Cat>_<Col> for every gathered
            %            catalog/column.
            %          - OutFile: cellstr of files actually written (empty if none).
            % See also: catsHTM.crossIDCatsHTM (produces the T / Cats_cone inputs),
            %           catsHTM.gatherByPointer (the Source='pointer' data reader),
            %           catsHTM.sourcePointer, catsHTM.catRowID.
            % Author : Dana Kovaleva (Jul 2026)
            % Example: % Step 1 - build the cross-id index (see catsHTM.crossIDCatsHTM):
            %          [T, Cats_cone] = catsHTM.crossIDCatsHTM(254/(180/pi), 64/(180/pi), 360);
            %          % Step 2a - from the snapshot (default when Cats_cone is given):
            %          D = catsHTM.gatherCrossIDData(T, Cats_cone, 'OutFile','~/tmp/xid_data');
            %          % Step 2b - straight from catsHTM, no Cats_cone needed, any columns:
            %          D = catsHTM.gatherCrossIDData(T, [], 'Source','pointer', ...
            %                 'Columns',struct('GAIADR3',{{'Mag_G'}}, 'PS1',{{'gPSFMag','rPSFMag'}}));

            arguments
                T
                Cats_cone                       = [];
                Args.Source                = 'auto';
                Args.CatList               = {};
                Args.Columns               = {};
                Args.ColPrefix logical     = true;
                Args.IncludeGlobal logical = true;
                Args.FillValue             = NaN;
                Args.CatDir                = '';
                Args.Signature             = [];
                Args.ValidateSig logical   = true;
                Args.OutFile               = '';
                Args.OutFormat             = {'mat'};
                Args.Verbose logical       = true;
            end

            % resolve the data source ('auto' -> cats if Cats_cone given, else pointer)
            Source = validatestring(Args.Source, {'auto','cats','pointer'});
            if strcmp(Source, 'auto')
                if isempty(Cats_cone)
                    Source = 'pointer';
                else
                    Source = 'cats';
                end
            end
            if strcmp(Source, 'cats') && isempty(Cats_cone)
                error('gatherCrossIDData:noCats', ...
                    'Source=''cats'' requires the Cats_cone struct (2nd argument).');
            end

            if ~istable(T) && ~isa(T, 'AstroCatalog')
                error('gatherCrossIDData:badTable', ...
                    'T must be a MATLAB table or an AstroCatalog (crossIDCatsHTM output).');
            end
            % column accessor for either a table or an AstroCatalog. For an
            % AstroCatalog the text OriginCat column is absent (it lives in
            % Summary.OriginCat), so it is simply not gathered here.
            if istable(T)
                Nglobal  = height(T);
                VarNames = T.Properties.VariableNames;
                GetCol   = @(Name) T.(Name);
            else
                Nglobal  = size(T.Catalog, 1);
                VarNames = T.ColNames;
                GetCol   = @(Name) getCol(T, Name);
            end

            % catalogs to gather: from CatList, else every Ind_<Cat> column in T
            if isempty(Args.CatList)
                IsInd   = startsWith(VarNames, 'Ind_');
                CatList = extractAfter(VarNames(IsInd), 'Ind_');
            else
                CatList = Args.CatList;
                if ischar(CatList) || isstring(CatList)
                    CatList = cellstr(CatList);
                end
            end

            % ---- global columns ---------------------------------------------------
            VarData = {};
            VarName = {};
            if Args.IncludeGlobal
                for Gc = {'MasterID','RA','Dec','OriginCat'}
                    if ismember(Gc{1}, VarNames)
                        VarData{end+1} = GetCol(Gc{1}); %#ok<AGROW>
                        VarName{end+1} = Gc{1};         %#ok<AGROW>
                    end
                end
            end

            % ---- gather each catalog ---------------------------------------------
            for Icat = 1:1:numel(CatList)
                Name = CatList{Icat};
                [Block, ColNm, Msg] = localGatherOne(Source, Name, VarNames, GetCol, ...
                    Cats_cone, Args, Nglobal);
                if ~isempty(Msg)
                    if Args.Verbose
                        fprintf('gatherCrossIDData: %s\n', Msg);
                    end
                else
                    for Ic = 1:1:numel(ColNm)
                        if Args.ColPrefix
                            ThisName = [Name '_' ColNm{Ic}];
                        else
                            ThisName = ColNm{Ic};
                        end
                        VarData{end+1} = Block(:, Ic);              %#ok<AGROW>
                        VarName{end+1} = ThisName;                  %#ok<AGROW>
                    end
                    if Args.Verbose
                        fprintf('gatherCrossIDData: %s - %d columns [%s]\n', Name, numel(ColNm), Source);
                    end
                end
            end

            % ensure valid, unique table variable names
            VarName  = matlab.lang.makeValidName(VarName);
            VarName  = matlab.lang.makeUniqueStrings(VarName, {}, namelengthmax);
            Data     = table(VarData{:}, 'VariableNames', VarName);

            % ---- optional file output --------------------------------------------
            OutFile = {};
            if ~isempty(Args.OutFile)
                OutFile = localWriteData(Args.OutFile, Args.OutFormat, Data, Args.Verbose);
            end
        end

    end

end % end class


%==========================================================================
% LOCAL HELPER FUNCTIONS
%==========================================================================

function H = localHashBytes(Bytes)
% Lowercase hex MD5 of a byte vector (used by catsHTM.catalogSignature).
% Java's MessageDigest is always available in MATLAB, so no external deps.
    Bytes = uint8(Bytes(:));
    MD = java.security.MessageDigest.getInstance('MD5');
    MD.update(typecast(Bytes, 'int8'));          % Java byte[] is signed
    Dig = typecast(MD.digest(), 'uint8');        % int8 digest -> 0..255
    H = lower(reshape(dec2hex(Dig, 2).', 1, [])); % 32-char hex string
end

function Bytes = localReadBytes(FileName)
% Read a whole file as a uint8 column (empty on failure).
    Bytes = uint8([]);
    Fid = fopen(FileName, 'r');
    if Fid >= 0
        Bytes = fread(Fid, Inf, '*uint8');
        fclose(Fid);
    end
end

function [PL1, PL2, PL3, PL4, PL5, PL6] = computeHTMPolesVec(CV1, CV2, CV3)
% Compute polysphere poles for N triangles (vectorized)
% Replicates celestial.htm.polysphere_poles for N triangles at once.
% Sorts vertices by position angle from centroid for consistent pole
% orientation, then computes cross products of consecutive edges.
% Input  : - CV1: Nx3 cosine direction matrix for vertex 1.
%          - CV2: Nx3 cosine direction matrix for vertex 2.
%          - CV3: Nx3 cosine direction matrix for vertex 3.
% Output : - PL1..PL6: Nx1 pole coordinates as
%            [Pole1Lon, Pole1Lat, Pole2Lon, Pole2Lat, Pole3Lon, Pole3Lat].

    N = size(CV1, 1);

    % Centroid of each triangle (mean of cosine directions)
    CenCD1 = (CV1(:,1) + CV2(:,1) + CV3(:,1)) / 3;
    CenCD2 = (CV1(:,2) + CV2(:,2) + CV3(:,2)) / 3;
    CenCD3 = (CV1(:,3) + CV2(:,3) + CV3(:,3)) / 3;
    [CenLong, CenLat] = celestial.coo.cosined2coo(CenCD1, CenCD2, CenCD3);

    % Vertex lon/lat for position angle computation
    [Long1, Lat1] = celestial.coo.cosined2coo(CV1(:,1), CV1(:,2), CV1(:,3));
    [Long2, Lat2] = celestial.coo.cosined2coo(CV2(:,1), CV2(:,2), CV2(:,3));
    [Long3, Lat3] = celestial.coo.cosined2coo(CV3(:,1), CV3(:,2), CV3(:,3));

    % Position angle from centroid to each vertex (for sorting)
    [~, PA1] = celestial.coo.sphere_dist_fast(CenLong, CenLat, Long1, Lat1);
    [~, PA2] = celestial.coo.sphere_dist_fast(CenLong, CenLat, Long2, Lat2);
    [~, PA3] = celestial.coo.sphere_dist_fast(CenLong, CenLat, Long3, Lat3);

    % Sort vertices by PA per triangle (consistent winding order)
    AllPA = [PA1, PA2, PA3];    % Nx3
    [~, SI] = sort(AllPA, 2);   % Nx3 sort indices per row

    % Gather cosine directions in sorted vertex order
    RowIdx = repmat((1:N)', 1, 3);
    LinIdx = sub2ind([N, 3], RowIdx, SI);

    AllCD1 = [CV1(:,1), CV2(:,1), CV3(:,1)];   % Nx3
    AllCD2 = [CV1(:,2), CV2(:,2), CV3(:,2)];
    AllCD3 = [CV1(:,3), CV2(:,3), CV3(:,3)];

    SortCD1 = AllCD1(LinIdx);   % Nx3 sorted
    SortCD2 = AllCD2(LinIdx);
    SortCD3 = AllCD3(LinIdx);

    % Sorted vertices as 3-component direction vectors
    SV1 = [SortCD1(:,1), SortCD2(:,1), SortCD3(:,1)];  % sorted vertex 1
    SV2 = [SortCD1(:,2), SortCD2(:,2), SortCD3(:,2)];  % sorted vertex 2
    SV3 = [SortCD1(:,3), SortCD2(:,3), SortCD3(:,3)];  % sorted vertex 3

    % Cross products of consecutive edges (inline cross_fast for speed):
    % Pole1 = cross(sv1, sv2), Pole2 = cross(sv2, sv3), Pole3 = cross(sv3, sv1)
    P1 = [SV1(:,2).*SV2(:,3) - SV1(:,3).*SV2(:,2), ...
          SV1(:,3).*SV2(:,1) - SV1(:,1).*SV2(:,3), ...
          SV1(:,1).*SV2(:,2) - SV1(:,2).*SV2(:,1)];
    P2 = [SV2(:,2).*SV3(:,3) - SV2(:,3).*SV3(:,2), ...
          SV2(:,3).*SV3(:,1) - SV2(:,1).*SV3(:,3), ...
          SV2(:,1).*SV3(:,2) - SV2(:,2).*SV3(:,1)];
    P3 = [SV3(:,2).*SV1(:,3) - SV3(:,3).*SV1(:,2), ...
          SV3(:,3).*SV1(:,1) - SV3(:,1).*SV1(:,3), ...
          SV3(:,1).*SV1(:,2) - SV3(:,2).*SV1(:,1)];

    % Convert pole direction vectors to lon/lat
    [PL1, PL2] = celestial.coo.cosined2coo(P1(:,1), P1(:,2), P1(:,3));
    [PL3, PL4] = celestial.coo.cosined2coo(P2(:,1), P2(:,2), P2(:,3));
    [PL5, PL6] = celestial.coo.cosined2coo(P3(:,1), P3(:,2), P3(:,3));

end
            


% ======================================================================
function CatList = localBuildCatList(RefCat, UserList, SkipCats, Verbose)
    % Resolve the list of catalogs to cross-match against the anchor.
    Data      = catsHTM.catalogs;
    AvailName = {Data([Data.Status]).Name};

    if isempty(UserList)
        CatList = AvailName;
    else
        % explicit list is trusted (may be on-path catalogs not in the
        % registry); warn about unregistered names but keep them.
        if ischar(UserList) || isstring(UserList)
            UserList = cellstr(UserList);
        end
        Known = ismember(UserList, AvailName);
        if Verbose && any(~Known)
            fprintf('crossIDCatsHTM: catalog(s) not in catsHTM.catalogs registry (kept, resolved by path): %s\n', ...
                strjoin(UserList(~Known), ', '));
        end
        CatList = UserList;
    end

    % remove the anchor and any explicitly skipped catalogs
    Drop    = [{RefCat}, cellstr(SkipCats).'];
    CatList = CatList(~ismember(CatList, Drop));
    CatList = CatList(:).';
end

% ======================================================================
function Rad = localResolveRadius(Name, DefRad, Pairs)
    % Per-catalog matching radius, falling back to the default.
    Rad = DefRad;
    if ~isempty(Pairs)
        Idx = find(strcmp(Pairs(:,1), Name), 1);
        if ~isempty(Idx)
            Rad = Pairs{Idx, 2};
        end
    end
end

% ======================================================================
function V = localPad(V, N, FillVal)
    % Pad a column vector to length N with FillVal.
    V = V(:);
    if numel(V) < N
        V(numel(V)+1:N, 1) = FillVal;
    end
end

% ======================================================================
function C = localPadCell(C, N)
    % Pad a cell column to length N with {NaN}.
    C = C(:);
    if numel(C) < N
        C(numel(C)+1:N, 1) = repmat({NaN}, N-numel(C), 1);
    end
end

% ======================================================================
function Out = localMapVec(V, Map)
    % Map a vector of native cone indices (or the scalar-NaN sentinel) through
    % a per-row lookup Map, preserving NaNs and shape.
    Out = nan(size(V));
    Ok  = ~isnan(V);
    Out(Ok) = Map(V(Ok));
end

% ======================================================================
function G = localExtraCatRowID(Name, Cid, Ric, NsrcTab)
    % Collapse an extra-match pointer vector (or NaN) to scalar CatRowIDs,
    % reusing the precomputed getNsrcMeta offset table; returns a row vector.
    G = catsHTM.catRowID(Name, Cid(:), Ric(:), 'Nsrc', NsrcTab).';
end

% ======================================================================
function [IndVec, NmVec, DistVec, ExtraML, OrphSorted] = ...
        localMultiMatch(SortedCat, MasterCat, RadMat, RadUnits, SI, Ncone, ML)
    % Per master row, return the nearest match plus the list of additional
    % (non-nearest) matches. Indices are remapped SortedCat -> native via SI.
    % IndVec/NmVec/DistVec are ML-by-1 (nearest, count, nearest dist [arcsec]);
    % ExtraML is an ML-by-1 cell (native index row vector, or NaN for <=1);
    % OrphSorted are SortedCat rows matched to no master source.
    RM = imProc.match.matchReturnIndicesMulti(SortedCat, MasterCat, ...
            'CooType','sphere', 'Radius',RadMat, 'RadiusUnits',RadUnits);
    MM = RM(1).Ind;                       % struct array, one per master row

    IndVec  = nan(ML,1);
    NmVec   = zeros(ML,1);
    DistVec = nan(ML,1);
    ExtraML = repmat({NaN}, ML, 1);
    Matched = false(Ncone,1);
    for Ig = 1:1:ML
        Ids = MM(Ig).Ind(:);
        if ~isempty(Ids)
            Matched(Ids)  = true;
            [Dsort, Ord]  = sort(MM(Ig).Dist(:));   % nearest first
            IdsN          = SI(Ids(Ord));           % native indices
            IndVec(Ig)    = IdsN(1);
            NmVec(Ig)     = numel(Ids);
            DistVec(Ig)   = convert.angular('rad', 'arcsec', Dsort(1));
            if numel(IdsN) > 1
                ExtraML{Ig} = IdsN(2:end).';        % additional native indices
            end
        end
    end
    OrphSorted = find(~Matched);
end

% ======================================================================
function [Dir, Prefix] = localCatsTarget(CatsDir, OutFile)
    % Resolve the directory and filename prefix for streamed catalog files.
    if isempty(CatsDir)
        if isempty(OutFile)
            Dir = pwd;
        else
            Dir = fileparts(OutFile);
            if isempty(Dir)
                Dir = pwd;
            end
        end
    else
        Dir = CatsDir;
    end
    if isempty(OutFile)
        Prefix = 'xidCats';
    else
        [~, Prefix] = fileparts(OutFile);
    end
end

% ======================================================================
function F = localTableTarget(TableFile, OutFile)
    % Resolve the .mat path for the streamed cross-id table.
    if ~isempty(TableFile)
        F = TableFile;
    elseif ~isempty(OutFile)
        [P, B] = fileparts(OutFile);
        if isempty(P)
            P = pwd;
        end
        F = fullfile(P, [B '_xidtable.mat']);
    else
        F = fullfile(pwd, 'xidTable.mat');
    end
    [~, ~, E] = fileparts(F);
    if ~strcmpi(E, '.mat')
        F = [F '.mat'];
    end
end

% ======================================================================
function P = localWriteCol(Dir, Idx, Ind, Nm, Dist)
    % Stream one catalog's finalized (pre-pad) columns to a temp .mat.
    P          = fullfile(Dir, sprintf('col_%05d.mat', Idx));
    Chunk.Ind  = Ind;
    Chunk.Nm   = Nm;
    Chunk.Dist = Dist;
    save(P, '-struct', 'Chunk', '-v7.3');
end

% ======================================================================
function localAssembleTableFile(TableFile, ColNames, ColFiles, Nglobal, ...
        mRA, mDec, OriginCat, AddDistCol, Summary, Cats_cone, Verbose)
    % Write the cross-id table to a v7.3 .mat one column at a time via
    % matfile, so the full wide table is never held in memory. Variables:
    % MasterID, RA, Dec, OriginCat, Ind_<Cat>, Nmatch_<Cat>[, Dist_<Cat>],
    % plus Summary and Cats_cone.
    if isfile(TableFile)
        delete(TableFile);
    end
    Mf           = matfile(TableFile, 'Writable', true);
    Mf.MasterID  = (1:Nglobal).';
    Mf.RA        = mRA;
    Mf.Dec       = mDec;
    Mf.OriginCat = OriginCat;
    for Icol = 1:1:numel(ColNames)
        Chunk = load(ColFiles{Icol});
        Mf.(['Ind_' ColNames{Icol}])    = localPad(Chunk.Ind, Nglobal, NaN);
        Mf.(['Nmatch_' ColNames{Icol}]) = localPad(Chunk.Nm,  Nglobal, 0);
        if AddDistCol
            Mf.(['Dist_' ColNames{Icol}]) = localPad(Chunk.Dist, Nglobal, NaN);
        end
    end
    Mf.Summary = Summary;
    Mf.Cats_cone    = Cats_cone;
    if Verbose
        fprintf('crossIDCatsHTM: wrote streamed table %s (%d rows, %d catalogs)\n', ...
            TableFile, Nglobal, numel(ColNames));
    end
end

% ======================================================================
function Path = localWriteCat(Dir, Prefix, Name, Cat, Verbose)
    % Write one cone_search catalog to its own .mat file; return the path.
    % Loaded back as: L = load(Path); L.Cat  (the AstroCatalog).
    Path       = fullfile(Dir, sprintf('%s_%s.mat', Prefix, Name));
    Payload.Cat = Cat;
    save(Path, '-struct', 'Payload', '-v7.3');
    if Verbose
        fprintf('crossIDCatsHTM: wrote %s\n', Path);
    end
end

% ======================================================================
function localWriteOut(OutFile, Formats, XidTable, TableForm, Cats_cone, Summary, Verbose)
    % Write the cross-id results to disk (.mat and/or .csv).
    %   XidTable  - the object to store in the .mat (matches the return type:
    %               AstroCatalog or table).
    %   TableForm - the full MATLAB table (with OriginCat) used for the .csv.
    if ischar(Formats) || isstring(Formats)
        Formats = cellstr(Formats);
    end
    % strip any extension the user supplied; both files share the stem
    [Path, Base] = fileparts(OutFile);
    if isempty(Path)
        Path = pwd;
    end
    Stem = fullfile(Path, Base);

    if any(strcmpi(Formats, 'mat'))
        MatFile = [Stem '.mat'];
        save(MatFile, 'XidTable', 'Cats_cone', 'Summary', '-v7.3');
        if Verbose
            fprintf('crossIDCatsHTM: wrote %s\n', MatFile);
        end
    end
    if any(strcmpi(Formats, 'csv'))
        CsvFile = [Stem '.csv'];
        writetable(TableForm, CsvFile);
        if Verbose
            fprintf('crossIDCatsHTM: wrote %s\n', CsvFile);
        end
    end
    if any(strcmpi(Formats, 'json'))
        % lean, portable sidecar carrying just the per-catalog version
        % signatures, so a CSV/DB export of the pointer table stays validatable
        % (catsHTM.checkCatalogSignature) without the full .mat Summary.
        JsonFile = [Stem '_signature.json'];
        if isfield(Summary, 'Signature') && ~isempty(fieldnames(Summary.Signature))
            Fid = fopen(JsonFile, 'w');
            if Fid < 0
                warning('catsHTM:crossIDCatsHTM:jsonOpen', ...
                    'Could not open %s for writing.', JsonFile);
            else
                fwrite(Fid, jsonencode(Summary.Signature));
                fclose(Fid);
                if Verbose
                    fprintf('crossIDCatsHTM: wrote %s\n', JsonFile);
                end
            end
        elseif Verbose
            fprintf(['crossIDCatsHTM: no Summary.Signature to write as JSON ', ...
                '(StampSignature off?); skipped %s\n'], JsonFile);
        end
    end
end


% ======================================================================
function [Block, ColNm, Msg] = localGatherOne(Source, Name, VarNames, GetCol, Cats_cone, Args, Nglobal)
    % Gather one catalog's columns from either the Cats_cone snapshot (Ind_<Cat>)
    % or the catsHTM store (CellID_<Cat>/RowInCell_<Cat>). Returns a non-empty
    % Msg when the catalog is skipped (missing columns / not in Cats_cone).
    Block = []; ColNm = {}; Msg = '';
    if strcmp(Source, 'cats')
        IndName = ['Ind_' Name];
        if ~ismember(IndName, VarNames)
            Msg = sprintf('no %s column in T; skipping %s', IndName, Name);
            return;
        end
        if ~isfield(Cats_cone, Name)
            Msg = sprintf('%s not in Cats_cone; skipping', Name);
            return;
        end
        Cat = localGetCat(Cats_cone.(Name));
        Ind = GetCol(IndName);
        [ColIdx, ColNm] = localResolveColumns(Cat, Args.Columns, Name);
        Block = repmat(Args.FillValue, Nglobal, numel(ColIdx));
        Ok    = ~isnan(Ind);
        if any(Ok) && ~isempty(ColIdx)
            Block(Ok, :) = Cat.Catalog(Ind(Ok), ColIdx);
        end
    else   % 'pointer'
        CidName = ['CellID_' Name];
        RicName = ['RowInCell_' Name];
        if ~ismember(CidName, VarNames) || ~ismember(RicName, VarNames)
            Msg = sprintf(['no %s/%s columns in T (run crossIDCatsHTM with ', ...
                'AddPointer); skipping %s'], CidName, RicName, Name);
            return;
        end
        Cid    = GetCol(CidName);
        Ric    = GetCol(RicName);
        Wanted = localWantedColumns(Args.Columns, Name);
        % pass this catalog's stored signature (if any) so gatherByPointer can
        % refuse to dereference pointers into a changed catalog build.
        Sig = [];
        if isstruct(Args.Signature) && isfield(Args.Signature, Name)
            Sig = Args.Signature.(Name);
        end
        [Block, ColNm] = catsHTM.gatherByPointer(Name, Cid, Ric, ...
            'Columns', Wanted, 'CatDir', Args.CatDir, 'FillValue', Args.FillValue, ...
            'Signature', Sig, 'ValidateSig', Args.ValidateSig);
        ColNm = ColNm(:).';
    end
end

% ======================================================================
function Cat = localGetCat(Entry)
    % Return an AstroCatalog from either an object or a .mat path.
    if ischar(Entry) || isstring(Entry)
        L   = load(char(Entry));
        Cat = L.Cat;
    else
        Cat = Entry;
    end
end

% ======================================================================
function [ColIdx, ColNm] = localResolveColumns(Cat, Columns, Name)
    % Resolve the column indices/names to pull from a Cats_cone catalog object.
    AllNames = Cat.ColNames;
    Wanted   = localWantedColumns(Columns, Name);
    if isempty(Wanted)
        Wanted = AllNames;                       % all columns
    end
    % keep only columns that exist in this catalog, preserving requested order
    [Tf, Loc] = ismember(Wanted, AllNames);
    ColIdx    = Loc(Tf);
    ColNm     = AllNames(ColIdx);
    ColIdx    = ColIdx(:).';
    ColNm     = ColNm(:).';
end

% ======================================================================
function Wanted = localWantedColumns(Columns, Name)
    % The requested column list for a given catalog (cellstr, or {} = all).
    if isempty(Columns)
        Wanted = {};
    elseif isstruct(Columns)
        if isfield(Columns, Name)
            Wanted = Columns.(Name);
        else
            Wanted = {};
        end
    else
        Wanted = Columns;
    end
    if ischar(Wanted) || isstring(Wanted)
        Wanted = cellstr(Wanted);
    end
end

% ======================================================================
function Written = localWriteData(OutFile, Formats, Data, Verbose)
    % Write the gathered table to .mat and/or .csv (stem-shared).
    if ischar(Formats) || isstring(Formats)
        Formats = cellstr(Formats);
    end
    [Path, Base] = fileparts(OutFile);
    if isempty(Path)
        Path = pwd;
    end
    Stem    = fullfile(Path, Base);
    Written = {};
    if any(strcmpi(Formats, 'mat'))
        MatFile = [Stem '.mat'];
        save(MatFile, 'Data', '-v7.3');
        Written{end+1} = MatFile;
        if Verbose
            fprintf('gatherCrossIDData: wrote %s\n', MatFile);
        end
    end
    if any(strcmpi(Formats, 'csv'))
        CsvFile = [Stem '.csv'];
        writetable(Data, CsvFile);
        Written{end+1} = CsvFile;
        if Verbose
            fprintf('gatherCrossIDData: wrote %s\n', CsvFile);
        end
    end
end
