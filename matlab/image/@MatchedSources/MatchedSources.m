% MatchedSources class - A container for matrices of matched sources
% Properties:
%   Data  - A structure, in which each field is a matrix of identical sizes
%           containing Nepoch X Nsrc measurments of some property.
%   JD     - A vector of times per epoch. If not set by user default is a
%            vector of 1..Nepoch.
%   Fields - A cell array of field names (Dependent)
%   Nsrc   - Number of sources in each matrix.
%   Nepoch - Number of epochs in each matrix.
%   DimEpoch - Dim of epoch (constant) = 1
%   DimSrc   - Dim of sources (constant) = 2
% Methods:
%   read (static) - Read a mat/hdf5 into MatchedSources
%   write - Write a MatchedSources into mat/hdf5 file.
%   addMatrix - Add matrix/struct/matched AstroTable into the MatchedSources Data.
%   getMatrix - Get matrix using field name.
%   summary   - Summary of a specific field matrix in MatchedSources.
%   plotRMS   - plot rms of mag vs. mag

% #functions (autogen)
% addMatrix - Add matrix/struct/matched AstroTable into the MatchedSources Data Obj = addMatrix(Obj, Matrix, FieldName)
% deleteMatrix - remove matrix and field name from an MatchedSources object
% designMatrix - Generate a general purpose design matrix from an MatchedSources object Description: Construct a design matrix of the form: H = [FunCell{1}(Col1), FunCell{2}(Col2), ...] where the FunCell are user provided functionals, and Col are the MatchedSources matrix of a
% designMatrixCalib - Generate the design matrix for relative photometric calibration Reference: Ofek+2011
% get.Fields - getter for Fields
% get.JD - getter for JD (return 1..Nepoch) if doesnt exist
% get.Nepoch - getter for Nepoch
% get.Nsrc - getter for Nsrc
% getFieldNameDic - Get field name in MatchedSources Data properties that first appear in a dictionary (cell array).
% getLC_ind - get the LC [JD, Mag] of a source by its index (column number)
% getLonLat - Get data matrices containing the RA/Dec fields.
% getMatrix - Get matrix using field name
% notNanEpochs - Return a vector of logicals indicating epochs which do have any NaNs in their data.
% notNanSources - Return a vector of logicals indicating soueces which do have any NaNs in theor data.
% plotRMS - plot rms of a propery (field) vs. its mean.
% read - read mat file or HDF5 file containing MatchedSources
% statSummary - Calculate statistical summary properties of a data property in a MatchedSources object.
% summary - Summary of a specific field matrix in MatchedSources
% write - Write a MatchedSources object to HDF5 or mat file
% #/functions (autogen)
%

classdef MatchedSources < Component
    properties
        Data(1,1) struct  % each field [Nepoch, Nsrc]
        Units(1,1) struct % each field [Units]
        JD                % time per epoch
    end
    
    properties (Dependent)
        Fields cell
    end
    properties (Dependent)
        Nsrc(1,1)
        Nepoch(1,1)
    end
    
    properties (Constant)
        DimEpoch = 1;
        DimSrc   = 2;
        
        DefNamesX cell                   = {'X','X_IMAGE','XWIN_IMAGE','X1','X_PEAK','XPEAK'};
        DefNamesY cell                   = {'Y','Y_IMAGE','YWIN_IMAGE','Y1','Y_PEAK','YPEAK'};
        DefNamesRA cell                  = {'RA','ALPHA','ALPHAWIN_J2000','ALPHA_J2000','RA_J2000','RAJ2000','RightAsc'};
        DefNamesDec cell                 = {'Dec','DEC','DELTA','DELTAWIN_J2000','DELTA_J2000','DEC_J2000','DEJ2000','Declination'};
        DefNamesErrRA cell               = {'RAERR','RA_ERR','ALPHAERR','ALPHA_ERR'};
        DefNamesErrDec cell              = {'DecErr','DECERR','DEC_ERR','DELTAERR','DELTA_ERR'};
        DefNamesMag cell                 = {'Mag','PSF_MAG','MAG_PSF','Mag_BP','Mag_G','Mag_RP','MAG_CONV_2'};
    end
   
    methods % constructor
        
    end
    
    methods % setters/getters
        function Result = get.JD(Obj)
            % getter for JD (return 1..Nepoch) if doesnt exist
            if isempty(Obj.JD)
                Result = (1:1:Obj.Nepoch).';
            else
                Result = Obj.JD;
            end
            Obj.JD = Result;
            
        end
        
        function Result = get.Nsrc(Obj)
            % getter for Nsrc
            FN = fieldnames(Obj.Data);
            if isempty(FN)
                Result = [];
            else
                Result = size(Obj.Data.(FN{1}), Obj.DimSrc);
            end
        end
        
        function Result = get.Nepoch(Obj)
            % getter for Nepoch
            FN = fieldnames(Obj.Data);
            if isempty(FN)
                Result = [];
            else
                Result = size(Obj.Data.(FN{1}), Obj.DimEpoch);
            end
        end
        
        function Result = get.Fields(Obj)
            % getter for Fields
            Result = fieldnames(Obj.Data);
        end
    end
    
    methods (Static) % static read
        function Obj = read(FileName, Args)
            % read mat file or HDF5 file containing MatchedSources
            % Input  : - A file name.
            %          * ...,key,val,...
            %            'Fields' - A field, or cell array of fields to
            %                   read from an HDF5 file. In the case of a
            %                   mat file, all fields are read.
            %            'FileType' - One of the following:
            %                   ['auto'] - will attempt to identify file type
            %                           by its extension.
            %                   'hdf5' - An HDF5 file, in which the
            %                           datasets are the field names.
            %                   'mat' - A mat file containing a structure
            %                           or an MatchedSources object.
            % Output : - A MatchedSources object.
            % Author : Eran Ofek (Jun 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix({rand(100,200),rand(100,200)},{'FLUX','MAG'})
            %          MS.write('try.hdf5')
            %          clear MS;
            %          MS = MatchedSources.read('try.hdf5');
            %          MS1 = MatchedSources.read('try.hdf5','Fields','FLUX');
            %          delete('try.hdf5');
            
            arguments
                FileName char
                Args.Fields                  = [];  % read all fields
                Args.FileType char           = 'auto';  % 'hdf5' | 'mat' | 'auto'
            end
           
            switch lower(Args.FileType)
                case 'auto'
                    [~,~, Ext] = fileparts(FileName);
                    Args.FileType = Ext(2:end);
            end
            
            switch lower(Args.FileType)
                case {'hdf5','h5','hd5'}
                    if isempty(Args.Fields)
                        % read all fields
                        Info  = h5info(FileName);
                        Ndata = numel(Info.Datasets);
                        for Idata=1:1:Ndata
                            DS   = sprintf('/%s',Info.Datasets(Idata).Name);
                            Data = h5read(FileName, DS);
                            Struct.(Info.Datasets(Idata).Name) = Data;
                        end
                    else
                        if ischar(Args.Fields)
                            Args.Fields = {Args.Fields};
                        end
                        Ndata = numel(Args.Fields);
                        for Idata=1:1:Ndata
                            Data = h5read(FileName, sprintf('/%s',Args.Fields{Idata}));
                            Struct.(Args.Fields{Idata}) = Data;
                        end
                    end
                        
                case {'mat'}
                    % read mat file
                    % assume contains a structure
                    Struct = io.files.load2(FileName);
                    if isa(Struct, 'MatchedSources')
                        Struct = Struct.Data;
                    end
                otherwise
                    error('Unknown FileType');
            end
            Obj = MatchedSources;
            Obj.addMatrix(Struct);
            
        end
    end
    
    methods % write
        function Result = write(Obj, FileName, Args)
            % Write a MatchedSources object to HDF5 or mat file
            % Input  : - A single element MatchedSources object.
            %          - FileName to write.
            %          * ...,key,val,...
            %            'FileType' - Options are:
            %                   ['hdf5'] - HDF5 file with dataset named like
            %                       the field names.
            %                   'mat' - Save the Data struct to a mat file.
            %                   'matobj' - Save the entire MatchedSource object to
            %                       a mat file.
            % Output : - Return true if sucess.
            % Author : Eran Ofek (Jun 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix({rand(100,200),rand(100,200)},{'FLUX','MAG'})
            %          MS.write('try.hdf5')
            
            arguments
                Obj(1,1)
                FileName
                Args.FileType      = 'hdf5';
            end
           
            switch lower(Args.FileType)
                case {'h5','hdf5','hd5'}
                    Ndata = numel(Obj.Fields);
                    for Idata=1:1:Ndata
                        h5create(FileName, sprintf('/%s',Obj.Fields{Idata}), size(Obj.Data.(Obj.Fields{Idata})));
                        h5write(FileName, sprintf('/%s',Obj.Fields{Idata}), Obj.Data.(Obj.Fields{Idata}));
                    end
                case {'mat'}
                    % save the Data structure
                    save(FileName, Obj.Data, '-v7.3');
                case {'matobj'}
                    % save the MatchedSources as object
                    save(FileName, Obj, '-v7.3');
                otherwise
                    error('Unknown FileType option');
            end
           
            Result = true;
        end
    end
    
    methods (Static)
        function H=designMatrixCalib(Nep, Nsrc, Args)
            % Generate the design matrix for relative photometric calibration
            % Reference: Ofek+2011
            % Input  : - Number of epochs.
            %          - Number of sources.
            %          * ...,key,val,...
            %            'UseSparse' - A logical indicating if to generate
            %                   a sparse design matrix. Default is false.
            %            'SrcProp' - A cell array of additional properties
            %                   to add to the design matrix.
            %                   Each cell element can be a row vector
            %                   (property per source; e.g., color), a
            %                   column vector (property per epoch; e.g.,
            %                   air mass), or a matrix of size Nep X Nsrc.
            %                   These properties will be added to the
            %                   design matrix according to the scheme
            %                   dictated by 'SrcPropCoefType'.
            %                   Default is {}.
            %            'SrcPropCoefType' - A vector of numbers, each
            %                   element corresponds to a cell element in
            %                   'SrcProp'. The numbres may be one of the
            %                   following:
            %                   1 - will add a single column to the design
            %                   matrix (i.e., a single coef.).
            %                   2 - will add a column per epoch.
            % Output : - The design matrix.
            % Author : Eran Ofek (Jul 2021)
            % Example: H=MatchedSources.designMatrixCalib(2,4)
            %          H=MatchedSources.designMatrixCalib(2,4,'UseSparse',true)
            %          H=MatchedSources.designMatrixCalib(2,4,'SrcProp',{3.*ones(1,4)})
            %          H=MatchedSources.designMatrixCalib(2,4,'SrcProp',{3.*ones(1,4)},'SrcPropCoefType',2)
            
            arguments
                Nep
                Nsrc
                Args.UseSparse(1,1) logical  = false;
                Args.SrcProp cell            = {};
                Args.SrcPropCoefType         = 1;  % 1 - single coef.
                
            end
            
            Nprop = numel(Args.SrcProp);
            % count additional required columns
            NextraCol = 0;
            FilledVal = 0;   % number of non zero values (for sparse mat)
            for Iprop=1:1:Nprop
                switch Args.SrcPropCoefType(Iprop)
                    case 1
                        % single coef
                        NextraCol = NextraCol + 1;
                    case 2
                        % coef per epoch
                        NextraCol = NextraCol + Nep;
                    otherwise
                        error('Unknown SrcPropCoefType option');
                end
                FilledVal = FilledVal + Nsrc.*Nep;
            end
            
            if Args.UseSparse
                Nnonzero = 2.*Nep.*Nsrc + FilledVal;
                sparse([],[],[],Nep.*Nsrc, Nsrc + Nep + NextraCol, Nnonzero);
            else
                H = zeros(Nep.*Nsrc, Nsrc + Nep + NextraCol);
            end
            DiagMat = diag(ones(Nep,1));
            OnesVec = ones(Nep,1);
            
            %LinesVec  = (1:Nsrc);        % column indices for the one-vec matrices
            LinesDiag = Nsrc + (1:Nep);  % column indices for the diag matrices
            for Isrc=1:1:Nsrc
                Rows = (1:Nep).' + (Isrc-1).*Nep;
                H(Rows, Isrc)      = OnesVec;
                H(Rows, LinesDiag) = DiagMat;
            end
            
            % adding optional blocks
            ColInd = Nsrc + Nep;
            for Iprop=1:1:Nprop
                % check what is the nature of the additional property
                % 1. A full matrix with number per source/epoch, and a
                % single free coef.
                % 2. A full matrix with number per source/epoch, and a
                % coef. per epoch.
                % 3. A full matrix with number per source/epoch, and a
                % coef. per source.
                % 4. A vector of peroerties per source, and a single common
                % coef.
                % 5. Like 4, but a coef. oer epoch.
               
                
                switch Args.SrcPropCoefType(Iprop)
                    case 1
                        % common property for all sources, at all epochs
                        % and a single free coef.
                        ColInd = ColInd + 1;
                        if size(Args.SrcProp{Iprop},1) == 1 && numel(Args.SrcProp{Iprop}) == Nsrc
                            % input is a row vector (parameter per source)
                            % Args.SrcProp{Iprop} is a vector of length Nsrc
                            Tmp = repmat(Args.SrcProp{Iprop}(:), 1, Nep).';
                            H(:,ColInd) = Tmp(:);
                        elseif size(Args.SrcProp{Iprop},2) == 1 && numel(Args.SrcProp{Iprop}) == Nep
                            % input is a column vector [parameter per
                            % epoch]
                            Tmp = repmat(Args.SrcProp{Iprop}, Nep, 1);
                            H(:,ColInd) = Tmp;
                        else
                            % assume Args.SrcProp{Iprop} is a matrix of size
                            % Nep x Nsrc
                            H(:,ColInd) = Args.SrcProp{Iprop}(:);
                        end
                    case 2
                        % common property for all sources, at all epochs
                        % but a free coef per epoch
                        
                        if size(Args.SrcProp{Iprop},1) == 1 && numel(Args.SrcProp{Iprop}) == Nsrc
                            % input is a row vector (parameter per source)
                            DiagMatProp = diag(ones(1,Nep));
                                                        
                            LinesDiag = ColInd + (1:Nep);  % column indices for the diag matrices
                            for Isrc=1:1:Nsrc
                                Rows = (1:Nep).' + (Isrc-1).*Nep;
                                H(Rows, LinesDiag) = DiagMatProp.*Args.SrcProp{Iprop}(Isrc);
                            end
                            ColInd = LinesDiag(end);
                            
                        else
                            error('SrcProp for prop %d must be a vector of length Nsrc',Iprop);
                        end
                        
                    otherwise
                        error('Unknown SrcPropCoefType option');
                end
                    
                        
            end
        end
        
    end
    
    methods  % functions / get/set Data
        function Obj = addMatrix(Obj, Matrix, FieldName, Units)
            % Add matrix/struct/matched AstroTable into the MatchedSources Data
            % Obj = addMatrix(Obj, Matrix, FieldName)
            % Input  : - A single element MatchedSources object.
            %          - One of the following inputs:
            %            1. A numeric matrix. In this case, FieldName must
            %            be a char array with field name in which to store
            %            the matrix.
            %            2. A cell array of matrices. In this case
            %            FieldName must be a cell array of field names, per
            %            matrix.
            %            3. A struct. In this case the struct will be
            %            copied to the Data field.
            %            4. A matched AstroTable/AstroCatalog object
            %            In this case, the columns will be selected from
            %            FieldName.
            %          - A field name (or a cell array of field names).
            %            If the field already exist, then replace the
            %            current content with the new content.
            %          - A cell array of units per field.
            %            If empty, will generate empty units.
            %            Default is {}.
            % Output : - A MatchedSources object.
            % Author : Eran Ofek (Jun 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix(rand(100,200),'FLUX')
            %          MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            %          MS = MatchedSources;
            %          St.Flux=rand(100,200); MS.addMatrix(St);
            
            
            arguments
                Obj(1,1)
                Matrix
                FieldName   = [];
                Units       = {};
            end
            
            if ischar(FieldName)
                FieldName = {FieldName};
            end
                
            if ischar(Units)
                Units = {Units};
            end
            
            Nf = numel(FieldName);
            if numel(Units) ~= Nf
                [Units{1:1:Nf}] = deal('');
            end
                
            if isnumeric(Matrix)
                % matrix is numeric - add
                Obj.Data.(FieldName{1})  = Matrix;
                Obj.Units.(FieldName{1}) = Units{1};
            elseif isstruct(Matrix)
                % store the struct as is in Data
                FN = fieldnames(Matrix);
                for Ifn=1:1:numel(FN)
                    Obj.Data.(FN{Ifn}) = Matrix.(FN{Ifn});
                    %Obj.Units.(FieldName{Ifn}) = Units{Ifn};
                end
            elseif iscell(Matrix)
                Ncell = numel(Matrix);
                for Icell=1:1:Ncell
                    Obj.Data.(FieldName{Icell}) = Matrix{Icell};
                    Obj.Units.(FieldName{Icell}) = Units{Icell};
                end
            elseif isa(Matrix, 'AstroTable')
                % Assume input is an array of matched AstroTables
                [Nrow, Ncol] = Matrix.sizeCatalog;
                if ~all(Nrow==Nrow(1))
                    error('For AstroTable/AstroCatalog input, all catalogs must have the same number of rows');
                end
                            
                [Res, Summary, N_Ep, Units] = imProc.match.matched2matrix(Matrix, FieldName, true);
                Obj.addMatrix(Res);
                for Ifn=1:1:Nf
                    Obj.Units.(FieldName{Ifn}) = Units.(FieldName{Ifn});
                end
                
%
%                 Nepoch = numel(Matrix);
%                 Nfn = numel(FieldName);
%                 for Ifn=1:1:Nfn
%                     Obj.Data.(FieldName{Ifn}) = nan(Nepoch, Nrow(1));
%                     for Iepoch=1:1:Nepoch
%                         ColVal = getCol(Matrix(Iepoch), FieldName{Ifn});
%                         Obj.Data.(FieldName{Ifn})(Iepoch,:) = ColVal.';
%                     end
%                 end
                        
            else
                error('Unknown Matrix input option');
            end
            
        end
        
        function [Result, Matched] = unifiedCatalogsIntoMatched(Obj, AT, Args)
            % Unify and match AstroCatalog objects into a MatchedSources object.
            %   Given a vector of AstroCatalog objects use imProc.match.unifiedSourcesCatalog
            %   to generate a matched and unified catalogs that contains
            %   all the "individual" sources in all the images.
            %   Next, this matched AstroCatalog version is converted into a
            %   MatchedSources object.
            % Input  : - An (empty) MatchedSources object.
            %          - A vector of AstroCatalog or AstroImage objects of sources
            %            observed in a single field over multiple epochs.
            %          * ...,key,val,...
            %            'JD' - A vector of JD (one per AstroCatalog
            %                   element). If empty, then will set this to
            %                   1:Nepochs. Default is Nepochs.
            %            'CooType' - Matching CooType: 'pix' | ['sphere'].
            %            'Radius' - Matching radius. Default is 3.
            %            'RadiusUnits' - Matching radius units.
            %                   Default is 'arcsec'.
            %            'unifiedSourcesCatalogArgs' - A cell array of
            %                   additional arguments to pass to imProc.match.unifiedSourcesCatalog
            %            'MatchedColums' - A cell arry of columns to insert
            %                   into the the MatchedSources object.
            %                   Default is {'RA','Dec','X','Y','SN_1','SN_2','SN_3','SN_4','MAG_CONV_2','MAGERR_CONV_2','MAG_CONV_3','MAGERR_CONV_3','FLAGS'};
            %            'CreateNewObj' - A logical indicating if to
            %                   generate a new copy of the MatchedSources object.
            %                   Default is false.
            % Output : - A MatchedSources object.
            %          - The matched AstroCatalog.
            % Author : Eran Ofek (Nov 2021)
            % Example: see usage in pipeline.generic.mergeCatalogs
            
            arguments
                Obj
                AT
                Args.JD                              = []; % of empty put 1:N
                Args.CooType                         = 'sphere';
                Args.Radius                          = 3;
                Args.RadiusUnits                     = 'arcsec';
                Args.unifiedSourcesCatalogArgs cell  = {};
                Args.MatchedColums cell              = {'RA','Dec','X','Y','SN_1','SN_2','SN_3','SN_4','MAG_CONV_2','MAGERR_CONV_2','MAG_CONV_3','MAGERR_CONV_3','FLAGS'};
                
                Args.CreateNewObj logical            = false;
            end
           
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            %[Nepochs, Nfields] = size(AT);
            Nepochs = numel(AT);
            
            if isempty(Args.JD)
                Args.JD = (1:1:Nepochs).';
            else
                Args.JD = Args.JD(:);
            end
            
            Ifields = 1;
            [~, ~, Matched(Ifields,:)] = imProc.match.unifiedSourcesCatalog(AT, 'CooType',Args.CooType,...
                                                             'Radius',Args.Radius,...
                                                             'RadiusUnits',Args.RadiusUnits,...
                                                             Args.unifiedSourcesCatalogArgs{:});

            Result(Ifields).addMatrix(Matched(Ifields,:), Args.MatchedColums);
            % populate JD
            Result(Ifields).JD = Args.JD;  
            
        end
        
        function varargout = getMatrix(Obj, FieldNames)
            % Get matrix using field name
            % Input  : - A single element MatchedSources object.
            %          - Field name, or a cell array of field names.
            % Output : - A mtrix of [Epoch X Source] for each requested
            %            field.
            % Author : Eran Ofek (Jun 2021)
            % Example: MS = MatchedSources; MS.Data.FLUX = rand(100,200);
            %          A = MS.getMatrix('FLUX');
            
            arguments
                Obj(1,1)
                FieldNames
            end
           
            if ischar(FieldNames)
                FieldNames = {FieldNames};
            end
            
            Nfn = numel(FieldNames);
            if nargout>Nfn
                error('Number of output arguments is larger than the number of requested fields');
            end
            varargout = cell(1, nargout);
            for Ifn=1:1:nargout
                varargout{Ifn} = Obj.Data.(FieldNames{Ifn});
            end
            
            
        end
        
        function Result = getUnits(Obj, FieldNames)
            % Return the units
            % Input  : - A single element MatchedSources object.
            %          - A char or a cell array of field names.
            % Output : - A char or cell array (according to input) units.
            % Author : Eran Ofek (Nov 2021)
            % Example: Result = getUnits(MS, 'FLUX')
            
            arguments
                Obj(1,1)
                FieldNames
            end
           
            if ischar(FieldNames)
                Result = Obj.Units.(FieldNames);
            elseif iscell(FieldNames)
                Nf = numel(FieldNames);
                Result = cell(1, Nf);
                for If=1:1:Nf
                    Result{If} = Obj.Units.(FieldNames{If});
                end
            else
                error('Unknown FieldNames type - must be a char or cell');
            end
            
        end
        
        function Obj = deleteMatrix(Obj, FieldName)
            % remove matrix and field name from an MatchedSources object
            % Input  : - A MatchedSources object
            %          - A field name, or a cell array of field names.
            % Output : - A MatchedSources object
            % Author : Eran Ofek (Jun 2021)
            % Example: MS.deleteMatrix('X2')
           
            if ischar(FieldName)
                FieldName = {FieldName};
            end
            
            for Ifn=1:1:numel(FieldName)
                Obj = rmfield(Obj.Data, FieldName{Ifn});
                Obj = rmfield(Obj.Units, FieldName{Ifn});
            end
            
        end
        
        function Result = summary(Obj, Field)
            % Summary of a specific field matrix in MatchedSources
            % Input  : - A single element MatchedSources object.
            %          - A char array of field name. If empty, use the
            %            first field. Default is ''.
            % Output : - A structure of summary information
            %            .Nepoch
            %            .Nsrc
            %            .NepochSrcAppear
            %            .NsrcInEpoch
            % Author : Eran Ofek (Jun 2021)
            % Example: MS=MatchedSources;
            %          MS.addMatrix(rand(100,200),'FLUX');
            %          MS.summary
            %          MS.summary('FLUX')
            
            arguments
                Obj(1,1)
                Field char          = '';
            end
            
            if isempty(Field)
                % select first field
                FN = fieldnames(Obj.Data);
                Field = FN{1};
            end
            
            Mat = Obj.Data.(Field);
            % summary
            Result.Nepoch = Obj.Nepoch;
            Result.Nsrc   = Obj.Nsrc;
            % Number of epoch in which each source appears
            Result.NepochSrcAppear = sum(~isnan(Mat), Obj.DimEpoch);
            % Number of sources apeear in each epoch
            Result.NsrcInEpoch     = sum(~isnan(Mat), Obj.DimSrc);
            
        end
        
        function [FieldName] = getFieldNameDic(Obj, Dic)
            % Get field name in MatchedSources Data properties that first
            % appear in a dictionary (cell array).
            % Input  : - A Matched Sources object.
            %          - A cell array of dictionary (i.e., alternative
            %            field names corresponding to a single field.
            % Output : - A string of field name.
            % Author : Eran Ofek (Sep 2021)
            % Example: Obj = MatchedSources;
            %          Obj.addMatrix(rand(30,40),'RA');
            %          Obj.addMatrix(rand(30,40),'Dec');
            %          [FieldName] = getFieldNameDic(Obj, MatchedSources.DefNamesDec)
            
            FN = fieldnames(Obj.Data);
            Flag = ismember(FN, Dic);
            FieldName = FN(Flag);
            if isempty(FieldName)
                FieldName = '';
            else
                FieldName = FieldName{1};
            end
        end
        
        function [MatRA, MatDec, MatErrRA, MatErrDec] = getLonLat(Obj)
            % Get data matrices containing the RA/Dec fields.
            % Input  : - A MatchedSources object.
            % Output : - A matrix of RA (longitude).
            %          - A matrix of Dec (latitude).
            %          - A matrix of RA errors.
            %          - A matrix of Dec errors.
            % Author : Eran Ofek (Sep 2021)
            % Example: Obj = MatchedSources;
            %          Obj.addMatrix(rand(30,40),'RA');
            %          Obj.addMatrix(rand(30,40),'Dec');
            %          [MatRA, MatDec] = getLonLat(Obj)
           
            [FieldRA]     = getFieldNameDic(Obj, Obj.DefNamesRA);
            [FieldDec]    = getFieldNameDic(Obj, Obj.DefNamesDec);
            [FieldErrRA]  = getFieldNameDic(Obj, Obj.DefNamesErrRA);
            [FieldErrDec] = getFieldNameDic(Obj, Obj.DefNamesErrDec);
            
            if isempty(FieldRA)
                MatRA = [];
            else
                MatRA     = Obj.Data.(FieldRA);
            end
            if isempty(FieldDec)
                MatDec = [];
            else
                MatDec    = Obj.Data.(FieldDec);
            end
            if isempty(FieldErrRA)
                MatErrRA = [];
            else
                MatErrRA  = Obj.Data.(FieldErrRA);
            end
            if isempty(FieldErrDec)
                MatErrDec = [];
            else
                MatErrDec = Obj.Data.(FieldErrDec);
            end
            
        end
    end
    
    methods % design matrix
        function [H, Y, ErrY] = designMatrix(Obj, ColNames, FunCell, ColNameY, FunY, ColNameErrY, FunErrY)
            % Generate a general purpose design matrix from an MatchedSources object
            % Description: Construct a design matrix of the form:
            %              H = [FunCell{1}(Col1), FunCell{2}(Col2), ...]
            %              where the FunCell are user provided functionals,
            %              and Col are the MatchedSources matrix of a
            %              specific column name after vectorization (i.e.,
            %              Matrix(:)).
            %              Also constrict a Y and ErrY vectors.
            % Input  : - A single element MatchedSources object.
            %          - A cell array of column names from which to
            %            construct the design matrix.
            %          - (FunCell) A cell array of functional to operate on each
            %            corresponding column name. Each element in the
            %            cell may be a function_handle, or an integer
            %            representing the power to apply to the column.
            %            If empty element, then the cooresponding column in
            %            the design matrix will contains one (i.e., solve
            %            for a constant).
            %          - Column name for the Y vector.
            %          - A function hanle, numeric, or empty (lie FunCell)
            %            to apply to the Y column name.
            %          - Column name for the ErrY vector.
            %          - A function hanle, numeric, or empty (lie FunCell)
            %            to apply to the ErrY column name.
            % Output : - Design matrix.
            %          - Y column.
            %          - ErrY column.
            % Author : Eran Ofek (Jun 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix(rand(100,200),'FLUX')
            %          MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            %          St.X2=rand(100,200);
            %          MS.addMatrix(St);
            %          [H, Y] = MS.designMatrix({'FLUX','X','Y'},{@sin, 1, 2},'MAG',1);
            %          [H, Y] = MS.designMatrix({[],'X','Y'},{[], 1, 2},'MAG',1);
            %          [H, Y, ErrY] = MS.designMatrix({[],'X','Y'},{[], 1, 2},'MAG',1, 'MAG',2);
            
            arguments
                Obj(1,1)
                ColNames
                FunCell
                ColNameY char
                FunY             = @(x) ones(size(x));
                ColNameErrY char = [];
                FunErrY          = @(x) ones(size(x));
            end
            
            if ~iscell(FunCell)
                FunCell = {FunCell};
            end
            if ischar(ColNames)
                ColNames = {ColNames};
            end
            
            Nfun = numel(FunCell);
            if Nfun~=numel(ColNames)
                error('FunCell and ColNames must contain the same number of elements');
            end
            
            Npt = Obj.Nsrc.*Obj.Nepoch;
            % design matrix
            H   = nan(Npt, Nfun);
            for Ifun=1:1:Nfun
                if isa(FunCell{Ifun}, 'function_handle')
                    H(:,Ifun) = FunCell{Ifun}( Obj.Data.(ColNames{Ifun})(:) );
                else
                    % functional may be [] -> ones, or number -> power
                    if isempty(FunCell{Ifun})
                        % ones
                        H(:,Ifun) = ones(Npt,1);
                    else
                        % power
                        H(:,Ifun) = Obj.Data.(ColNames{Ifun})(:).^FunCell{Ifun};
                    end
                end
            end
            
            % Y column
            if nargout>1
                if isa(FunY, 'function_handle')
                    Y = FunY( Obj.Data.(ColNameY)(:) );
                else
                    % functional may be [] -> ones, or number -> power
                    if isempty(FunY)
                        % ones
                        Y = ones(Npt,1);
                    else
                        % power
                        Y = Obj.Data.(ColNameY)(:).^FunY;
                    end
                end
                
                % ErrY column
                if nargout>2
                    if isa(FunErrY, 'function_handle')
                        ErrY = FunY( Obj.Data.(ColNameErrY)(:) );
                    else
                        % functional may be [] -> ones, or number -> power
                        if isempty(FunErrY)
                            % ones
                            ErrY = ones(Npt,1);
                        else
                            % power
                            ErrY = Obj.Data.(ColNameErrY)(:).^FunErrY;
                        end
                    end
                end
                
            end
             
        end
                
        function Flag = notNanSources(Obj, ColNames)
            % Return a vector of logicals indicating soueces which do have any NaNs in theor data.
            % Input  : - A single element MatchesSources object.
            %          - A field name, a cell array of field names, or
            %            empty. If empty, will use all field names.
            %            For each field name, will flag sources which
            %            contains NaN's in their columns.
            %            Defailt is empty.
            % Output : - A row vector of logicals. True for sources that
            %            non of their values in their columns contains NaN.
            %            This is calculated over all requested fields, as
            %            an or operation (i.e., if one of the columns in
            %            one of the fields contains NaN, the star will be
            %            flagged).
            % Author : Eran Ofek (Jun 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix(rand(100,200),'FLUX')
            %          MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            %          St.X2=rand(100,200);
            %          MS.addMatrix(St);
            %          MS.Data.FLUX(1,1)=NaN;
            %          Flag = notNanSources(MS, 'FLUX');
            %          Flag = notNanSources(MS, []); % use all fields
            
            arguments
                Obj(1,1)
                ColNames     = [];
            end
           
            if isempty(ColNames)
                ColNames = Obj.Fields;
            end
            
            if ischar(ColNames)
                ColNames = {ColNames};
            end
            Ncol = numel(ColNames);
            CountNanSrc = zeros(1, Obj.Nsrc);
            for Icol=1:1:Ncol
                CountNanSrc = CountNanSrc + sum(isnan(Obj.Data.(ColNames{Icol})), 1);
            end
            Flag = CountNanSrc==0;
            
        end
        
        function Flag = notNanEpochs(Obj, ColNames)
            % Return a vector of logicals indicating epochs which do have any NaNs in their data.
            % Input  : - A single element MatchesSources object.
            %          - A field name, a cell array of field names, or
            %            empty. If empty, will use all field names.
            %            For each field name, will flag epochs which
            %            contains NaN's in their rows.
            %            Defailt is empty.
            % Output : - A column vector of logicals. True for epochs that
            %            non of their values in their rows contains NaN.
            %            This is calculated over all requested fields, as
            %            an or operation (i.e., if one of the rows in
            %            one of the fields contains NaN, the epoch will be
            %            flagged).
            % Author : Eran Ofek (Jun 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix(rand(100,200),'FLUX')
            %          MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            %          St.X2=rand(100,200);
            %          MS.addMatrix(St);
            %          MS.Data.FLUX(1,1)=NaN;
            %          Flag = notNanEpochs(MS, 'FLUX');
            %          Flag = notNanEpochs(MS, []); % use all fields
            
            arguments
                Obj(1,1)
                ColNames     = [];
            end
           
            if isempty(ColNames)
                ColNames = Obj.Fields;
            end
            
            if ischar(ColNames)
                ColNames = {ColNames};
            end
            Ncol = numel(ColNames);
            CountNanEpoch = zeros(Obj.Nepoch, 1);
            for Icol=1:1:Ncol
                CountNanEpoch = CountNanEpoch + sum(isnan(Obj.Data.(ColNames{Icol})), 2);
            end
            Flag = CountNanEpoch==0;
            
        end
        
    end
    
    methods  % statistics and functions
        function Result = combineFlags(Obj, Args)
            % Combine (bitwise or) the flags of each sourc over all epochs
            % Input  : - - A MatchedSources object.
            %          * ...,key,val,...
            %            'FlagsNameDic' - A cell array of dictionary field names for the
            %                   Flags property for which to combine using or operator for each source.
            %                   If empty, then do not calculate.
            %                   Default is 'FLAGS'.
            %            'FlagsType' -  Afunction handle for FLAGS type.
            %                   Default is @uint32.
            % Output : - A structure array with the column wise (sources)
            %            of bitwise-or combine of all flags.
            % Author : Eran Ofek (Sep 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix(uint32(rand(100,200).*1000),'FLAGS')
            %          Result = combineFlags(MS)
            
            arguments
                Obj
                Args.FlagsNameDic                = 'FLAGS';
                Args.FlagsType                   = @uint32;
            end

            [FlagsName] = getFieldNameDic(Obj(1), Args.FlagsNameDic);
            
            N = numel(Obj);
            for I=1:1:N
                % get Mag matrix
                
                DataFlags       = getMatrix(Obj(I), FlagsName);
                Result(I).FLAGS = tools.array.bitor_array(Args.FlagsType(DataFlags));
            end

        end
        
        function [Result, Funs] = statSummary(Obj, Args)
            % Calculate statistical summary properties of a data property in a
            % MatchedSources object.
            % Input  : - A MatchedSources object.
            %          * ...,key,val,...
            %            'FieldNameDic' - A cell array of dictionary field names for the
            %                   data property for which to calculate statistics.
            %                   Default is 'AstroCatalog.DefNamesMag'.
            %            'Funs' - A structure array of functions to
            %                   calculate. If empty, use default.
            %                   Default is empty.
            %            'FundInd' - Indices or logical of functions in
            %                   'Funs' to use. Default is true (all true).
            % Output : - A structure array with the column wise (sources)
            %            basic statistcs properties for the sources in data
            %            properties. E.g., the mean, median, std, etc, for
            %            each source.
            %          - The sturcture array of functions.
            % Author : Eran Ofek (Sep 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix(rand(100,200),'FLUX')
            %          MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            %          Result = statSummary(MS);

            arguments
                Obj
                Args.FieldNameDic                = AstroCatalog.DefNamesMag;
                Args.Funs                        = [];
                Args.FunInd                      = true;
            end

            
            if isempty(Args.Funs)
                % use default
                I = 0;
                I = I+1;
                Funs(I).Fun  = @mean;
                Funs(I).Name = 'Mean';
                Funs(I).Args = {1, 'omitnan'};
                I = I+1;
                Funs(I).Fun  = @median;
                Funs(I).Name = 'Median';
                Funs(I).Args = {1, 'omitnan'};
                I = I+1;
                Funs(I).Fun  = @std;
                Funs(I).Name = 'Std';
                Funs(I).Args = {[], 1, 'omitnan'};
                I = I+1;
                Funs(I).Fun  = @imUtil.background.rstd;
                Funs(I).Name = 'RStd';
                Funs(I).Args = {1};
                I = I+1;
                Funs(I).Fun  = @range;
                Funs(I).Name = 'Range';
                Funs(I).Args = {'omitnan'};
                I = I+1;
                Funs(I).Fun  = @min;
                Funs(I).Name = 'Min';
                Funs(I).Args = {[], 1, 'omitnan'};
                I = I+1;
                Funs(I).Fun  = @max;
                Funs(I).Name = 'Max';
                Funs(I).Args = {[], 1, 'omitnan'};
                I = I+1;
                Funs(I).Fun  = @(Data,Dim) sum(~isnan(Data),Dim);
                Funs(I).Name = 'Nobs';
                Funs(I).Args = {1};
            else
                Funs = Args.Funs;
            end
            
            if islogical(Args.FunInd)
                Args.FunInd = repmat(Args.FunInd,1,numel(Funs));
            end
            
            % select functions
            Funs = Funs(Args.FunInd);
            
               
            
            % get field name from dictionary
            [FieldName] = getFieldNameDic(Obj(1), Args.FieldNameDic);

            Nfun = numel(Funs);
            
            N = numel(Obj);
            for I=1:1:N
                % get Mag matrix
            
                Data = getMatrix(Obj(I), FieldName);
                
                % calc statistics
                for Ifun=1:1:Nfun
                    Result(I).(Funs(Ifun).Name) = Funs(Ifun).Fun(Data, Funs(Ifun).Args{:});
                end
                
%                 Result(I).Mean     = mean(Data, 1, 'omitnan');
%                 Result(I).Median   = median(Data, 1, 'omitnan');
%                 Result(I).Std      = std(Data, [], 1, 'omitnan');
%                 Result(I).RStd     = imUtil.background.rstd(Data, 1);
%                 Result(I).Range    = range(Data, 1);
%                 Result(I).Min      = min(Data, [], 1, 'omitnan');
%                 Result(I).Max      = max(Data, [], 1, 'omitnan');
%                 Result(I).Nobs     = sum(~isnan(Data), 1);
                
            end

       end
        
    end
    
    methods % plot
        function H = plotRMS(Obj, Args)
            % plot rms of a propery (field) vs. its mean.
            % Input  : - A single element MatchedSources object.
            %          * ...,key,val,...
            %            'FieldX' - A cell array of field names. Will chose
            %                   the first field name that appears in the
            %                   Data structure, and its content will be plotted.
            %                   Default is {'MAG','MAG_PSF','MAG_APER'}.
            %            'PlotSymbol' - A cell array of parameters to pass
            %                   to the plot function.
            %                   Default is
            %                   {'k.','MarkerFaceColor','k','MarkerSize',3}.
            %            'BinSize' - Bin size for plotting binned data
            %                   points. If empty, do not plot binned data.
            %                   Default is [].
            %            'DivideErrBySqrtN' - Divide binned plot errors by
            %                   sqrt(N). Default is true.
            %            'BinMarker' - Default is 'o'.
            %            'BinColor' - Default is 'k'.
            %            'BinMarkerSize' - Default is 6.
            %            'XScale' - Default is 'linear'.
            %            'YScale' - Default is 'log'.
            %            'MeanFun' - Default is @tools.math.stat.median.
            %            'StdFun' - Default is @tools.math.stat.nanstd.
            %            'Xlabel' - Default is 'Mean Mag'.
            %            'Ylabel' - Default is 'RMS'.
            %            'FontSize' - Default is 16.
            %            'Interpreter' - Default is 'latex'
            % Output : - Handle for data points plot.
            % Author : Eran Ofek (Jun 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix(rand(100,200),'MAG_PSF');
            %          MS.plotRMS
            %          MS.plotRMS('BinSize',0.1)
           
            arguments
                Obj(1,1)
                Args.FieldX cell              = {'MAG','MAG_PSF','MAG_APER'};
                Args.PlotSymbol               = {'k.','MarkerFaceColor','k','MarkerSize',3};
                Args.BinSize                  = [];
                Args.DivideErrBySqrtN(1,1) logical = true;
                Args.BinMarker                = 'o';
                Args.BinColor                 = 'k';
                Args.BinMarkerSize            = 6;
                Args.XScale                   = 'linear';
                Args.YScale                   = 'log';
                Args.MeanFun function_handle  = @tools.math.stat.nanmedian;
                Args.StdFun  function_handle  = @tools.math.stat.nanstd;
                Args.Xlabel                   = 'Mean Mag';
                Args.Ylabel                   = 'RMS';
                Args.FontSize                 = 16;
                Args.Interpreter              = 'latex';
            end
        
            if ischar(Args.PlotSymbol)
                Args.PlotSymbol = {Args.PlotSymbol};
            end
            FN = fieldnames(Obj.Data);
            % search for the first FieldX that appears in FN
            Ind = find(ismember(Args.FieldX, FN),1);
            FieldX = Args.FieldX{Ind};
            
            Mat   = Obj.Data.(FieldX);
            % axis x - e.g., mean mag
            AxisX = Args.MeanFun(Mat, Obj.DimEpoch);
            AxisY = Args.StdFun(Mat, [], Obj.DimEpoch);
            
            H = plot(AxisX, AxisY, Args.PlotSymbol{:});
            Hgca = gca;
            Hgca.XScale = Args.XScale;
            Hgca.YScale = Args.YScale;
            
            if ~isempty(Args.BinSize)
                % add bins to plot
                hold on;
                
                B = timeseries.binning([AxisX(:), AxisY(:)], Args.BinSize, [NaN NaN], {'MidBin', @mean, @std, @numel});
                if Args.DivideErrBySqrtN
                    plot.errorxy([B(:,1), B(:,2), B(:,3)./sqrt(B(:,4))],'Marker',Args.BinMarker,'MarkerEdgeColor',Args.BinColor,'MarkerFaceColor',Args.BinColor,'MarkerSize',Args.BinMarkerSize);
                else
                    plot.errorxy([B(:,1), B(:,2), B(:,3)],'Marker',Args.BinMarker,'MarkerEdgeColor',Args.BinColor,'MarkerFaceColor',Args.BinColor,'MarkerSize',Args.BinMarkerSize);
                end
                
                hold off;
            end
            
            Hx = xlabel(Args.Xlabel);
            Hx.FontSize = Args.FontSize;
            Hx.Interpreter = Args.Interpreter;
            Hy = ylabel(Args.Ylabel);
            Hy.FontSize = Args.FontSize;
            Hy.Interpreter = Args.Interpreter;
            
        end
        
        % get LC by source index
        function [JD, Mag] = getLC_ind(Obj, Ind, FieldMag)
            % get the LC [JD, Mag] of a source by its index (column number)
            % Input  : - A single-element MatchedSources object
            %          - The index/s of the source in the matched matrix
            %            (i.e., column number).
            %          - A dictionary cell array of field names to search
            %            in the MatchedSources.Data (return the first
            %            exitsing field).
            % Output : - JD vector.
            %          - Mag (or selected field) vector/array for the selected
            %            sources.
            % Author : Eran Ofek (Jul 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix(rand(100,200),'FLUX')
            %          MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            %          [JD, Mag] = getLC_ind(MS, [2 3], {'FLUX'})
            
            arguments
                Obj(1,1) MatchedSources
                Ind
                FieldMag             = AstroCatalog.DefNamesMag;
            end
            
            [~, Name] = tools.cell.strNameDict2ind(Obj.Fields, FieldMag);
            JD  = Obj.JD;
            Mag = Obj.Data.(Name)(:,Ind);
            
        end
        
        % plot LC by source index
        
        % plot LC by source position
    end
    
    methods (Static) % unitTest
        Result = unitTest()
            % unitTest for MatchedSources class
                    
    end
end
