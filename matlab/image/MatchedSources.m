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

classdef MatchedSources < Component
    properties
        Data(1,1) struct  % each field [Nepoch, Nsrc]
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
        function H=designMatrixCalib(Nep, Nsrc)
            %
            % Example: MatchedSources.designMatrixCalib(5,7)
            
            
            H = zeros(Nep.*Nsrc, Nsrc + Nep);
            DiagMat = diag(ones(Nep,1));
            OnesVec = ones(Nep,1);
            
            %LinesVec  = (1:Nsrc);        % column indices for the one-vec matrices
            LinesDiag = Nsrc + (1:Nep);  % column indices for the diag matrices
            for Isrc=1:1:Nsrc
                Rows = (1:Nep).' + (Isrc-1).*Nep;
                H(Rows, Isrc)      = OnesVec;
                H(Rows, LinesDiag) = DiagMat;
            end
            
            
        end
        
    end
    
    methods  % functions / get/set Data
        function Obj = addMatrix(Obj, Matrix, FieldName)
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
            end
           
                
            if isnumeric(Matrix)
                % matrix is numeric - add
                if ~ischar(FieldName)
                    error('For numeric matrix FieldName must be a char array');
                end
                Obj.Data.(FieldName) = Matrix;
            elseif isstruct(Matrix)
                % store the struct as is in Data
                FN = fieldnames(Matrix);
                for Ifn=1:1:numel(FN)
                    Obj.Data.(FN{Ifn}) = Matrix.(FN{Ifn});
                end
            elseif iscell(Matrix)
                Ncell = numel(Matrix);
                for Icell=1:1:Ncell
                    Obj.Data.(FieldName{Icell}) = Matrix{Icell};
                end
            elseif isa(Matrix, 'AstroTable')
                % Assume input is an array of matched AstroTables
                [Nrow, Ncol] = Matrix.sizeCatalog;
                if ~all(Nrow==Nrow(1))
                    error('For AstroTable/AstroCatalog input, all catalogs must have the same number of rows');
                end
                
                if ischar(FieldName)
                    FieldName = {FieldName};
                end
            
                [Res, Summary, N_Ep] = imProc.match.matched2matrix(Matrix, FieldName, true);
                Obj.addMatrix(Res);
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
            end
            
        end
        
        function Result = summary(Obj, Field)
            % Summary of a specific field matrix in MatchedSources
            % Input  : - A single element MatchedSources object.
            %          - A char array of field name. If empty, use the
            %            first field. Default is ''.
            % Output : - A structure of summary information
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
            %            'MeanFun' - Default is @nanmedian.
            %            'StdFun' - Default is @nanstd.
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
                Args.MeanFun function_handle  = @nanmedian
                Args.StdFun  function_handle  = @nanstd
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
        function Result = unitTest()
            % unitTest for MatchedSources class
            
            % write
            io.msgLog(LogLevel.Test, 'testing MatchedSources write');
            MS = MatchedSources;
            MS.addMatrix({rand(100,200),rand(100,200)},{'FLUX','MAG'})
            MS.write('try.hdf5')
            delete('try.hdf5');
            
            % read
            io.msgLog(LogLevel.Test, 'testing MatchedSources read');
            clear MS
            MS = MatchedSources;
            MS.addMatrix({rand(100,200),rand(100,200)},{'FLUX','MAG'})
            MS.write('try.hdf5')
            clear MS;
            % read all Fields
            MS = MatchedSources.read('try.hdf5');
            % read some fields
            MS1 = MatchedSources.read('try.hdf5','Fields','FLUX');
            delete('try.hdf5');

            % addMatrix
            io.msgLog(LogLevel.Test, 'testing MatchedSources addMatrix');
            MS = MatchedSources;
            MS.addMatrix(rand(100,200),'FLUX')
            MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            
            St.X2=rand(100,200);
            MS.addMatrix(St);
            
            % deleteMatrix
            io.msgLog(LogLevel.Test, 'testing MatchedSources deleteMatrix');
            MS.deleteMatrix('X2')
                        
            % match sources for addMatrix:
            AC = AstroCatalog;
            AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
            AC.ColNames = {'RA','Dec'};
            AC.ColUnits = {'rad','rad'};
            AC.getCooTypeAuto
            AC2 = AstroCatalog;
            AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0];
            AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
            AC2.getCooTypeAuto
            [MC,UM,TUM] = imProc.match.match(AC,AC2,'Radius',0.01,'RadiusUnits','rad');
            [MC,UM,TUM] = imProc.match.match([AC;AC2; AC; AC2],AC2,'Radius',0.01,'RadiusUnits','rad');
            % run addMatrix with AstroCatalog input
            MS = MatchedSources;
            MS.addMatrix(MC,{'RA','Dec'});
            
            % summary
            io.msgLog(LogLevel.Test, 'testing MatchedSources summary');
            MS=MatchedSources;                
            MS.addMatrix(rand(100,200),'FLUX');
            MS.summary
            MS.summary('FLUX')
            
            % design matrix
            io.msgLog(LogLevel.Test, 'testing MatchedSources designMatrix');
            clear MS
            MS = MatchedSources;
            MS.addMatrix(rand(100,200),'FLUX')
            MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            St.X2=rand(100,200);
            MS.addMatrix(St);
            [H, Y] = MS.designMatrix({'FLUX','X','Y'},{@sin, 1, 2},'MAG',1);
            [H, Y] = MS.designMatrix({[],'X','Y'},{[], 1, 2},'MAG',1);
            [H, Y, ErrY] = MS.designMatrix({[],'X','Y'},{[], 1, 2},'MAG',1, 'MAG',2);

            % notNanSources
            io.msgLog(LogLevel.Test, 'testing MatchedSources notNanSources');
            MS = MatchedSources;
            MS.addMatrix(rand(100,200),'FLUX')
            MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            St.X2=rand(100,200);
            MS.addMatrix(St);
            MS.Data.FLUX(1,1)=NaN;
            Flag = notNanSources(MS, 'FLUX');
            if Flag(1,1) || ~all(Flag(2:end))
                error('Problem with notNanSources');
            end
            Flag = notNanSources(MS, []); % use all fields
            io.msgLog(LogLevel.Test, 'testing MatchedSources notNanEpochs');
            MS = MatchedSources;
            MS.addMatrix(rand(100,200),'FLUX')
            MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            St.X2=rand(100,200);
            MS.addMatrix(St);
            MS.Data.FLUX(1,1)=NaN;
            Flag = notNanEpochs(MS, 'FLUX');
            Flag = notNanEpochs(MS, []); % use all fields

            % getMatrix
            io.msgLog(LogLevel.Test, 'testing MatchedSources getMatrix');
            MS = MatchedSources; 
            MS.Data.FLUX = rand(100,200);
            A = MS.getMatrix('FLUX');
            
            % plotRMS
            io.msgLog(LogLevel.Test, 'testing MatchedSources plotRMS');
            MS = MatchedSources;
            MS.addMatrix(rand(100,200),'MAG_PSF');
            MS.plotRMS
            MS.plotRMS('BinSize',0.1)
            
            MS = MatchedSources;
            MS.addMatrix(rand(100,200),'FLUX');
            MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'});
            [JD, Mag] = getLC_ind(MS, [2 3], {'FLUX'});
            
            
            io.msgStyle(LogLevel.Test, '@passed', 'MatchedSources test passed');
            Result = true;
        end
    end
end