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
            %          - A field name.
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
                Obj.Data = Matrix;
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
    end
    
    methods (Static) % unitTest
        function Result = unitTest()
            % unitTest for MatchedSources class
            
            % write
            MS = MatchedSources;
            MS.addMatrix({rand(100,200),rand(100,200)},{'FLUX','MAG'})
            MS.write('try.hdf5')
            delete('try.hdf5');
            
            % read
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
            MS = MatchedSources;
            MS.addMatrix(rand(100,200),'FLUX')
            MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            MS = MatchedSources;
            
            St.Flux=rand(100,200);
            MS.addMatrix(St);
            
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
            MS=MatchedSources;                
            MS.addMatrix(rand(100,200),'FLUX');
            MS.summary
            MS.summary('FLUX')
            
            % plotRMS
            MS = MatchedSources;
            MS.addMatrix(rand(100,200),'MAG_PSF');
            MS.plotRMS
            MS.plotRMS('BinSize',0.1)
            
            Result = true;
        end
    end
end