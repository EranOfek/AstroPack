% MatchedSources class - A container for matrices of matched sources
% Properties:
%   Data  - A structure, in which each field is a matrix of identical sizes
%           containing Nepoch X Nsrc measurments of some property.
%   SrcData - A structure, in which each field is a vector of properties
%           per source (e.g., mean mag).
%   JD     - A vector of times per epoch. If not set by user default is a
%            vector of 1..Nepoch.
%   Fields - A cell array of field names (Dependent)
%   Nsrc   - Number of sources in each matrix.
%   Nepoch - Number of epochs in each matrix.
%   DimEpoch - Dim of epoch (constant) = 1
%   DimSrc   - Dim of sources (constant) = 2
% Methods:
%   read (static) - Read a mat/hdf5 into MatchedSources
%   write1 - Write a MatchedSources into mat/hdf5 file.
%   addMatrix - Add matrix/struct/matched AstroTable into the MatchedSources Data.
%   getMatrix - Get matrix using field name.
%   summary   - Summary of a specific field matrix in MatchedSources.
%   plotRMS   - plot rms of mag vs. mag
%
% Examples:
%   % prepare list of files to upload
%   L = MatchedSources.rdirMatchedSourcesSearch('CropID',10);      
%   Upload all files to a MatchedSources object
%   MS = MatchedSources.readList(L);    
%   % Merge all MatchedSources elkement into a single element object
%   MSU=mergeByCoo(MS,MS(1));
%   % Add GAIA mag and colors
%   MSU.addExtMagColor;
%
%   % To see the rms vs mag prior to calibration
%   MSU.plotRMS
%
%   R = lcUtil.zp_meddiff(MSU,'MagField','MAG_PSF','MagErrField','MAGERR_PSF');
%   MSU.applyZP(R.FitZP);
%   MSU.plotRMS
%
%   MSG = MSU.copy;
%   [GoodObs, GoodStar] = MSU.selectGoodPhotCalibStars;
%   MSGs = MSG.selectBySrcIndex(GoodStar);
%
%   R=lsqRelPhot(MSG, 'Flag',GoodFlag);
%   %R=lsqRelPhot(MSG, 'Flag',GoodFlag, 'StarProp',{nanmedian(MSG.Data.X1)', nanmedian(MSG.Data.Y1)', MSG.SrcData.ExtColor(:)}, 'Method','cgs');
%   R=lsqRelPhot(MSG, 'Flag',GoodFlag, 'StarProp',{MSG.SrcData.ExtColor(:)}, 'Method','cgs');
%   MSG.applyZP(R.FitZP);
%   MSG.plotRMS
%
%
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
        SrcData(1,1) struct   % Data for sources (e.g., mean RA)
        FileName          % optional file name
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
            %   Each dataset in the hdf5 file will be read into a matrix
            %   with the same name in the Data property.
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
            %          MS.write1('try.hdf5')
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
                        Struct.JD = h5read(FileName, '/JD');
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
            % treat special fields
            if isfield(Struct, 'JD')
                Obj.JD = Struct.JD;
                Struct = rmfield(Struct, 'JD');
            end
            Obj.addMatrix(Struct);
            
            Obj.FileName = FileName;
            
        end
        
        function Result = read_rdir(FileTemplate, Args)
            % Read all MatchedSources files in a dir tree into a MatchedSources object.
            %   By default will read all Level=MergedMat files, recursivley
            %   from a tree of directories, into a MatchedSources object.
            %   If OrderPart argument is provided than will attempt to
            %   order the files into a matrix in which rows are for
            %   different epochs, and columns for different property like
            %   CropID index.
            % Input  : - A file template name to attempt reading.
            %            Default is '*_sci_merged_MergedMat_*.hdf5'.
            %          * ...,key,val,...
            %            'readArgs' - A cell array of additional arguments
            %                   to pass to the MatchedSources.read static function.
            %            'OrderPart' - If provided, will try to order the
            %                   objects by (Epoch,CropID).
            %                   Default is [].
            %                   For more info about CropID see ImagePath
            %                   class.
            % Output : - A MatchedSources object.
            % Author : Eran Ofek (Jul 2022)
            % Example: mm=MatchedSources.read_rdir;
            
            arguments
                FileTemplate         = '*_sci_merged_MergedMat_*.hdf5';   
                
                Args.readArgs cell   = {};
                Args.OrderPart       = []; %'CropID';   % [] - do not order
            end
            
            List = io.files.rdir(FileTemplate);
            List = fullfile({List.folder},{List.name}).';
            %FN   = FileNames(List);
            %FN.FormatVersion = '%03d';
            %List = FN.genFile;
            
            Nlist = numel(List);   
   
            % read all files regardless of order
            if Nlist==0
                Result = [];
            else
                for Ilist=1:1:Nlist
                    %File          = fullfile(List(Ilist).folder, List(Ilist).name);
                    Result(Ilist) = MatchedSources.read(List{Ilist}, Args.readArgs{:});
                end

                if ~isempty(Args.OrderPart)
                    IP     = ImagePath.parseFileName({Result.FileName});
                    Part   = str2double({IP.(Args.OrderPart)});
                    Npart  = max(Part);
                    Nepoch = Nlist./Npart;
                    
                    ResultO = MatchedSources;
                    for Ipart=1:1:Npart
                        if any([Part==Ipart])
                            ResultO(1:Nepoch,Ipart) = Result([Part==Ipart].');
                            VecMeanJD = zeros(Nepoch,1);
                            for Iep=1:1:Nepoch
                                VecMeanJD(Iep) = mean(ResultO(Iep,Ipart).JD);
                            end
                            [~,SI] = sort(VecMeanJD);
                            ResultO(:,Ipart) = ResultO(SI,Ipart);
                        end
                    end
                    
                    Result = ResultO;
                    
                end
            end
            
            
            
            
%     Nsub = 24;
%     State = false(Nlist./Nsub, Nsub);
%     for Ilist=1:1:Nlist
%         FileName = fullfile(List(Ilist).folder, List(Ilist).name);
%         FileMergedMat = strrep(FileName, '_sci_merged_Cat_001.fits', '_sci_merged_MergedMat_001.hdf5');
%         IP=ImagePath.parseFileName(FileMergedMat);
%         CropID = str2double(IP.CropID);        
%         
%         FC = sum(State(:,CropID));
%         Counter = sum(State(:,CropID)) + 1;
%         
%         State(Counter, CropID) = true;
%         
%         
%         MS(Counter, CropID) = MatchedSources.read(FileMergedMat);
%         
            
        end
        
        function Result = rdirMatchedSourcesSearch(Args)
            % Recursive search for MergedMat files and return file names.
            %   The output is a structure array in which each element
            %   contains the file name per CropID.
            % Input  : * ...,key,val,...
            %            'FileTemplate' - File name template to search
            %                   Default is '*merged_MergedMat_1.hdf5'.
            %            'Path' - Path in which to start the recursive
            %                   search. Default is pwd.
            %            'MinJD' - Min JD. Default is -Inf.
            %            'MaxJD' - Max JD. Default is Inf.
            %            'CropID' - List of CropID files.
            %                   If empty, then will retrieve all.
            %            'Product' - Requested Product.
            %                   Default is 'MergedMat'.
            % Output : - A structure array with field
            %            .FullName containing
            %               a cell array of full file names (including path).
            %               The structure is an array in which each element
            %               corresponds to a different CropID.
            %            .Folder - A cell array of folders.
            %            .CropID - The CropID index is stored in the .CropID field.
            % Author : Eran Ofek (Nov 2023)
            % Example: Result = MatchedSources.rdirMatchedSourcesSearch()
            %          Result = MatchedSources.rdirMatchedSourcesSearch('CropID',[])


            arguments
                Args.FileTemplate       = '*merged_MergedMat_1.hdf5';
                Args.Path               = pwd;
                Args.MinJD              = -Inf;
                Args.MaxJD              = Inf;
                Args.CropID             = [];  % can be a vector, empty - will do for each CropID
                Args.Product            = 'MergedMat';
            end

            PWD = pwd;
            cd(Args.Path);
            List = io.files.rdir(Args.FileTemplate);
            FN   = FileNames.generateFromFileName({List.name},'FullPathFromFileName',true);
            JD   = FN.julday;
            % search by user specified criteria
            Flag = JD(:)>Args.MinJD & JD(:)<Args.MaxJD & strcmp(FN.Product(:), Args.Product);
            if isempty(Args.CropID)
                Args.CropID = sort(unique(FN.CropID));
            end
            Ncrop = numel(Args.CropID);
            Result = struct('FileName',cell(Ncrop,1), 'Folder',cell(Ncrop,1), 'CropID',cell(Ncrop,1));
            for Icrop=1:1:Ncrop
                FlagC = Flag & FN.CropID(:)==Args.CropID(Icrop);

                FNC   = FN.reorderEntries(FlagC, 'CreateNewObj',true);

                Result(Icrop).FileName = FNC.genFile;
                Result(Icrop).Folder   = {List(FlagC).folder}.';
                Result(Icrop).CropID   = Args.CropID(Icrop);
            end
            
            cd(PWD);
        end
    
        function Result = readList(List, Args)
            % Read a list of MatchedSources hdf5 files into a MatchedSources object.
            % Input  : - A structure with two fields:
            %            .FullName - conatining a cell array of files.
            %            .Folder - a cell array of folders.
            %            This structure is the output of MatchedSources.rdirMatchedSourcesSearch
            %          * ...,key,val,..
            %            'FieldFullName' - The input structure field
            %                   containing the file names. Default is
            %                   'FileName'.
            %            'FieldFolder' - The input structure field contains
            %                   the cell of folders. Default is 'Folder'.
            % Output : - A MatchedSources object populated with the files.
            % Author : Eran Ofek (Nov 2023)
            % Example: L = MatchedSources.rdirMatchedSourcesSearch('CropID',[10]);
            %          MS = MatchedSources.readList(L);

            arguments
                List
                Args.FieldFileName   = 'FileName';
                Args.FieldFolder     = 'Folder';
            end

            if iscell(List)
                ListSt(1).(Args.FieldFullName) = List;
            else
                ListSt = List;
            end

            Nst = numel(ListSt);
            for Ist=1:1:Nst
                Nfile = numel(ListSt(Ist).(Args.FieldFileName));
                File = fullfile(ListSt(Ist).(Args.FieldFolder), ListSt(Ist).(Args.FieldFileName));
                for Ifile=1:1:Nfile
                    Result(Ifile) = MatchedSources.read(File{Ifile});
                end
            end

        end
    end
    
    methods % write
        function Result = write1(Obj, FileName, Args)
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
            %            'RealIfComplex' - A logical indicating if to take
            %                   the real value (of a complex value).
            %                   This is used only if FileType=hdf5.
            %                   Default is true.
            % Output : - Return true if sucess.
            % Author : Eran Ofek (Jun 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix({rand(100,200),rand(100,200)},{'FLUX','MAG'})
            %          MS.write1('try.hdf5')
            
            arguments
                Obj(1,1)
                FileName
                Args.FileType             = 'hdf5';
                Args.RealIfComplex logical = true;
            end
           
            switch lower(Args.FileType)
                case {'h5','hdf5','hd5'}
                    Ndata = numel(Obj.Fields);
                    for Idata=1:1:Ndata
                        h5create(FileName, sprintf('/%s',Obj.Fields{Idata}), size(Obj.Data.(Obj.Fields{Idata})));
                        if Args.RealIfComplex
                            h5write(FileName, sprintf('/%s',Obj.Fields{Idata}), real(Obj.Data.(Obj.Fields{Idata})));
                        else
                            h5write(FileName, sprintf('/%s',Obj.Fields{Idata}), Obj.Data.(Obj.Fields{Idata}));
                        end
                    end
                    % save also the JD
                    h5create(FileName, '/JD', size(Obj.JD));
                    h5write(FileName,  '/JD', Obj.JD);
                case {'mat'}
                    % save the Data structure
                    Tmp = Obj.Data;
                    save(FileName, 'Tmp', '-v7.3');
                case {'matobj'}
                    % save the MatchedSources as object
                    save(FileName, 'Obj', '-v7.3');
                otherwise
                    error('Unknown FileType option');
            end
           
            Result = true;
        end
    end
    
    methods (Static)  % design matrix
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
                    if isempty(Units)
                        Obj.Units.(FN{Ifn}) = '';
                    else
                        Obj.Units.(FN{Ifn}) = Units{Ifn};
                    end
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
                    if isfield(Units, FieldName{Ifn})
                        Obj.Units.(FieldName{Ifn}) = Units.(FieldName{Ifn});
                    end
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
            %                   Default is {'RA','Dec','X1','Y1','SN_1','SN_2','SN_3','SN_4','MAG_CONV_2','MAGERR_CONV_2','MAG_CONV_3','MAGERR_CONV_3','FLAGS'};
            %            'CreateNewObj' - A logical indicating if to
            %                   generate a new copy of the MatchedSources object.
            %                   Default is false.
            % Output : - A MatchedSources object.
            %          - The matched AstroCatalog.
            % Author : Eran Ofek (Nov 2021)
            % Example: see usage in pipeline.generic.mergeCatalogs
            
            arguments
                Obj
                AT                                         % AstroCatalog | AstroImage
                Args.JD                              = []; % of empty put 1:N
                Args.CooType                         = 'sphere';
                Args.Radius                          = 3;
                Args.RadiusUnits                     = 'arcsec';
                Args.unifiedSourcesCatalogArgs cell  = {};
                Args.MatchedColums cell              = {'RA','Dec','X1','Y1','SN_1','SN_2','SN_3','SN_4','MAG_CONV_2','MAGERR_CONV_2','MAG_CONV_3','MAGERR_CONV_3','FLAGS'};
                
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
            % Return the units for a mtrix name.
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
                Obj.Data = rmfield(Obj.Data, FieldName{Ifn});
                Obj.Units = rmfield(Obj.Units, FieldName{Ifn});
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
        
        function [FieldNames] = getFieldNameSearch(Obj, SubString)
            % Search for MatchedSources field names that contains a substring.
            % Input  : - A single element MatchedSources Object.
            %          - A sub string to search.
            % Output : - A cell array of all field names that contains the
            %            substring.
            % Author : Eran Ofek (Dec 2021)
            % Example: Obj = MatchedSources;
            %          Obj.addMatrix(rand(30,40),'RA');
            %          Obj.addMatrix(rand(30,40),'Dec');
            %          [FieldName] = getFieldNameSearch(Obj, 'De')
            
            arguments
                Obj(1,1)
                SubString
            end
            
            Flag = contains(Obj.Fields, SubString);
            FieldNames = Obj.Fields(Flag);
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
        
        function Obj = addSrcData(Obj, Field, Data, Args)
            % Populate/calculate the SrcData structure 
            %   The SrcData property contains a structure in which each
            %   field may contain a vector (element per source).
            %   This can be used to store some mean properties (e.g., mean
            %   magnitude).
            % Input  : - An MatchedSources object.
            %            If Data is empty, this may be a multi-element
            %            object.
            %          - A field name in the SrcData to populate, or a cell
            %            array of fields.
            %            Cell array if fields is valid only if Data is
            %            empty.
            %            If empty, use all fields in the Data property.
            %            Default is empty.
            %          - Data. If empty, then will look for the field name
            %            in the Data property, abd calculate the mean over
            %            all columns of the data property.
            %            If input is a matrix, then apply the MeanFun over
            %            columns. If a vector then pipulate the
            %            SrcData.(Field) with this vector.
            %          * ...,key,val,...
            %            'MeanFun' - Mean function to apply over columns.
            %                   Default is @tools.math.stat.nanmedian
            %            'MeanFunArgs' - A cell array of additional
            %                   arguments to pass to 'MeanFun' after the
            %                   Dim argument. Default is {}.
            % Output : - The MatchedSources object with the added SrcData.
            % Author : Eran Ofek (Dec 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix(rand(100,200),'FLUX')
            %          MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            %          MS = MatchedSources;
            %          St.Flux=rand(100,200); MS.addMatrix(St);
            %          MS.addSrcData('FLUX')
            
            arguments
                Obj
                Field                           = [];      % char or cell
                Data                            = [];
                Args.MeanFun function_handle    = @tools.math.stat.nanmedian;
                Args.MeanFunArgs cell           = {};
            end
            
            Nobj = numel(Obj);
            
            if iscell(Field) && ~isempty(Data)
                error('Field may be a cell array only if Data is empty');
            end
            
            if isempty(Field)
                % use all firlds
                Field = Obj.Fields;
            end
            
            % convert Field to cell array
            if ischar(Field)
                Field = {Field};
            end
            Nfield = numel(Field);
            
            if isempty(Data)
                % get data from some mean of existing property
                for Iobj=1:1:Nobj
                    for Ifield=1:1:Nfield
                        Data = getMatrix(Obj(Iobj), Field{Ifield});
                        Obj(Iobj).SrcData.(Field{Ifield}) = Args.MeanFun(Data, 1, Args.MeanFunArgs{:});
                    end
                end
            else
                if numel(Obj)>1
                    error('When Data is not empty, the MatchedSources elenment must has a single element');
                end
                if size(Data,1)==1 
                    % already single epoch
                    Obj.SrcData.(Field{1}) = Data;
                elseif size(Data,2)==1
                    Obj.SrcData.(Field{1}) = Data.';
                else
                    Obj.SrcData.(Field{1}) = Args.MeanFun(Data, 1, Args.MeanFunArgs{:});
                end
            end
        end
        
        function Result = applySortInd(Obj, Ind, Dim, ApplySrcData, Args)
            % Reorder all sources accoring to vector of indices (e.g., sort)
            % Input  : - A single element MatchedSources object.
            %          - Vector of indices by which to order the data matrices.
            %          - Dimension along to order the data. Default is 2.
            %          - A logical indicating if to apply the reorder also
            %            to the SrcData fields.
            %            This is operational only if Dim=2.
            %            Default is true.   
            %          * ...,key,val,...
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the object. Default is false.
            % Output : - The ordered MatchedSources object.
            % Author : Eran Ofek (Jul 2022)
           
            arguments
                Obj(1,1)
                Ind
                Dim                       = 2;
                ApplySrcData logical      = true;
                Args.CreateNewObj logical = false;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end

            Fields = fieldnames(Obj.Data);
            Nfield = numel(Fields);
            for Ifield=1:1:Nfield
                if Dim==1
                    Result.Data.(Fields{Ifield}) = Obj.Data.(Fields{Ifield})(Ind,:);
                else
                    Result.Data.(Fields{Ifield}) = Obj.Data.(Fields{Ifield})(:,Ind);
                    
                    % apply also to SrcData
                    if ApplySrcData && isfield(Obj.SrcData, Fields{Ifield})
                        Result.SrcData.(Fields{Ifield}) = Obj.SrcData.(Fields{Ifield})(Ind);
                    end
                end
            end
        end
        
        function Obj = sortData(Obj, FieldName, Args)
            % Sort (source wise) all the Data and SrcData fields by some of the Data fields.
            %   This function also add a field (default is 'Dec') to the
            %   SrcData property.
            %   For example, you can use this to sort all the Data and
            %   SrcData fields by the mean declination of the sources.
            %   The sort is applied for each element in the MatchedSources
            %   object.
            % Input  : - A MatchedSources object.
            %          - Field name by which to sort.
            %            Default is 'Dec'.
            %          * ...,key,val,...
            %            'MeanFun' - Mean function to apply over columns
            %                   for the sorted data field.
            %                   Default is @median
            %            'MeanFunArgs' - A cell array of additional
            %                   arguments to pass to 'MeanFun' after the
            %                   Dim argument. Default is {'omitnan'}.
           
            arguments
                Obj
                FieldName             = 'Dec';
                Args.MeanFun          = @median;
                Args.MeanFunArgs cell = {'omitnan'};
            end            
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if iscell(FieldName)
                    [Field]  = getFieldNameDic(Obj(Iobj), FieldName);
                else
                    Field    = FieldName;
                end
                
                Obj(Iobj) = addSrcData(Obj(Iobj), Field, [], 'MeanFun',Args.MeanFun, 'MeanFunArgs',Args.MeanFunArgs);
                 
                [~,SortedInd] = sort(Obj(Iobj).SrcData.(Field));
            
                Obj(Iobj) = applySortInd(Obj(Iobj), SortedInd, 2, true);
            end
        end
        
        function Result = mergeByCoo(Obj, ObjRef, Args)
            % Merge MatchedSources objects by coordinates.
            %   Given an array of MatchedSources object and a reference
            %   MatchedSource object, search by coordinates, all the
            %   sources in the MatchedSource that are matched to a source
            %   in the reference. Create a new MatchedSources object with
            %   all the epochs (element of the MatchedSources array) for
            %   each source in the reference.
            % Input  : - A MatchedSources object.
            %          - A single element MatchedSources object with the
            %            refrence catalog. The number of sources in the
            %            output equal to the number of sources in this
            %            object.
            %          * ...,key,val,...
            %            'CooUnits' - The units of the RA/Dec coordinates
            %                   in all the MatchedSources objects.
            %                   Default is 'deg'.
            %            'SearchRadius' - Matching search radius.
            %                   Default is 3.
            %            'SearchRadiusUnits' - Searchradius units.
            %                   Default is 'arcsec'.
            %            'FieldRA' - Field name in the Data property that
            %                   contains the J2000.0 R.A.
            %                   Alternatively, a cell array dictionary of
            %                   names from which the first exiting name
            %                   will be selected.
            %                   Default is AstroCatalog.DefNamesRA.
            %            'FieldDec' - Like 'FieldRA' but for the J2000.0
            %                   Dec. Default is AstroCatalog.DefNamesDec.
            %            'MeanFun' - Mean function to apply over columns
            %                   for the sorted data field.
            %                   Default is @median
            %            'MeanFunArgs' - A cell array of additional
            %                   arguments to pass to 'MeanFun' after the
            %                   Dim argument. Default is {'omitnan'}.
            % Output : - A MatchedSources object with a single element.
            %            This object contains the matched sources in all
            %            the MatchedSources object.
            %            The number of sources in the
            %            output equal to the number of sources in this
            %            object.
            % Author : Eran Ofek (Jul 2022)
            
            
            arguments
                Obj
                ObjRef(1,1) MatchedSources
                Args.CooUnits      = 'deg';
                Args.SearchRadius  = 3;
                Args.SearchRadiusUnits = 'arcsec';
                
                Args.FieldRA       = AstroCatalog.DefNamesRA;
                Args.FieldDec      = AstroCatalog.DefNamesDec;
                Args.MeanFun       = @median;
                Args.MeanFunArgs   = {'omitnan'};
            end
            
            UnitsConv = convert.angular(Args.CooUnits, 'rad',1);
            
            SearchRadiusRad = convert.angular(Args.SearchRadiusUnits, 'rad', Args.SearchRadius);  % [rad]
            
            
            Nobj = numel(Obj);
            
            [FieldRA]  = getFieldNameDic(ObjRef, Args.FieldRA);
            [FieldDec] = getFieldNameDic(ObjRef, Args.FieldDec);
            
            % sort all by Declination + add SrcData
            ObjRef = sortData(ObjRef, 'Dec', 'MeanFun',Args.MeanFun, 'MeanFunArgs',Args.MeanFunArgs);
            Obj    = sortData(Obj, 'Dec', 'MeanFun',Args.MeanFun, 'MeanFunArgs',Args.MeanFunArgs);
            
            % populate the SrcData - estimate mean RA/Dec
            ObjRef = addSrcData(ObjRef, {FieldRA, FieldDec}, [], 'MeanFun',Args.MeanFun, 'MeanFunArgs',Args.MeanFunArgs);
            Obj    = addSrcData(Obj, {FieldRA, FieldDec}, [], 'MeanFun',Args.MeanFun, 'MeanFunArgs',Args.MeanFunArgs);
            
            Fields = fieldnames(ObjRef.Data);
            Nfield = numel(Fields);
                  
            Nsrc    = ObjRef.Nsrc;
            
            
            % reference image            
            RA  = ObjRef.SrcData.(FieldRA)  .* UnitsConv;
            Dec = ObjRef.SrcData.(FieldDec) .* UnitsConv;
            
            Result = MatchedSources;
            for Iobj=1:1:Nobj
                % for each MatchedSources element
                % match sources by coordinates
                
                % use negative SearchRad in order to return Dist and Ind1
                Ind = VO.search.search_sortedlat_multi([Obj(Iobj).SrcData.(FieldRA)(:), Obj(Iobj).SrcData.(FieldDec)(:)].*UnitsConv,...
                                                       RA, Dec, -SearchRadiusRad);
                % select from Obj(Iobj).Data objects with index: Ind.Ind
                % and put them in new matrix...
                FlagI    = [Ind.Ind1];
                FlagRef  = (1:1:Nsrc).';

                Nepoch   = Obj(Iobj).Nepoch;
                IndF     = ~isnan(FlagI);
                FlagI    = FlagI(IndF);
                
                Result.JD = [Result.JD(:); Obj(Iobj).JD(:)];
                for Ifield=1:1:Nfield
                    
                    % initialzie Result with NaNs
                    Mat   = nan(Nepoch, Nsrc);
                    % remove NaNs from FlagI

                    FlagRefI = FlagRef(IndF);
                    % Result = NaN; Result(Ind,:) = Obj(FlagInd, :) insert NaN to Result when appear in FlagInd
                    Mat(:,FlagRefI) = Obj(Iobj).Data.(Fields{Ifield})(:,FlagI);
                    if Iobj==1
                        Result.Data.(Fields{Ifield}) = Mat;
                    else
                        Result.Data.(Fields{Ifield}) = [Result.Data.(Fields{Ifield}); Mat];
                    end
                    
                end                
            
            end
            
        end
        
        function Result = selectBySrcIndex(Obj, Ind, Args)
            % Selected sources by index in MatchedSources object
            % Input  : - A MatchedSources object.
            %          - A vector of indices or logical flags corresponding
            %            to the sources to select.
            %          * ...,key,val,...
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new object. Default is true.
            % Output : - A MatchedSources object with the seclected
            %            sources.
            % Author : Eran Ofek (Jan 2023)
            % Example: Result = selectBySrcIndex(Obj, [1 2 3]);
            
            arguments
                Obj
                Ind
                Args.CreateNewObj logical   = true;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            FieldsD = fieldnames(Obj(1).Data);
            NfD     = numel(FieldsD);
            FieldsS = fieldnames(Obj(1).SrcData);
            NfS     = numel(FieldsS);
           
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                for If=1:1:NfD
                    Result(Iobj).Data.(FieldsD{If}) = Obj(Iobj).Data.(FieldsD{If})(:,Ind);
                end
                for If=1:1:NfS
                    Result(Iobj).SrcData.(FieldsS{If}) = Obj(Iobj).SrcData.(FieldsS{If})(Ind);
                end
            end
                
        end
        
        function Result = selectByEpoch(Obj, EpochSelect, Args)
            % Selected entries in MatchedSources object by epoch index or ranges
            % Input  : - A MatchedSources object.
            %          - A vector of indices or logical flags corresponding
            %            to the sources to select.
            %            Alterantively, if this is a two column matrix,
            %            then will be treated as ranges [min max] JD of
            %            JD to select.
            %          * ...,key,val,...
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new object. Default is true.
            % Output : - A MatchedSources object with the seclected
            %            epoch.
            % Author : Eran Ofek (Jan 2023)
            % Example: Result = selectByEpoch(Obj, [1 2 3]');
            
            arguments
                Obj
                EpochSelect                  % two columns for range
                Args.CreateNewObj logical   = true;
            end
            
             if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            FieldsD = fieldnames(Obj(1).Data);
            NfD     = numel(FieldsD);
           
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if size(EpochSelect,2)==2
                    Ind = tools.array.find_ranges_flag(Obj(Iobj).JD, EpochSelect);
                else
                    Ind = EpochSelect;
                end
                
                Result(Iobj).JD = Obj(Iobj).JD(Ind);
                for If=1:1:NfD
                    Result(Iobj).Data.(FieldsD{If}) = Obj(Iobj).Data.(FieldsD{If})(Ind,:);
                end
              
            end
                
            
        end
       
        function Result = juldayFun(Obj, Fun)
            % Apply function to JD column pf each MatchedSources element.
            % Input  : - A MatchedSources object.
            %          - One of the following scalar returning functions:
            %            'mid' | 'median' | 'mean' | 'range' | 'std'
            %            Default is 'mid'.
            % Output : - A vector of results, one per MatchedSources
            %            element.
            % Author : Eran Ofek (Apr 2023)

            arguments
                Obj
                Fun    = 'mid';
            end

            Nobj = numel(Obj);
            Result = nan(size(Obj));

            for Iobj=1:1:Nobj
                if isempty(Obj(Iobj).JD)
                    Result(Iobj) = NaN;
                else
                    switch Fun
                        case 'mid'
                            Result(Iobj) = (min(Obj(Iobj).JD) + max(Obj(Iobj).JD)).*0.5;
                        case 'median'
                            Result(Iobj) = median(Obj(Iobj).JD);
                        case 'mean'
                            Result(Iobj) = mean(Obj(Iobj).JD);
                        case 'range'
                            Result(Iobj) = range(Obj(Iobj).JD);
                        case 'std'
                            Result(Iobj) = std(Obj(Iobj).JD);
                        otherwise
                            error('Unknown Fun option');
                    end
                end
            end
        end
    
    end

    methods % add aux. Data matrices
        function Obj=addAirMassPA(Obj, Args)
            % Add Airmass, Parallactic angle, Az, Alt to MatchedSources
            %   Based on RA, Dec, JD in MatchedSources objects add fields.
            % Input  : - A MatchedSources object.
            %          * ...,key,val,...
            %            'GeoCoo' - A mandatory Geodetic position for which
            %                   to calculate AM, PA, Az, Alt.
            %                   [Lon(deg), Lat(deg)].
            %            'FieldRA' - Col field of RA data in MatchedSources
            %                   object. Default is MatchedSources.DefNamesRA
            %            'FieldDec' - Col field of Dec data in MatchedSources
            %                   object. Default is MatchedSources.DefNamesDec
            %            'InUnits' - [RA,Dec] Coordinates input units.
            %                   Default is 'deg'.
            %            'OutUnits' - [Az, ALt, PA] coordinates output
            %                   units. Default is 'deg'.
            %            'AddAM' - Add AM. Default is true.
            %            'AddPA' - Add PA. Default is true.
            %            'AddAz' - Add Az. Default is false.
            %            'AddAlt' - Add Alt. Default is false.
            %            'FieldAM' - Added AM field name.
            %                   Default is 'AM'.
            %            'FieldPA' - Added PA field name.
            %                   Default is 'PA'.
            %            'FieldAz' - Added Az field name.
            %                   Default is 'Az'.
            %            'FieldAlt' - Added Alt field name.
            %                   Default is 'Alt'.
            % Output : - An updated MatchedSources object with the added
            %            data.
            % Author : Eran Ofek (May 2023)
            % Example: MS = MS.addAirMassPA('GeoCoo',[35.041,30.053]);

            arguments
                Obj
                Args.GeoCoo       = [];   % [deg deg km]
                Args.FieldRA      = MatchedSources.DefNamesRA;
                Args.FieldDec     = MatchedSources.DefNamesDec;
                Args.InUnits      = 'deg';
                Args.OutUnits     = 'deg';

                Args.AddAM logical   = true;
                Args.AddPA logical   = true;
                Args.AddAz logical   = false;
                Args.AddAlt logical  = false;
                
                Args.FieldAM         = 'AM';
                Args.FieldPA         = 'PA';
                Args.FieldAz         = 'Az';
                Args.FieldAlt        = 'Alt';

            end

            RAD = 180./pi;

            if isempty(Args.GeoCoo)
                error('Geodetic position must be provided');
            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                [FieldRA]  = getFieldNameDic(Obj, Args.FieldRA);
                [FieldDec] = getFieldNameDic(Obj, Args.FieldDec);

                [Az, Alt, AM, PA] = celestial.coo.radec2azalt(Obj(Iobj).JD, Obj(Iobj).Data.(FieldRA), Obj(Iobj).Data.(FieldDec), 'GeoCoo',Args.GeoCoo, 'InUnits',Args.InUnits, 'OutUnits',Args.OutUnits);
            
                % add fields to Data:
                if Args.AddAM
                    Obj(Iobj) = addMatrix(Obj(Iobj), AM, Args.FieldAM, '');
                end
                if Args.AddPA
                    Obj(Iobj) = addMatrix(Obj(Iobj), PA, Args.FieldPA, '');
                end
                if Args.AddAz
                    Obj(Iobj) = addMatrix(Obj(Iobj), Az, Args.FieldAz, '');
                end
                if Args.AddAlt
                    Obj(Iobj) = addMatrix(Obj(Iobj), Alt, Args.FieldAlt, '');
                end


            end

        end
    
        function Obj=addExtMagColor(Obj, Args)
            % Add magnitude/color from external catalog into a MatchedSources object
            %   FFU: Non-efficient code.
            % Input  : - A MatchedSources object.
            %          * ...,key,val,...
            %            See code
            % Output : - The MatchedSources object with the external
            %            catalog magnitude and color added.
            % Author : Eran Ofek (May 2023)

            arguments
                Obj
                Args.Catalog       = 'GAIADR3';
                Args.ColMag        = 'phot_bp_mean_mag';
                Args.ColColor      = {'phot_bp_mean_mag','phot_rp_mean_mag'};  % single column or two columns
                Args.ColCoo        = {'RA','Dec'};
                Args.SearchRadius  = 2;
                Args.SearchUnits   = 'arcsec';

                Args.FieldRA      = MatchedSources.DefNamesRA;
                Args.FieldDec     = MatchedSources.DefNamesDec;
                Args.CooUnits     = 'deg';

                Args.Insert2Data logical = false;
                Args.FieldMag            = 'ExtMag';
                Args.FieldColor          = 'ExtColor';
                Args.FieldExtRA          = 'ExtRA';
                Args.FieldExtDec         = 'ExtDec';
                Args.ApplyPM logical     = true;
            end

            RAD = 180./pi;

            ConvertFactor = convert.angular(Args.CooUnits, 'rad');

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                [FieldRA]  = getFieldNameDic(Obj, Args.FieldRA);
                [FieldDec] = getFieldNameDic(Obj, Args.FieldDec);

                RA  = median(Obj(Iobj).Data.(FieldRA), 1, 'omitnan');
                Dec = median(Obj(Iobj).Data.(FieldDec), 1, 'omitnan');

                Nra = numel(RA);
                MagVector   = nan(1, Nra);
                ColorVector = nan(1, Nra);
                for Ira=1:1:Nra
                    if ~isnan(RA(Ira)) && ~isnan(Dec(Ira))
                        [Cat] = catsHTM.cone_search(Args.Catalog, RA(Ira).*ConvertFactor, Dec(Ira).*ConvertFactor, Args.SearchRadius,...
                                        'RadiusUnits',Args.SearchUnits, 'OutType','astrocatalog');

                        EpochOut = convert.time(mean(Obj(Iobj).JD), 'JD','J');

                        if Cat.sizeCatalog>0 && Args.ApplyPM
                            Cat = imProc.cat.applyProperMotion(Cat, Cat.Catalog(1,3), EpochOut,'EpochInUnits','J','EpochOutUnits','J','ApplyPlx',true);
                        end

                        Mag   = Cat.getCol(Args.ColMag);
                        Color = Cat.getCol(Args.ColColor);
                        Coo   = Cat.getCol(Args.ColCoo);

                        switch numel(Mag)
                            case 0
                                Mag   = NaN;
                                Color = NaN;
                                Coo   = [NaN NaN];
                            case 1
                                % do nothing

                            otherwise
                                Mag   = NaN;
                                Color = NaN;
                                Coo   = [NaN NaN];
                        end
                        
                        if numel(Color)==2
                            Color = Color(1) - Color(2);
                        end

                        MagVector(Ira)   = Mag;
                        ColorVector(Ira) = Color;
                        RAVector(Ira)    = Coo(1);
                        DecVector(Ira)   = Coo(2);

                    end
                end

                if Args.Insert2Data
                    Obj(Iobj).Data.(Args.FieldMag)   = repmat(MagVector, Obj(Iobj).Nepoch, 1);
                    Obj(Iobj).Data.(Args.FieldColor) = repmat(ColorVector, Obj(Iobj).Nepoch, 1);
                end
                % insert to SrcData
                Obj(Iobj).SrcData.(Args.FieldMag)    = MagVector;
                Obj(Iobj).SrcData.(Args.FieldColor)  = ColorVector;

                Obj(Iobj).SrcData.(Args.FieldExtRA)  = RAVector.*RAD;
                Obj(Iobj).SrcData.(Args.FieldExtDec) = DecVector.*RAD;

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
            % Return a vector of logicals indicating epochs which do not have any NaNs in their data.
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
                Args.UseMex logical              = true;
            end

            [FlagsName] = getFieldNameDic(Obj(1), Args.FlagsNameDic);
            
            N = numel(Obj);
            for I=1:1:N
                % get Mag matrix
                
                DataFlags       = getMatrix(Obj(I), FlagsName);
                Result(I).FLAGS = tools.array.bitor_array(Args.FlagsType(DataFlags), 1, Args.UseMex);
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
        
        function Result = binning(Obj, Ngroup, Args)
            % Bin MatchedSources Data, JD, SrcData entries.
            %   Note that the function assumes that the data is sorted by
            %   time.
            %   Note that error entries will be binned using the same
            %   MeanFun as all the other entries.
            % Input  : - A MatchedSources object.
            %          - Number of sucessive entries to bin.
            %            E.g., if 2, pairs of sucessive lines will be
            %            binned. Default is 2.
            %          * ...,key,val,...
            %            'TimeMeanFun' - A function handle to use in order
            %                   to bin the JD entries. Default is @mean.
            %            'MeanFun' - A function handle to use in order
            %                   to bin the Data and SrcData entries.
            %                   Default is @mean.
            %            'MeanFunArgs' - A cell array of additional
            %                   arguments to pass to the binning function,
            %                   after the data and dim arguments.
            %                   Default is {'omitnan'}.
            %            'CreateNewObj' - A logical indicating if the
            %                   output is a new MatchedSources object.
            %                   Default is true.
            % Output : - A matchedSources object with the binned data.
            % Author : Eran Ofek (Jul 2022)
            % Example: Result = binning(Obj, 2);
            
            arguments
                Obj
                Ngroup                             = 2;
                
                Args.TimeMeanFun function_handle   = @mean;
                Args.MeanFun function_handle       = @mean;
                Args.MeanFunArgs cell              = {'omitnan'};
                
                Args.CreateNewObj logical          = true;
            end
           
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                % for each MatchedSources element
                Nepoch = Obj(Iobj).Nepoch;
                IndF = (1:Ngroup:Nepoch); % index of first in each bin
                Nbin = numel(IndF);
                IndF = [IndF, Nepoch];
                
                % JD property
                JD   = nan(Nbin,1);
                for Ibin=1:1:Nbin
                    Ind = (IndF(Ibin):1:IndF(Ibin+1)).';
                    JD(Ibin) = Args.TimeMeanFun(Obj(Iobj).JD(Ind), 1);
                end
                Result(Iobj).JD = JD;
                
                % Data property
                Fields = fieldnames(Obj(Iobj).Data);
                Nf     = numel(Fields);
                Mat = nan(Nbin, Obj(Iobj).Nsrc);
                for If=1:1:Nf
                    for Ibin=1:1:Nbin
                        Ind = (IndF(Ibin):1:IndF(Ibin+1)).';
                        Mat(Ibin,:) = Args.MeanFun(Obj(Iobj).Data.(Fields{If})(Ind,:), 1, Args.MeanFunArgs{:});
                    end
                    Result(Iobj).Data.(Fields{If}) = Mat;
                end
                
                % SrcData property
                %Fields       = fieldnames(Result(Iobj).SrcData);
                Result(Iobj) = addSrcData(Result(Iobj), [], [], 'MeanFun',Args.MeanFun, 'MeanFunArgs',Args.MeanFunArgs);
                
            end
            
        end
    end
    
    methods % combine/merge
        
        function [Result] = searchFlags(MS, Args)
            % Search specific flags in MatchedSources matrix.
            %   Return a matrix of logical indicating if the specific flags
            %   are present in each entry.
            % Input  : - A single element MatchedSources object.
            %          * ...,key,val,...
            %            'BitDic' - A BitDictionary object to use.
            %                   Default is BitDictionary.
            %            'PropFlags' - The name of the flags matrix in the Data
            %                   property. Default is 'FLAGS'.
            %            'FlagsList' - A cell array containing a list of
            %                   bit names to identify.
            %                   Default is {'NearEdge','Saturated','NaN','Negative'}
            %            'Operator' - If multiple bit names are requested
            %                   then this is the operator to apply between
            %                   the bit names. Options are @or | @and.
            %                   Default is @or.
            % Output : - An array of logicals of size Nepoch X Nsrc.
            %            Each element in the array indicate if the
            %            requested bit names where found in this entry.
            % Author : Yhuda Stern (Sep 2023)
            % Example: FlagId = searchFlags(MS);

            arguments
               MS(1,1) MatchedSources
               Args.BitDic       = BitDictionary;
               Args.PropFlags    = 'FLAGS';
               Args.FlagsList    = {'NearEdge','Saturated','NaN','Negative'};
               Args.Operator     = @or; % @or | @and
            end

            BitClass = Args.BitDic.Class;
            Flags = BitClass(MS.Data.(Args.PropFlags));

            Result = zeros(size(Flags));
            Nflag  = numel(Args.FlagsList);
            for Iflag = 1:1:Nflag
                FieldIndex = find(strcmp(Args.BitDic.Dic.BitName, Args.FlagsList{Iflag}));
                if ~isempty(FieldIndex)
                    Result = Args.Operator(Result, bitget(Flags,FieldIndex));
                else
                    error('Field "%s" not found in dictionary', Args.Flags{Iflag});
                end
            end
        end
   
        function Result = countFlags(MS, Args)
            % Count specific flags per source or per epoch.
            % Input  : - A single element MatchedSources object.
            %          * ...,key,val,...
            %            'Dim' - Dimension over which to count the flags.
            %                   Default is 1 (return count per source).
            %            'BitDic' - A BitDictionary object to use.
            %                   Default is BitDictionary.
            %            'PropFlags' - The name of the flags matrix in the Data
            %                   property. Default is 'FLAGS'.
            %            'FlagsList' - A cell array containing a list of
            %                   bit names to identify.
            %                   Default is {'NearEdge','Saturated','NaN','Negative'}
            %            'Operator' - If multiple bit names are requested
            %                   then this is the operator to apply between
            %                   the bit names. Options are @or | @and.
            %                   Default is @or.
            %            'NotFlag' - Apply @not operator before counting
            %                   the flags. Default is false.
            % Output : - A vector containing the numbre of entries (along
            %            the requested dimension) with the specific
            %            requested flags.
            % Author : Eran Ofek (Oct 2023)
            % Example: Res = countFlags(MS);

            arguments
               MS(1,1) MatchedSources
               Args.Dim             = 1;
               Args.BitDic          = BitDictionary;
               Args.PropFlags       = 'FLAGS';
               Args.FlagsList       = {'NearEdge','Saturated','NaN','Negative'};
               Args.Operator        = @or; % @or | @and
               Args.NotFlag logical = false;
            end
            
            FlagMatrix = searchFlags(MS, 'BitDic',Args.BitDic, 'PropFlags',Args.PropFlags, 'FlagsList',Args.FlagsList, 'Opertor',Args.Operator);
            if Args.NotFlag
                FlagMatrix = ~FlagMatrix;
            end
            Result = sum(FlagMatrix, Args.Dim);            
             
        end

        
        
    end
    
    methods % conversions
        function Result=convert2AstroCatalog(Obj, Args)
            % Convert MatchedSources object into an AstroCatalog
            %   The AstroCatalog will include a JD column followed by the
            %   fields in the MatchedSources object.
            % Input  : - A MatchedSources object.
            %          * ...,key,val,...
            %            'SameEpochBlock' - A logical indicating if to
            %                   order the sources by epoch blocks (true),
            %                   or by source blocks (false).
            %                   Default is true.
            % Output : - An AstroCatalog object with the data sotored in
            %            the MatchedSources Data property.
            % Author : Eran Ofek (May 2023)
            % Example: MS = MatchedSources;
            %          MS.addMatrix([1 2 3; 4 5 6],'FLUX')
            %          MS.addMatrix({rand(2,3), rand(2,3), rand(2,3)},{'MAG','X','Y'})
            %          T = MS.convert2AstroCatalog;
            %          T = MS.convert2AstroCatalog('SameEpochBlock',false);
            
            arguments
                Obj(1,1)
                Args.SameEpochBlock logical    = true;
            end
            
            if isempty(Obj.Nepoch) || isempty(Obj.Nsrc)
                % empty MatchedSources
                Result = [];
            else
                Nfields = numel(Obj.Fields);
                Array   = zeros(Obj.Nsrc.*Obj.Nepoch, Nfields+1);
                JD      = Obj.JD;
                if isempty(JD)
                    JD = (1:1:Obj.Nepoch).';
                end
                JD = JD(:);
                for If=1:1:Nfields
                    if Args.SameEpochBlock
                        % write blocks of the same epoch followed by next
                        % epoch
                        if If==1
                            Vector = repmat(JD, [1 Obj.Nsrc]).';
                            Vector = Vector(:);
                        else
                            Vector = Obj.Data.(Obj.Fields{If-1}).';
                            Vector = Vector(:);
                        end
                    else
                        % write block of same sources in differnt epochs,
                        % followed by next source
                        if If==1
                            Vector = repmat(JD, [Obj.Nsrc, 1]);
                        else
                            Vector = Obj.Data.(Obj.Fields{If-1})(:);
                        end
                    end
                    
                    Array(:,If) = Vector;
                end
                Result = AstroCatalog({Array}, 'ColNames',[{'JD'}, Obj.Fields(:)']);
            end
            
        end
    end
    

    methods % detrending and photometry
        function [Obj,ApplyToMagField] = applyZP(Obj, ZP, Args)
            % Apply zero point and optional color term to MatchedSources matrices.
            %   This function can be used to apply a vector of ZP to
            %   instrumental magnitude matrices.
            %   Only simple ZP can be treated (i.e., no higher terms,
            %   detrending).
            % Input  : - A MatchedSources object.
            %          - A vector of ZP (ZP per epoch), or structure array
            %            (elelmnt per MatchedSources element)
            %            with a ZP vector in each element.
            %          * ...,key,val,...
            %            'FieldZP' - If the second input is a structure
            %                   array, then this is the field name
            %                   containing the ZP vector.
            %                   Default is 'FitZP'.
            %            'ApplyToMagField' - A char or cell array.
            %                   If char, then will first search for all
            %                   field names in the first element of the
            %                   MatchedSources object that contains this
            %                   substring. All the releveant GAIfields will be
            %                   put in a cell array. Will apply the ZP for
            %                   all fields in the cell array.
            %                   Default is 'MAG'.
            %            'Operator' - A function handke with operator for
            %                   the ZP. E.g., NewMatrix = operator(Matrix, ZP)
            %                   Default is @minus.
            %            'Color' - A vector of colors (one per source).
            %                   [No support for structure array].
            %                   Default is [].
            %            'ColorTerm' - Color terms that multiplys the color
            %                   vector. Default is [].
            %            'ColorOperator' - A function handle for the color
            %                   operator. Default is @minus.
            % Output : - The MatchedSources object, but with the magnitude
            %            fields applied with the ZP.
            %          - A cell array of field names to which the ZP was
            %            applied.
            % Author : Eran Ofek (Dec 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix(rand(100,200),'FLUX')
            %          MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            %          MS.applyZP(ones(100,1))
            
            arguments
                Obj(1,1)
                ZP
                Args.FieldZP          = 'FitZP';
                Args.ApplyToMagField  = 'MAG_'; % if cell then exact search - for all Matched sources
                Args.Operator         = @minus;
                Args.Color            = [];    %will work only for single element MatchedSources
                Args.ColorTerm        = [];
                Args.ColorOperator    = @minus;
                
            end
            
            if ischar(Args.ApplyToMagField)
                % Args.ApplyToMagCol is a substring to search
                ApplyToMagField = getFieldNameSearch(Obj(1), Args.ApplyToMagField);
            else
                ApplyToMagField = Args.ApplyToMagField;
            end
            Nfield = numel(ApplyToMagField);
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if isstruct(ZP)
                    VecZP = ZP(Iobj).(Args.FieldZP);
                else
                    VecZP = ZP;
                end
             
                VecZP = VecZP(:); % must be a column vector - line per epoch
                for Ifield=1:1:Nfield
                    Obj(Iobj).Data.(ApplyToMagField{Ifield}) = Args.Operator(Obj(Iobj).Data.(ApplyToMagField{Ifield}), VecZP);
                    if ~isempty(Args.Color)
                        ColorVec = Args.Color(:).' .* Args.ColorTerm;  % a must be a row vector [per source]
                        Obj(Iobj).Data.(ApplyToMagField{Ifield}) = Args.ColorOperator(Obj(Iobj).Data.(ApplyToMagField{Ifield}), ColorVec);
                    end
                end
             
            end
        end

        function [GoodObs, GoodStar, Good] = selectGoodPhotCalibStars(Obj, Args)
            % Selects stars which are good for photometric calibration
            %   Given a single element MatchedSources object select stars
            %   which have good FLGAS, not NaN mag, good S/N, errors and chi^2.
            % Input  : - A single-element MatchedSources object.
            %          * ...,key,val,...
            %            'PropFlags' - The name of the flags matrix in the Data
            %                   property. Default is 'FLAGS'.
            %            'BitDic' - A BitDictionary object to use.
            %                   Default is BitDictionary.
            %            'BadFlags' - A cell array containing a list of
            %                   bit names to identify as bad.
            %                   Default is {'CR_DeltaHT','NearEdge','Saturated','NaN','Negative'}
            %            'PropSN' - S/N property name. Default is 'SN_2'.
            %            'MinSN' - Minimum S/N for good sources.
            %                   Default is 10.
            %            'MaxSN' - Maximum S/N for good sources.
            %                   Default is 1000.
            %            'PropMag' - Magnitude property name.
            %                   Default is 'MAG_PSF'.
            %            'MinMag' - Minimum mag. Default is -Inf.
            %            'MaxMag' - Maximum mag. Default is Inf.
            %            'PropMagErr' - Mag. error property name.
            %                   Default is 'MAGERR_PSF'.
            %            'MaxErr' - Maximum mag. error. Default is 0.05.
            %            'PropPsfChi2' - Chi2 property name.
            %                   Default is 'PSF_CHI2DOF'.
            %            'Chi2Quantile' - [Lower, Upper] range of quantile
            %                   in which to select sources by their chi^2.
            %                   Default is [0.05 0.95].
            %            'PropExtColor' - Property name in the SrcData
            %                   property, that contains the external color.
            %                   Note that this can be populated using the
            %                   MatchedSources/addExtMagColor method.
            %                   Default is 'ExtColor'.
            %            'ExtColorExist' - If true, then will only choose
            %                   stars for which the SrcData.ExtColor is not NaN.
            %                   (Only if the SrcData.ExtColor is
            %                   populated).
            %                   Default is true.
            % Output : - A matrix of size Nepoch X Nsrc with logicals
            %            indicating if the observation is good for phot. calib.
            %          - A vector of logical indicating if a star has only
            %            true (good) entries.
            %          - A structure with all the specific problem
            %            logicals.
            % Author : Eran Ofek (Nov 2023)
            % Example: GoodFlag = MSU.selectGoodPhotCalibStars;

            arguments
                Obj(1,1)

                Args.PropFlags        = 'FLAGS';
                Args.BitDic           = BitDictionary;
                Args.BadFlags cell    = {'CR_DeltaHT','NearEdge','Saturated','NaN','Negative'};
                Args.PropSN           = 'SN_2';
                Args.MinSN            = 10;
                Args.MaxSN            = 1000;
                Args.PropMag          = 'MAG_PSF'
                Args.MinMag           = -Inf;
                Args.MaxMag           = Inf;
                Args.PropMagErr       = 'MAGERR_PSF'
                Args.MaxErr           = 0.05;
                Args.PropPsfChi2      = 'PSF_CHI2DOF';
                Args.Chi2Quantile     = [0.05 0.95];  % will remove lower and upper 0.05
                Args.PropExtColor     = 'ExtColor';
                Args.ExtColorExist logical = true;
            end

            % sources with good flags
            Good.Flags = ~searchFlags(Obj, 'BitDic',Args.BitDic,...
                                       'PropFlags',Args.PropFlags,...
                                       'FlagsList',Args.BadFlags,...
                                       'Operator',@or);
            % sources with good S/N
            Good.SN    = Obj.Data.(Args.PropSN)>Args.MinSN & Obj.Data.(Args.PropSN)<Args.MaxSN;

            % sources with good errors
            Good.Err   = Obj.Data.(Args.PropMagErr)<Args.MaxErr;

            % sources with good mag and not NaN
            Good.Mag   = Obj.Data.(Args.PropMag)>Args.MinMag & Obj.Data.(Args.PropMag)<Args.MaxMag & ~isnan(Obj.Data.(Args.PropMag));

            % sources with good chi2
            RqngeQuantile = quantile(Obj.Data.(Args.PropPsfChi2), Args.Chi2Quantile, 'all');
            Good.Chi2     = Obj.Data.(Args.PropPsfChi2)>RqngeQuantile(1) & Obj.Data.(Args.PropPsfChi2)<RqngeQuantile(2);

            if Args.ExtColorExist
                if isfield(Obj.SrcData, Args.PropExtColor)
                    Good.Color = ~isnan(Obj.SrcData.(Args.PropExtColor));
                else
                    Good.Color = true;
                end
            else
                Good.Color = true;
            end

            % combine all flags
            GoodObs      = Good.Flags & Good.SN & Good.Err & Good.Mag & Good.Chi2 & Good.Color;
              
            % good stars
            GoodStar = all(GoodObs, 1);

        end

        function [Result]=lsqRelPhot(Obj, Args)
            % Perform relative photometry calibration using the linear least square method.
            %   This function solves the following linear problem:
            %   m_ij = z_i + M_j + alpha*C + ... (see Ofek et al. 2011).
            %   By default will perfporm two iterations, where in the second
            %   iteration, the errors will be taken from the magnitude vs. std
            %   diagram, and stars with bad measurments will be removed.
            % Input  : - A matrix of instrumental magnitude, in which the epochs
            %            are in the rows, and stars are in the columns.
            %            If empty, then run a simulation.
            %          * ...,key,val,...
            %            'MagErr' - A scalar, or a matrix of errors in the
            %                   instrumental magnitudes.
            %            'Flag' - A vector of flags for all stars, indicating if to
            %                   use them as reference star (true) or not (false).
            %                   The vector size must be Nepoch X Nsrc and it
            %                   contains the sources epoch by epoch (i.e., given a
            %                   Data matrix Nepoch N Nsources, it is Data(:)).
            %                   If empty, assume all true.
            %                   Default is [].
            %            'Method' - LSQ solver method: 'lscov'.
            %                   Default is 'lscov'.
            %            'Algo' - ALgorithm for the lscov function: 'chol'|'orth'.
            %                   Default is 'chol'.
            %            'Niter' - Number of iterations for error estimation and
            %                   bad source removal. Default is 2.
            %            'MaxStarStd' - In the second iteration, remove stars with
            %                   std larger than this value. Default is 0.1.
            %            'UseMagStdErr' - If true, then in the second iteration
            %                   will replace the MagErr with the errors (per star)
            %                   estimated from the mag vs. std plot.
            %                   Default is true.
            %            'CalibMag' - A vector of calibrated magnitude for all the
            %                   stars. You can use NaN for unknown/not used
            %                   magnitudes. If empty, then do not calibrate.
            %                   Default is [].
            %            'Sparse' - Use sparse matrices. Default is true.
            %            
            %            'ZP_PrefixName' - In the column names cell of the design matrix, this is the
            %                   prefix of the images zero point.
            %            'MeanMag_PrefixName' - In the column names cell of the design matrix, this is the
            %                   prefix of the stars mean magnitudes.
            %            'StarProp' - A cell array of vectors. Each vector
            %                   must be of the length equal to the number of stars.
            %                   Each vector in the cell array will generate a new
            %                   column in the design matrix with a property per
            %                   star (e.g., color of each star).
            %                   Default is {}.
            %            'StarPropNames' - A cell array of names for the StarProp
            %                   column names. If this is a string than will be the
            %                   string prefix, with added index. Default is 'SP'.
            %            'ImageProp' - Like StarProp but for the images.
            %                   E.g., airmass.
            %                   Default is {}.
            %            'ImagePropNames' - Like StarPropNames, but for the images.
            %                   Default is 'IP'.
            %            'ThresholdSigma' - Threshold in sigmas (std) for flagging good
            %                   data (used by imUtil.calib.resid_vs_mag).
            %                   Default is 3.
            %            'resid_vs_magArgs' - A cell array of arguments to pass to 
            %                   imUtil.calib.resid_vs_mag
            %                   Default is {}.
            % Output : - A structure array (element per MatchedSources element)
            %            with the following fields:
            %            .Par   - All fitted parameters.
            %            .ParErr - Error in all fitted parameters.
            %            .FitZP - Fitted ZP parameters
            %            .FitMag - Fitted mean mag parameters.
            %            .Resid - All residuals.
            %            .Flag - Logical flags indicating which stars where used in
            %                   the solution.
            %            .NusedMeas - Number of used measurments.
            %            .StdResid - Std of used residuals.
            %            .RStdResid - Robust std of used residuals..
            %            .Stdstar - Std of each star measurments used in the
            %                   solution over all epochs.
            %            .StdImage - Std of each image measurments used in the
            %                   solution over all stars.
            %            .AssymStd - Assymptoic rms in the mag vs. std plot,
            %                   estimated from the minimum of the plot.
            %                   (Return NaN if Niter=1).
            %            .MagAssymStd - Magnitude of the assymptotic rms.
            %                   (Return NaN if Niter=1).
            %            .ColNames - Column names of the solution.
            % Author : Eran Ofek (Jun 2023)
            % Example: R=lsqRelPhot(MS);
            
            arguments
                Obj 
                Args.MagProp               = 'MAG_PSF';
                Args.MagErrProp            = 'MAGERR_PSF';
                
                Args.MagErr                = 0.02;
                Args.Flag                  = [];
                Args.Method                = 'lscov';
                Args.Algo                  = 'chol';  % 'chol' | 'orth'
                Args.Niter                 = 2;
                Args.MaxStarStd            = 0.1;
                Args.UseMagStdErr logical  = true;
                Args.CalibMag              = [];

                Args.Sparse logical        = true;
                Args.ZP_PrefixName         = 'Z';
                Args.MeanMag_PrefixName    = 'M';
                Args.StarProp              = {};  % one vector of properties per star - e.g., color
                Args.StarPropNames         = 'SP';
                Args.ImageProp             = {};  % one vector of properties per image - e.g., airmass
                Args.ImagePropNames        = 'IP';

                Args.ThresholdSigma        = 3;
                Args.resid_vs_magArgs cell = {};
            end


            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                InstMag    = Obj(Iobj).Data.(Args.MagProp);
                InstMagErr = Obj(Iobj).Data.(Args.MagErrProp);
                
                Result(Iobj)  = imUtil.calib.lsqRelPhot(InstMag,...
                                                      'MagErr',InstMagErr,...
                                                      'Method',Args.Method,...
                                                      'Algo',Args.Algo,...
                                                      'Niter',Args.Niter,...
                                                      'MaxStarStd',Args.MaxStarStd,...
                                                      'UseMagStdErr',Args.UseMagStdErr,...
                                                      'CalibMag',Args.CalibMag,...
                                                      'Sparse',Args.Sparse,...
                                                      'ZP_PrefixName',Args.ZP_PrefixName,...
                                                      'MeanMag_PrefixName',Args.MeanMag_PrefixName,...
                                                      'StarProp',Args.StarProp,...
                                                      'StarPropNames',Args.StarPropNames,...
                                                      'ImageProp',Args.ImageProp,...
                                                      'ImagePropNames',Args.ImagePropNames,...
                                                      'ThresholdSigma',Args.ThresholdSigma,...
                                                      'resid_vs_magArgs',Args.resid_vs_magArgs);
                % TBD / apply ZP?
            end
        end
        
        function Result=sysrem(Obj, Args)
            % Apply sysrem (Tamuz et al.) to magnitude matrices in MatchedSources object.
            % Input  : - A MatchedSources object.
            %          * ...,key,val,...
            %            'MagFields' - A cell array of magnitude fields on
            %                   which to apply sysrem.
            %                   Default is {'MAG_APER_3','MAG_PSF'}.
            %            'MagErrFields' - A cell array of magnitude error fields on
            %                   which to apply sysrem. The fields should
            %                   corresponds to those in MagFields.
            %                   Default is {'MAGERR_APER_3','MAGERR_APER_3'}
            %            'addSrcDataArgs' - A cell array of additional
            %                   arguments to pass to MatchedSources/addSrcData
            %                   Default is {}.
            %            'sysremArgs' - A cell array of additional arguments
            %                   to pass to timeSeries.detrend.sysrem
            %                   Default is {}.
            %            'CreateNewObj' - A logical indicating if to create
            %                   a new copy of the input object.
            %                   Default is false.
            % Output : - A MatchedSources object in which the SrcData is
            %            repopulated, and the magnitude fields were replaced
            %            with the sysrem detrended values.
            % Author : Eran Ofek (Jul 2023)
            % Example: MS.sysrem

            arguments
                Obj
                Args.MagFields              = {'MAG_PSF','MAG_APER_3'};
                Args.MagErrFields           = {'MAGERR_PSF','MAGERR_APER_3'};
                Args.addSrcDataArgs cell    = {};
                Args.sysremArgs cell        = {};
                Args.CreateNewObj logical   = false;
            end

            % add the SrcData property
            Obj.addSrcData(Args.addSrcDataArgs{:});

            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end

            Nmag = numel(Args.MagFields);

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                % for each element of the MatchedSources object
                for Imag=1:1:Nmag
                    % create Resid matrix
                    ResidMat = Obj(Iobj).Data.(Args.MagFields{Imag}) - Obj(Iobj).SrcData.(Args.MagFields{Imag});

                    [SysRemChi2,SysRemRes] = timeSeries.detrend.sysrem(ResidMat,...
                                                                       Obj(Iobj).Data.(Args.MagErrFields{Imag}),...
                                                                       Args.sysremArgs{:});
                   
                    Result(Iobj).Data.(Args.MagFields{Imag}) = Result(Iobj).SrcData.(Args.MagFields{Imag}) + SysRemRes(end).Resid;
                end

            end

        end

    end

    methods % light curves analysis
        function [PSD, Freq] = psd(Obj, Args)
            % Estimate the mean power spectral density of all the sources.
            % Input  : -A single element MatchedSources object.
            %          * ...,key,val,...
            %            'FieldName' - Field name for which to estimate
            %                   PSD. Default is 'MAG'.
            %            'IsEvenlySpaced' - Default is true.
            %            'SelectedSrcFlag' A vector of logicals indicating
            %                   which sources to use. If empty, use all.
            %                   Default is [].
            %            'SrcInvVar' - A vector of source inverse variance.
            %                   If empty, ones. If char then this is the
            %                   field name for the mag error.
            %                   Default is [].
            %            'RejectAboveErr' - If 'SrcInvVar' is field name,
            %                   than sources which median mag error is
            %                   larger than this will not be used.
            %                   Default is 0.05.
            %            'SaturateBelowErr' - If 'SrcInvVar' is field name,
            %                   than sources which median mag error is
            %                   smaller than this will be set to this
            %                   value. Default is 0.02.
            % Output : - A vector of power spectral density.
            %          - A vector of frequencies.
            % Author : Eran Ofek (Dec 2021)
            % Example: MS = MatchedSources;                                                       
            %          MS.addMatrix(rand(100,300),'FLUX');                                        
            %          MS.addMatrix({rand(100,300), rand(100,300), rand(100,200)},{'MAG','X','Y'});
            %          [PSD, Freq] = psd(MS)
            
            arguments
                Obj(1,1)
                Args.FieldName                 = 'MAG';
                Args.IsEvenlySpaced logical    = true;
                Args.SelectedSrcFlag           = [];
                Args.SrcInvVar                 = [];     % if FieldName than MAGERR
                Args.RejectAboveErr            = 0.05;
                Args.SaturateBelowErr          = 0.02;
            end
            
            [FieldName] = getFieldNameDic(Obj, Args.FieldName);
            Matrix = getMatrix(Obj, FieldName);
            [Nep, NsrcSel] = size(Matrix);
            
            % selected sources from matrix
            if isempty(Args.SelectedSrcFlag)
                Args.SelectedSrcFlag = true(NsrcSel,1);
            else
                Args.SelectedSrcFlag = Args.SelectedSrcFlag(:);
            end
            
            if isempty(Args.SrcInvVar)
                Args.SrcInvVar = ones(NsrcSel,1);
            end
            Args.SrcInvVar = Args.SrcInvVar(:).';
            
            if ischar(Args.SrcInvVar)
                % Args.SrcInvVar is a FieldName
                MagErr    = getMatrix(Obj, Args.SrcInvVar);
                MedMagErr = median(MagErr, 1, 'omitnan');
                Args.SelectedSrcFlag = Args.SelectedSrcFlag(:) & MedMagErr(:)<Args.RejectAboveErr;
            
                MedMagErr(MedMagErr<Args.SaturateBelowErr) = Args.SaturateBelowErr;
                
                Matrix = Matrix(:,Args.SelectedSrcFlag);
                MedMagErr = MedMagErr(Args.SelectedSrcFlag);
                Args.SrcInvVar = 1./(MedMagErr.^2);
            
            else
                Matrix = Matrix(:,Args.SelectedSrcFlag);
                Args.SrcInvVar = Args.SrcInvVar(Args.SelectedSrcFlag);
            end
            
            if Args.IsEvenlySpaced
                DT   = mean(diff(Obj.JD));
                Freq = ((1:1:Nep).'-1)./Nep;
                Freq = Freq./DT;
                
                PS   = abs(fft(Matrix, [], 1)).^2;

                % calculate the mean PS
                PSD = sum(PS.*Args.SrcInvVar, 2, 'omitnan') ./ sum(Args.SrcInvVar);
            else
                error('IsEvenlySpaced==false not supported yet');
            end
            
        end
        
        
        function [Result] = rmsMag(Obj, Args)
            % Calculate rms of some parameter as a function of magnitude.
            %       The mean magnitude over epochs, of all sources is
            %       calculated.
            %       The std of some parameter over epochs, of all sources is 
            %       calculated.
            %       The std as a function of mean magnitude is estimated
            %       using binning of polynomial fit.
            % Input  : - A MatchedSources object.
            %          * ...,key,val,...
            %            'MagField' - A field name containing the 'mag'
            %                   like matrix. Default is 'MAG'.
            %            'ParField' - A firld name containing the parameter
            %                   matrix. Default is 'MAG'.
            %            'Method' - Method:
            %                   'binning' - Use binning.
            %                   'polyfit' - use polyfit.
            %            'BinSize' - Bin Size. Default is 0.5.
            %            'InterpMethod' - Default is 'linear'.
            %            'PolyDeg' - Polynomial degree. Default is 3.
            %            'MeanFun' - Function handle for calculating the
            %                   mean. Default is @median.
            %            'MeanFunArgs' - A cell array of additional arguments to pass to
            %                   the 'MeanFun'. Default is {1,'omitnan'}.
            %            'StdFun' - Function handle for calculating the
            %                   std. Default is @std.
            %            'StdFunArgs' - A cell array of additional arguments to pass to
            %                   the 'StdFun'. Default is {[],1,'omitnan'}.
            %            'Nsigma' - For the 'binning' method. This indicate
            %                   the number of sigma above mean of the std in
            %                   which to flag a source as variable. 
            % Output : - A structure array (element per object element).
            %            .MeanMag - mean mag for all sources.
            %            .StdPar - par std for all sources.
            %            .InterpMeanStd - The interpolated binned/polyfitted
            %                   mean of the std for each source magnitude.
            %            .InterpStdStd - (avalable for 'binning' option)
            %                   The interpolated binned/polyfitted
            %                   rstd of the std for each source magnitude.
            %            .Flag - (avalable for 'binning' option) A logical
            %                   flag indicating if a source is a possible
            %                   variable.
            % Author : Eran Ofek (Jan 2022)
            % Example: MS = MatchedSources;
            %          MS.addMatrix(rand(100,200).*10,'MAG')
            %          [Result] = MS.rmsMag
            
            arguments
                Obj
                Args.MagField                  = 'MAG';
                Args.ParField                  = 'MAG';
                Args.Method                    = 'binning';
                Args.BinSize                   = 0.5;
                Args.InterpMethod              = 'linear';
                Args.PolyDeg                   = 3;
                Args.MeanFun function_handle   = @median;
                Args.MeanFunArgs cell          = {1, 'omitnan'}
                Args.StdFun function_handle    = @std;
                Args.StdFunArgs cell           = {[],1,'omitnan'};
                Args.Nsigma                    = 3;
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                [FieldNameMag] = getFieldNameDic(Obj(Iobj), Args.MagField);
                Mag = getMatrix(Obj(Iobj), FieldNameMag);
                [Nep, NsrcSel] = size(Mag);
                if strcmp(Args.MagField, Args.ParField)
                    Par = Mag;
                else
                    [FieldNamePar] = getFieldNameDic(Obj(Iobj), Args.MagField);
                    Par = getMatrix(Obj(Iobj), FieldNamePar);
                end

                Result(Iobj).MeanMag = Args.MeanFun(Mag, Args.MeanFunArgs{:});
                Result(Iobj).StdPar  = Args.StdFun(Par, Args.StdFunArgs{:});

                switch lower(Args.Method)
                    case 'binning'
                        B = timeSeries.bin.binning([Result(Iobj).MeanMag(:), Result(Iobj).StdPar(:)] ,Args.BinSize,[NaN NaN], {'MidBin',@numel, @median, @tools.math.stat.rstd});
                        Result(Iobj).B = B;
                        %Result(Iobj).EstimatedStdPar = interp1(B(:,1), B(:,3), Result(Iobj).MeanMag, Args.InterpMethod, 'extrap');
                        Result(Iobj).InterpMeanStd = interp1(B(:,1), B(:,3), Result(Iobj).MeanMag, Args.InterpMethod, 'extrap');
                        Result(Iobj).InterpStdStd = interp1(B(:,1), B(:,4), Result(Iobj).MeanMag, Args.InterpMethod, 'extrap');
                        Result(Iobj).FlagVar      = Result(Iobj).StdPar(:)> (Result(Iobj).InterpMeanStd(:) + Args.Nsigma.*Result(Iobj).InterpStdStd(:));
                    case 'polyfit'
                        Par = polyfit(Result(Iobj).MeanMag(:), Result(Iobj).StdPar(:), Args.PolyDeg);
                        Result(Iobj).InterpMeanStd = polyval(Par, Result(Iobj).MeanMag);
                    otherwise
                        error('Unknown Method option');
                end                
            end
            
        end
        
        
        function [FreqVec, PS] = period(Obj, Freq, Args)
            % Periodogram for all sources in MatchedSources object.
            % Input  : - A single element MatchedSources object.
            %          - Frequncies in which to calculate periodogram.
            %            One of the following.
            %            1. Empty, matrix. In this case will estimate the
            %               needed min:step:max frequncies.
            %            2. One element vector containing the max freq.
            %            3. Two element vector containing the [step, max]
            %               frequencies.
            %            4. A 3 element vector containing [min, step, max]
            %               frequencies.
            %            5. A vector of frequencies.
            %          * ...,key,val,...
            %            'MagField' - Field name containing the flux or
            %                   magnitude on which to calculate the periodogram.
            %                   Default is 'MAG_APER_3'.
            % Output : - A vector of frequencies.
            %          - A matrix of power spectra (freq. vs. star index).
            %          
            % Author : Eran Ofek (Jul 2023)
            % Example: [F, PS] = MS.period;

            arguments
                Obj(1,1)
                Freq      = [];
                Args.MagField     = 'MAG_APER_3';
            end

            T        = Obj.JD(:);

            MinFreq  = 0;
            MaxFreq  = [];
            StepFreq = [];
            FreqVec  = [];
            switch numel(Freq)
                case 0
                    % auto choose
                case 1
                    % assume input is max frequency
                    MaxFreq = Freq(1);
                case 2
                    StepFreq = Freq(1);
                    MaxFreq  = Freq(2);
                case 3
                    MinFreq  = Freq(1);
                    StepFreq = Freq(2);
                    MaxFreq  = Freq(3);
                otherwise
                    FreqVec = Freq;
            end

            if isempty(FreqVec)
                if isempty(MaxFreq)
                    MaxFreq  = 1./mean(diff(sort(T)));
                end
                if isempty(StepFreq)
                    StepFreq = 1./(2.*range(T));
                end

                FreqVec = (MinFreq:StepFreq:MaxFreq).';
            end

            [FreqVec,PS] = timeSeries.period.periodmulti_norm(T, Obj.Data.(Args.MagField), FreqVec);

        end

    end
    
    methods % find sources
 
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
            %            'FieldY' - Like 'FieldX', but for Y-axis (rms).
            %                   If empty, will use rms of FieldX.
            %                   Default is {}.
            %            'FactorRMS' - Factor by which to multiply the
            %                   Y-axis. E.g., for units conversion.
            %                   Default is 1.
            %            'PlotSymbol' - A cell array of parameters to pass
            %                   to the plot function.
            %                   Default is
            %                   {'k.','MarkerFaceColor','k','MarkerSize',3}.
            %            'PlotColor' - Symbol color. Default is 'k'.
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
            %            ** Additional parameters available for adding a
            %            noise curve.
            % Output : - Handle for data points plot.
            % Author : Eran Ofek (Jun 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix(rand(100,200),'MAG_PSF');
            %          MS.plotRMS
            %          MS.plotRMS('BinSize',0.1)
           
            arguments
                Obj(1,1)
                Args.FieldX                   = {'MAG','MAG_PSF','MAG_APER','MAG_APER_3','MAG_APER_2'};
                Args.FieldY                   = {};
                Args.FactorRMS                = 1;
                Args.PlotSymbol               = {'k.','MarkerFaceColor','k','MarkerSize',3};
                Args.PlotColor                = 'k';
                Args.BinSize                  = [];
                Args.DivideErrBySqrtN(1,1) logical = true;
                Args.BinMarker                = 'o';
                Args.BinColor                 = 'k';
                Args.BinMarkerSize            = 6;
                Args.XScale                   = 'linear';
                Args.YScale                   = 'log';
                Args.MeanFun function_handle  = @tools.math.stat.nanmedian;
                Args.StdFun function_handle   = @tools.math.stat.nanstd;
                Args.Xlabel                   = 'Mean Mag';
                Args.Ylabel                   = 'RMS';
                Args.FontSize                 = 16;
                Args.Interpreter              = 'latex';
                
                % add noise curve
                Args.PlotNoiseCurve logical   = false;
                Args.SNField                  = []; %'SN_3'; % if given skip other fields
                Args.AperArea                 = pi.*6.^2;
                Args.RN                       = 3.5;
                Args.Gain                     = 1;
                Args.FluxField                = 'FLUX_APER_3';
                Args.StdField                 = 'VAR_IM'; %'STD_ANNULUS';
                Args.IsStd logical            = false;
            end
        
            if ischar(Args.FieldX)
                Args.FieldX = {Args.FieldX};
            end
            
            
            if ischar(Args.PlotSymbol)
                Args.PlotSymbol = {Args.PlotSymbol};
            end
            FN = fieldnames(Obj.Data);
            % search for the first FieldX that appears in FN
            Ind = find(ismember(Args.FieldX, FN),1);
            FieldX = Args.FieldX{Ind};
            
            if isempty(Args.FieldY)
                FieldY = FieldX;
            else
                if ischar(Args.FieldY)
                    Args.FieldY = {Args.FieldY};
                end
                Ind = find(ismember(Args.FieldY, FN),1);
                FieldY = Args.FieldY{Ind};
            end
            
            Mat   = Obj.Data.(FieldX);
            % axis x - e.g., mean mag
            AxisX = Args.MeanFun(Mat, Obj.DimEpoch);
            
            if isempty(Args.FieldY)
                AxisY = Args.StdFun(Mat, [], Obj.DimEpoch);
            else
                MatY = Obj.Data.(FieldY);
                AxisY = Args.StdFun(MatY, [], Obj.DimEpoch);
            end
            AxisY = AxisY.*Args.FactorRMS;
            
            H = plot(AxisX, AxisY, Args.PlotSymbol{:});
            H.Color = Args.PlotColor;
            
            if ~isempty(Args.BinSize)
                % add bins to plot
                hold on;
                
                B = timeSeries.bin.binning([AxisX(:), AxisY(:)], Args.BinSize, [NaN NaN], {'MidBin', @mean, @std, @numel});
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
            
            % plot noise curve
            if Args.PlotNoiseCurve
                Obj.addSrcData;
                SN = Obj.SrcData.(Args.FluxField)./sqrt(Obj.SrcData.(Args.FluxField).*Args.Gain + Args.AperArea.*Obj.SrcData.VAR_IM + Args.AperArea.*Args.RN.^2);
                SN = SN(:);
                MagVec = Obj.SrcData.MAG_APER_3(:);
                FlagNN = ~isnan(SN);
                B = timeSeries.bin.binning([MagVec(FlagNN),1.086./SN(FlagNN)+0.0008],0.5,[10 18]);
                hold on;
                semilogy(B(:,1),B(:,3),'r-')
                
                
                
%                 if ~isempty(Args.SNField)
%                     [SNField] = getFieldNameDic(Obj, Args.SNField);
%                     SN        = median(Obj.Data.(SNField),1,'omitnan');
%                 else
%                     % S/N is not provided - calc from other fields
%                    [FluxField] = getFieldNameDic(Obj, Args.FluxField);
%                    [StdField]  = getFieldNameDic(Obj, Args.StdField);
%                 
%                    Flux   = median(Obj.Data.(FluxField),1,'omitnan');
%                    Std    = median(Obj.Data.(StdField),1,'omitnan');
%                    if Args.IsStd
%                        Var = Std.^2;
%                    else
%                        Var = Std;
%                    end
%                    SN = Flux./sqrt(Args.AperArea.*Var.*Args.Gain + Flux.*Args.Gain + Args.AperArea.*Args.RN.^2);
%                 end
%                 
%                 FlagNN = ~isnan(AxisX) & ~isnan(SN);
%                 VecX   = AxisX(FlagNN);
%                 SN     = SN(FlagNN);
%                 
%                 B=timeSeries.bin.binning([VecX(:), 1.086./SN(:)],0.5,[NaN NaN],{'MidBin',@tools.math.stat.nanmedian,@numel});
%                 FlagNN = ~isnan(B(:,2));
%                 B = B(FlagNN,:);
%                 hold on;
%                 plot(B(:,1), B(:,2),'k-')
                
            end
            Hgca = gca;
            Hgca.XScale = Args.XScale;
            Hgca.YScale = Args.YScale;
            
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
        
        % index from position
        function [Result] = coneSearch(Obj, RA, Dec, SearchRadius, Args)
            % search sources in MatchedSource object by RA/Dec
            % Input  : - A MatchedSources object.
            %          - R.A.
            %          - Dec.
            %          - Search radius. Default is 3.
            %          * ...,key,val,...
            %            'SearchRadiusUnits' - SearchRadius units.
            %                   Default is 'arcsec'.
            %            'InCooUnits' - Input RA/Dec units.
            %                   Default is 'deg'.
            %            'CooUnits' - Coordinates units in the Data matrix.
            %                   Default is 'deg'.
            %            'FieldRA' - Field name containing the R.A.
            %                   Default is 'RA'.
            %            'FieldDec' - Field name containing the Dec.
            %                   Default is 'Dec'.
            %            'MeanFun' - Mean function to apply over columns.
            %                   Default is @tools.math.stat.nanmedian
            %            'MeanFunArgs' - A cell array of additional
            %                   arguments to pass to 'MeanFun' after the
            %                   Dim argument. Default is {}.
            % Output : - A structure array (element per MatchedSource
            %            object element) with the following fields:
            %            .Ind - Indices of sources found withing search radius.
            %            .Flag - Flag of logicals of found sources.
            %            .Dist - Angular distance [rad] between found sources and
            %                   search position.
            %            .Nsrc - Number of sources found.
            % Author : Eran Ofek (Mar 2022)
            % Example: MS = MatchedSources;
            %          MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','RA','Dec'})
            %          [Ind,Flag,Dist] = coneSearch(MS, 0.5,0.5,100);
            
            arguments
                Obj
                RA
                Dec
                SearchRadius                 = 3;
                Args.SearchRadiusUnits       = 'arcsec';
                Args.InCooUnits              = 'deg';   % 'deg' | 'rad'
                Args.CooUnits                = 'deg';   % 'deg' | 'rad'
                Args.FieldRA                 = 'RA';
                Args.FieldDec                = 'Dec';
                Args.MeanFun function_handle = @tools.math.stat.nanmedian;
                Args.MeanFunArgs cell        = {};
            end
            
            Obj = addSrcData(Obj, Args.FieldRA,  [], 'MeanFun',Args.MeanFun, 'MeanFunArgs',Args.MeanFunArgs);
            Obj = addSrcData(Obj, Args.FieldDec, [], 'MeanFun',Args.MeanFun, 'MeanFunArgs',Args.MeanFunArgs);
            
            RA  = convert.angular(Args.InCooUnits, 'rad', RA);
            Dec = convert.angular(Args.InCooUnits, 'rad', Dec);
            
            SearchRadius = convert.angular(Args.SearchRadiusUnits, 'rad', SearchRadius);

            Nobj   = numel(Obj);
            Result = struct('Flag',cell(size(Obj)), 'Ind',cell(size(Obj)), 'Dist',cell(size(Obj)));
            for Iobj=1:1:Nobj
                MeanRA  = convert.angular(Args.CooUnits, 'rad', Obj(Iobj).SrcData.(Args.FieldRA));
                MeanDec = convert.angular(Args.CooUnits, 'rad', Obj(Iobj).SrcData.(Args.FieldDec));
                
                Dist = celestial.coo.sphere_dist_fast(RA, Dec, MeanRA, MeanDec);
                Result(Iobj).Flag = Dist<SearchRadius;
                Result(Iobj).Ind  = find(Result(Iobj).Flag);
                Result(Iobj).Dist = Dist(Result(Iobj).Flag);
                Result(Iobj).Nsrc = sum(Result(Iobj).Flag);
            end

        end
        
        % plot LC by source index
        
        % plot LC by source position

        % search moving source
        function Result = epochCooSearch(Obj, Pos, Args)
            % Search for epoch-dependent coordinates in MatchedSources object.
            %   Given a table with coordinates, interpolate this table into
            %   the MatchedSources epochs, and in each epoch search for the
            %   nearest object.
            % Input  : - A single element MatchedSources object.
            %          - Either an Astrocatalog object with JD,RA,Dec
            %            columns or a matrix of [JD, RA, Dec].
            %          * ...,key,val,...
            %            'MaxDist' - Max dist for source selection.
            %                   will select the nearest source within this
            %                   distance. If doesn't exist, then return NaN
            %                   entry. Default is 10.
            %            'MaxDistUnits' - MaxDist units. Default is 'arcsec'.
            %            'Interp' - A logical indicating if to interpolate
            %                   the 2nd input argument (Pos) onto the times
            %                   of the MatchedSources epochs.
            %                   Default is true.
            %            'InterpCooArgs' - A cell array of additional
            %                   arguments to pass to AstroCatalog/interpCoo.
            %                   Default is {}.
            %            'ColJD' - JD column name in the Pos table.
            %                   Default is 'JD'.
            %            'ColRA' - RA column name in the Pos table and also
            %                   the MatchedSources object. Default is 'RA'.
            %            'ColDec' - Like ColRA, but for Dec.
            %                   Default is 'Dec'.
            %            'PosUnits' - If Pos is in a matrix format than
            %                   this is the units in the Pos table.
            %                   Default is 'rad'.
            %            'DataUnits' - Units of RA/Dec in the
            %                   MatchedSources object. Will be used only if
            %                   not provided in the MatchedSources Units
            %                   property.
            %                   Default is 'rad'.
            % Output : - A structure with the following fields:
            %            .Ind - Index of source in each epoch.
            %                   NaN if not found.
            %            .Dist - Dist [rad] of moving coordinates to
            %                   nearest soiurce. NaN if not found.
            %            .Ncand - Number of candidates matched within
            %                   search radius.
            % Author : Eran Ofek (Sep 2023)
            % Example: [Cat]=celestial.SolarSys.jpl_horizons('ObjectInd','9804','StartJD',celestial.time.julday([14 6 2018]),'StopJD',  celestial.time.julday([20 6 2018]));
            %          MS = MatchedSources;
            %          MS.Data.RA = rand(100,1000);
            %          MS.Data.Dec = rand(100,1000);
            %          MS.Data.RA(1,1)  = Cat.Catalog(1,2);
            %          MS.Data.Dec(1,1) = Cat.Catalog(1,3);
            %          MS.JD = Cat.Catalog(1,1)+(0:1:99).';
            %          R = MS.epochCooSearch(Cat);

            arguments
                Obj(1,1)
                Pos
                Args.MaxDist             = 10;
                Args.MaxDistUnits        = 'arcsec';
                Args.Interp logical      = true;
                Args.InterpCooArgs cell  = {};
                Args.ColJD               = 'JD';
                Args.ColRA               = 'RA';
                Args.ColDec              = 'Dec';
                Args.PosUnits            = 'rad'; % only for matrix input
                Args.DataUnits           = 'rad'; % only if 'Units' fireld doesn't exist
            end

            if isempty(Obj.JD)
                error('movingSearch function requires populated JD property')
            end

            if ~isa(Pos, 'AstroTable') && ~isa(Pos, 'AstroCatalog')
                % assume Pos contains [JD, RA, Dec]
                Pos = AstroCatalog({Pos}, 'ColNames',{'JD','RA','Dec'}, 'ColUnits',Args.ColUnits);
            end
            
            MaxDistRad = convert.angular(Args.MaxDistUnits, 'rad', Args.MaxDist); % [radian]
            
            if Args.Interp
                InterpPos = Pos.interpCoo(Obj.JD, Args.InterpCooArgs{:});
            else
                InterpPos = Pos;
            end

            [RA, Dec] = getLonLat(InterpPos, 'rad');
            
            try
                if isempty(Obj.Units.(Args.ColRA))
                    Units = Args.DataUnits;
                else
                    Units = Obj.Units.(Args.ColRA);
                end
            catch
                Units = Args.DataUnits;
            end
            ConvFactor = convert.angular(Units, 'rad');
            
            % search object in each epoch:
            % Ind = VO.search.search_sortedlat_multi(Cat,0.5,0.5,0.01)
            Result.Ind   = nan(Obj.Nepoch,1);
            Result.Dist  = nan(Obj.Nepoch,1);
            Result.Ncand = zeros(Obj.Nepoch,1);
            for Iep=1:1:Obj.Nepoch
                Dist = celestial.coo.sphere_dist_fast(RA(Iep), Dec(Iep), Obj.Data.(Args.ColRA)(Iep,:).*ConvFactor, Obj.Data.(Args.ColDec)(Iep,:).*ConvFactor);
                [MinDist, MinDistInd] = min(Dist);
                if MinDist<MaxDistRad
                    Result.Ind(Iep)    = MinDistInd;
                    Result.Dist(Iep)   = MinDist;
                    Result.Ncand(Iep)  = sum(Dist<MaxDistRad);
                end
            end
            
        end
    end
    
    methods (Static) % unitTest
        Result = unitTest()
            % unitTest for MatchedSources class
                    
    end
end
