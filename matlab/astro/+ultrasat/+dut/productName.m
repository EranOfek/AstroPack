function [File, FN] = productName(Cfg, Kind, Tile, DateObs, Args)
    % The file name of a DUT product (an image or its truth catalog), from the Naming of the configuration
    %   The one place where DUT products are named: the drivers, the
    %   SkipExisting checks and usim (through FN) all use it.
    %   Fields: ProjName, Filter, CropID, Type from Cfg.Naming; Time from
    %   DateObs ('yyyymmdd.HHMMSS.FFF'); CCDID = Tile;
    %     Kind 'ref': FieldID = Naming.FieldIDRef(Pointing), Counter = Image,
    %                 Level = Naming.LevelRef, in Cfg.Paths.Ref;
    %     Kind 'new': FieldID = Naming.FieldIDNew, Counter = 1,
    %                 Level = Naming.LevelNew, in Cfg.Paths.New;
    %   Product 'image': Naming.ProductImage with Naming.Version;
    %   Product 'truth': Naming.ProductTruth with Naming.VersionTruth.
    % Input  : - Cfg: the configuration (ultrasat.dut.config).
    %          - Kind: 'ref' or 'new'.
    %          - Tile: 'A', 'B', 'C' or 'D'.
    %          - DateObs: the exposure start, 'yyyy-mm-ddTHH:MM:SS'.
    %          * ...,key,val,...
    %            'Pointing' - the aimpoint number (Kind 'ref'). Default is 1.
    %            'Image' - the image number within the epoch (Kind 'ref').
    %                   Default is 1.
    %            'Product' - 'image' (default) or 'truth'.
    % Output : - File: the full file name.
    %          - FN: the AstroFileName object of the file (for the
    %               'FileName' argument of ultrasat.usim; its FileType is
    %               fits, the directory is not part of it).
    % Author : D. Kovaleva (Oct 2026)
    % Example: Cfg = ultrasat.dut.config;
    %          F = ultrasat.dut.productName(Cfg, 'ref', 'B', '2028-01-04T13:21:56', 'Pointing', 2);

    arguments
        Cfg (1,1) struct
        Kind (1,:) char {mustBeMember(Kind, {'ref', 'new'})}
        Tile (1,1) char
        DateObs (1,:) char
        Args.Pointing (1,1) double = 1
        Args.Image (1,1) double = 1
        Args.Product (1,:) char {mustBeMember(Args.Product, {'image', 'truth'})} = 'image'
    end

    Nm = Cfg.Naming;
    FN = AstroFileName;
    FN.ProjName = Nm.ProjName;
    FN.Time     = string(datestr(datenum(DateObs, 'yyyy-mm-ddTHH:MM:SS'), 'yyyymmdd.HHMMSS.FFF'));
    FN.Filter   = Nm.Filter;
    FN.CCDID    = string(Tile);
    FN.CropID   = Nm.CropID;
    FN.Type     = Nm.Type;
    FN.FileType = Nm.FileType;
    if strcmp(Kind, 'ref')
        FN.FieldID = Nm.FieldIDRef(Args.Pointing);
        FN.Counter = Args.Image;
        FN.Level   = Nm.LevelRef;
        Dir        = Cfg.Paths.Ref;
    else
        FN.FieldID = Nm.FieldIDNew;
        FN.Counter = 1;
        FN.Level   = Nm.LevelNew;
        Dir        = Cfg.Paths.New;
    end
    if strcmp(Args.Product, 'image')
        FN.Product = Nm.ProductImage;
        FN.Version = Nm.Version;
    else
        FN.Product = Nm.ProductTruth;
        FN.Version = Nm.VersionTruth;
    end
    File = fullfile(Dir, char(FN.genFile(1)));
end
