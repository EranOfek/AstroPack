function [Summary, AllSI, Coadd, MS] = visitQuality(AllSI, Coadd, MS, Args)
    % Extract visit quality directly from info in visit directory
    %   This function reads all the images and meta data from a visit
    %   directory and:
    %   1. Extract selected header keywords (see 'HeadKeys' argument) into
    %      a structe of arrays with values.
    %   2. Check that selected column names (see 'TestCol' argument) are in
    %      specified range (excluding NaN ptional) and report in a
    %      structure of arrays of logicals that columns are valid.
    %
    % --- TBD ---
    %   3. Some MS info
    %   4. Overlap info
    %   5. time series quality info
    %
    % Input  : - An AstroImage of all individual images.
    %            Alternatively, a path name in which the visit resides.
    %            Default is pwd.
    %          - An AstroImage containing all the coadd images.
    %            Default is [].
    %          - A MatchedSources containing all the MatchedSources object.
    %            Default is [].
    %          * ...,key,val,...
    %            'HeadKeys' - A cell array of header keywords to extract.
    %                   Default is {'LIMMAG'}.
    %            'ColTest' - A three column cell array containing:
    %                   Column name,
    %                   Allowed [Min Max],
    %                   NaN_Allowd (logical).
    %                   Default is {}.
    %            'Overlap' -- pass arguments to pipeline.last.quality.overlapSources
    %                   as a cell array of {'Arg1','Val1','Arg2','Val2',..}
    % Output : - A structure with the following fields:
    %            .AllSI_HV - A structue with header keyword values of all
    %                   selected header keywords in 'HeadKeys' for the
    %                   individual images.
    %            .AllSI_CR - A structure with range test for the selected
    %                   columns specified in 'TestCol' for the individual
    %                   images.
    %            .Coadd_HV - Same as AllSI_HV, but for the coadd images.
    %            .Coagg_CR - Same as AllSI_CR, but for the coadd images.
    %            .
    % Author : Eran Ofek (2026 Mar) 
    % Example: [Summary, AllSI, Coadd, MS] = pipeline.last.quality.visitQuality();

    arguments
        AllSI      = pwd;
        Coadd      = [];
        MS         = [];
        Args.HeadKeys          = {'LIMMAG'};
        Args.TestCol           = {'XPEAK',[1 1716], false; 'YPEAK',[1 1716], false; 'X1',[1 1716], true; 'Y1',[1 1716],true; 'X',[1 1716],'true','Y',[1 1716],'true'}
        
        Args.Overlap           = {};
    end

    if ischar(AllSI) || isstring(AllSI)
        % read from path
        [AllSI, Coadd, MS] = pipeline.last.load.loadVisit(AllSI);
    end

    %--- AllSI ---
    StAr = AlllSI.getStructKey(Args.HeadKeys);
    % quality info from header
    Summary.AllSI_HV = tools.struct.structarray2struct(StAr);
    % check that catalog columns are in range
    Summary.AllSI_CR = imProc.cat.checkColumnsInRange(AllSI, Args.TestCol);


    %--- Coadd ---
    StAr = Cadd.getStructKey(Args.HeadKeys);
    % quality info from header
    Summary.Coadd_HV = tools.struct.structarray2struct(StAr);
    % check that catalog columns are in range
    Summary.Coadd_CR = imProc.cat.checkColumnsInRange(Coadd, Args.TestCol);
    
    % check the quality of overlap sources (mainly, astrometry + photometry)
    Summary.Coadd_Overlap = pipeline.last.quality.overlapSources(Coadd, Args.Overlap{:});
    
    %--- MS --- 

end
