function [Result] = zp_fit2D(Obj, Args)
    % Fit a 2-D instMag-refMag surface as a function of X and Y to each epoch in MatchedSources 
    % and subtract it from the instMag such that the instMag will be
    % corrected for positional zp.
    %   This function is using: imUtil.relPhot.fit2Dphot
    % Input  : - A MatchedSources object.
    %          * ...,key,val,... 
    %            'FieldMag' - Field for Mag to use. Default is 'MAG_BEST'.
    %                   Use Obj.bestMag to add 'MAG_BEST' to MatchedSources
    %                   Object.
    %            'FieldX' - Field of X position. Default is 'X1'.
    %            'FieldY' - Field of Y position. Default is 'Y1'.
    %            'FieldMagErr' - Field of MagErr. Default is 'MAGERR_PSF'.
    %            'FieldFlags' - Field of Flags. Default is 'FLAGS'.
    %            'RefEpochID' - Epoch of reference image. Default is 1.
    %            'MagErrFloor' - Mag. error to add in quadrature to all
    %                   magnitude errors (for weights). Default is 0.01
    %            'fit2DphotArgs' - A cell array of additional arguments to
    %                   pass to imUtil.relPhot.fit2Dphot
    %                   Default is {}.
    %            'RemoveFlags' - A cell array of flags that indicating that
    %                   the stars should not be used in the fitting process.
    %                   Default is {'Saturated','NearEdge'}.
    %            'FieldSN' - Field of S/N. Default is 'SN_3'.
    %            'MinSN' - Min S/N to use in fit. Default is 20.
    %            'BitDict' - BitDictionary object.
    %                   Default is BitDictionary.
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   copy of the input object. Default is true.
    % Output : - A modified MatchedSources object in which the FieldMag was
    %            updated.
    % Author : Eran Ofek (2025 Mar) 
    % Example: R=lcUtil.zp_fit2D(MS)

    arguments
        Obj
        Args.FieldMag          = 'MAG_BEST';
        Args.FieldX            = 'X1';
        Args.FieldY            = 'Y1';
        Args.FieldMagErr       = 'MAGERR_PSF';
        Args.FieldFlags        = 'FLAGS';
        Args.RefEpochID        = 1;
        Args.MagErrFloor       = 0.01;
        Args.fit2DphotArgs     = {};
        Args.RemoveFlags       = {'Saturated','NearEdge'};
        Args.FieldSN           = 'SN_3';
        Args.MinSN             = 20;
        Args.BitDict           = BitDictionary;
        
        Args.CreateNewObj logical   = true;
    end

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        Result(Iobj).addSrcData;
        RefMag = Obj(Iobj).Data.(Args.FieldMag)(Args.RefEpochID,:).';
        MagErr = sqrt(Obj(Iobj).Data.(Args.FieldMagErr)(Args.RefEpochID,:).^2 + Args.MagErrFloor.^2).';

        for Iepoch=1:1:Obj(Iobj).Nepoch
            if Iepoch==Args.RefEpochID
                % skip fit
                Result(Iobj).Data.(Args.FieldMag)(Iepoch,:) = RefMag.';
            else

                IndNN          = find(~isnan(Obj(Iobj).Data.(Args.FieldFlags)(Iepoch,:).'));
                BitFlag        = false(Obj(Iobj).Nsrc, 1);
                BitFlag(IndNN) = ~imProc.cat.findBit(Obj(Iobj).Data.(Args.FieldFlags)(Iepoch,IndNN).', Args.RemoveFlags, [], Args.BitDict);

                BitFlag = BitFlag & (Result(Iobj).SrcData.(Args.FieldSN)>Args.MinSN).';

                Res2D = imUtil.relPhot.fit2Dphot(Obj(Iobj).Data.(Args.FieldMag)(Iepoch,:).',...
                             RefMag,...
                             Obj(Iobj).Data.(Args.FieldX)(Iepoch,:).',...
                             Obj(Iobj).Data.(Args.FieldY)(Iepoch,:).',...
                             'MagErr',MagErr,...
                             'UseFlag',BitFlag,...
                             Args.fit2DphotArgs{:});
                Result(Iobj).Data.(Args.FieldMag)(Iepoch,:) = Res2D.ModelMag(:).';
            end

        end
    end
    
end
