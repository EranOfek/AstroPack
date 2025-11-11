function [ResultFit] = defResultFit(Nai, Origin)
    % Define and allocate memory for ResultFit structure array used by astrometry solvers
    % Input  : - Number of elements in the array, or a two element vector
    %            for i, j.
    %          - Origin init. Default [] (empty cell).
    % Output : - An ResultFit structure array.
    % Author : Eran Ofek (2025 Nov) 
    % Example: ResFit=imProc.astrometry.defResultFit(1);

    arguments
        Nai
        Origin = [];
    end

    if numel(Nai)==1
        Nai = [Nai, 1];
    end

    if isempty(Origin)
        Origin = cell(Nai(1), Nai(2));
    end
       
    ResultFit = struct('ImageCenterXY',cell(Nai(1), Nai(2)),...
                    'Nsolutions',cell(Nai(1), Nai(2)),...
                    'ResPattern',cell(Nai(1), Nai(2)),...
                    'ErrorOnMean',cell(Nai(1), Nai(2)),...
                    'BestInd',cell(Nai(1), Nai(2)),...
                    'WCS',cell(Nai(1), Nai(2)),...
                    'ParWCS',cell(Nai(1), Nai(2)),...
                    'Tran',cell(Nai(1), Nai(2)),...
                    'ResFit',cell(Nai(1), Nai(2)),...
                    'Origin',Origin,...
                    'Success',cell(Nai(1), Nai(2)));


end
