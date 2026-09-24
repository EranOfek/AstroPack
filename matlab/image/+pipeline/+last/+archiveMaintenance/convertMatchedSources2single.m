function convertMatchedSources2single(Args)
    % Convert MatchedSourecs Data fields to single precision
    % Input  : * ...,key,val,... 
    %            See code for details.
    % Output : null
    % Author : Eran Ofek (2024 Dec) 
    % Example: pipeline.last.archiveMaintenance.convertMatchedSources2single

    arguments
        Args.Template          = 'LAST.*_sci_merged_MergedMat_*.hdf5';
    end

    FF=io.files.rdir(Args.Template);
    AllFolder = unique({FF.folder});
    Nfolder   = numel(AllFolder);

    PWD = pwd;
    for Ifolder=1:1:Nfolder
        cd(AllFolder{Ifolder});
 
        F = dir(Args.Template);
        Nf = numel(F);
        for If=1:1:Nf
            MS = MatchedSources.read(F(If).name);
            delete(F(If).name)
            MS.write1(F(If).name, 'Type','single');
        end

        cd(PWD);
    end

end
