function prepGAIAspec(Args)
    % Reformat the GAIA sampled spectra into HDF5 files and generate a
    % catsHTM catalog for fast access of the spectra.
    % Author : Eran Ofek (Jul 2022)
   
    arguments
        % spectralo units W m−2 nm-1
        Args.URL = 'http://cdn.gea.esac.esa.int/Gaia/gdr3/Spectroscopy/xp_sampled_mean_spectrum/';
        Args.Wave = (336:2:1020).';
    end
    
    %%
    % get all file names
    List = www.find_urls(Args.URL,'match','http.*?\.csv.gz');

    % download files
    www.pwget(List,'--no-check-certificate -U Mozilla',10);
    
    system('gzip -d *.csv.gz');
    
    %%
    Nwave = numel(Args.Wave);
    Files = dir('XpSampledMean*.csv');
    Nfile = numel(Files);
    Cat   = nan(1e6, 4);
    K     = 0;
    for Ifile=1:1:Nfile
        [Ifile, Nfile]

        T = io.files.readtable1(Files(Ifile).name, 'CommentStyle','#');
        Nspec = size(T,1);
        Spec  = nan(Nspec, 2.*Nwave);
        for Ispec=1:1:Nspec
            K       = K + 1;
            Flux    = eval(T.flux{Ispec});
            FluxErr = eval(T.flux_error{Ispec});
            Spec(Ispec,:) = [Flux, FluxErr];
            Cat(K,:)      = [T.ra(Ispec), T.dec(Ispec), Ifile, Ispec];
        end
        % save hdf5 file of spectra
        SpecFileName = sprintf('GAIADR3_Spectra_%d.hdf5',Ifile);
        HDF5.save(Spec, SpecFileName,'/V', {});        
    end
    
    % prep catalog of spectra
    Cat = sortrows(Cat,2);          % sort by dec
    Cat(:,1:2) = Cat(:,1:2)./RAD;   % convert ra/dec to rad
    
    % prep catsHTM
    
    
    %%
end