function prepGAIAvari(Args)
    % The catalog doesn't contain RA/Dec so use VizierR to download
    
    arguments
        Args.URL     = 'http://cdn.gea.esac.esa.int/Gaia/gdr3/Variability/vari_summary/';
    end
    
    
     Links = www.find_urls(Args.URL,'match','http.*?\.csv.gz');
    
     www.pwget(Links,'--no-check-certificate -U Mozilla',10);
     
     system('gzip -d *.gz');
     
     Files = dir('Vari*.csv');
     Nfile = numel(Files);
     for Ifile=1:1:Nfile
	     T = io.files.readtable1(Files(Ifile).name,'CommentStyle','#');
         
         T
     end
     
end