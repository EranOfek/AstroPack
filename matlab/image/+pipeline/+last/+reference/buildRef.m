function [Result] = buildRef(RefGrid, DB, Args)
    % given a grid of reference images, build them from proc images
    %     employs proc/coadd image DB 
    % Input  : - a grid of reference images (number, RA1-RA4, Dec1-Dec4) 
    %          - a DB object
    %          * ...,key,val,... 
    % Output : - reference image files written to disk and ref_images table filled in the DB
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: load('~/LAST_RefIm_Grid_v2.mat'); D = db.Db; ...
    %          pipeline.last.reference.buildRef(LAST_RefIm_Grid,D);
    arguments
        RefGrid
        DB                     
        Args.RefTable = 'ref_images_v4';       
        Args.NsideLow = 2^8; 
    end
    % 
    RAD = 180/pi;
    % loop over the ref. image grid
    Nref = height(RefGrid);
    for Iref = 1:Nref
        % 0. read the ref polygon to be covered 
        P0 = [RefGrid.RA1(Iref), RefGrid.RA1(Iref); RefGrid.RA2(Iref), RefGrid.Dec2(Iref); ...
              RefGrid.RA3(Iref), RefGrid.Dec3(Iref); RefGrid.RA4(Iref), RefGrid.Dec4(Iref)];
        UpixCenter = celestial.healpix.ang2pix(Args.NsideLow, RefGrid.RA(Iref)/RAD, RefGrid.Dec(Iref)/RAD);
        % 1. find the overlapping single-epoch proc images 
        
        S = "select * from raw_images where";
        W = " upix_low = UpixCenter or upix_low = UpixNeighb";
        T = DB.query(strcat(S,W));
        
        % 2. qualify the overlapping proc images
        
        % 3. select exposures by specific obs. time, time span, etc. 
        
        % 4. for each epoch:
        Nexp = size();
        for Iexp = 1:Nexp
            % 4.1 merge the set of covering crops 
            
            % 4.2 rotate, align, and cut the merged crops to the ref. coordinates
        end
        
        % 5. proper coadd the the aligned and merged crops
        
        % 6. save the new reference on disk and fill the DB table line 
    end    
end
