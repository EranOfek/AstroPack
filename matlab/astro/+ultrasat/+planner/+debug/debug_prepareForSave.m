function debug_prepareForSave()
    % Test prepareForSave / restoreAfterLoad / clone behavior
    
    fprintf('--- debug_prepareForSave START ---\n');
    
    % 1. Get planner (assumes it already exists in workspace or load it)
    % You can replace this with your real loading logic
    upLCS = ultrasat.planner.uplanner('AstPlanner','YS','Type','LCS');
    upLCS.StartTime = '2029-02-01 00:00:00';
    upLCS.EndTime = upLCS.StartTime+caldays(420);
    upLCS.DailyWindowStartTime = duration('00:00:00');
    
    fprintf('--- analyzePlannerSize - after creation ---');
    ultrasat.planner.analyzePlannerSize(upLCS);
    
    LCS_grid = readtable(fullfile(upLCS.BaseDataDir,'LCS_nonoverlapping_grid_surveys.csv'));
    upLCS.addUniqTargets(LCS_grid.RA,LCS_grid.Dec,'Name',num2cell(LCS_grid.Field));

    fprintf('--- analyzePlannerSize - after addUniqTargets ---');
    ultrasat.planner.analyzePlannerSize(upLCS);

    % This is the new version of LCS (2026)
    upLCS.buildLCS1;      

    fprintf('--- analyzePlannerSize - after buildLCS1 ---');
    ultrasat.planner.analyzePlannerSize(upLCS);

    % 
    planner = upLCS;  
    fprintf('Original planner:\n');
    fprintf('  Vis empty? %d\n', isempty(planner.Vis));
    fprintf('  Mclient empty? %d\n', isempty(planner.Mclient));
    
    % 2. Measure original size
    fprintf('\nMeasuring original size...\n');
    bytes_orig = numel(getByteStreamFromArray(planner));
    fprintf('  Original size: %.2f MB\n', bytes_orig/1e6);
    
    fprintf('\nCalculating Vis checksum (original)...\n');
    hash_orig = calcVisHash(planner.Vis);
    fprintf('  Original Vis hash: %s\n', hash_orig);

    % 3. Clone
    fprintf('\nCloning planner...\n');
    planner_copy = planner.clone();
    
    % 4. Apply prepareForSave on clone
    fprintf('Applying prepareForSave on clone...\n');
    planner_copy.prepareForSave();
    
    vn = 'planner';
    s = struct();
    s.(vn) = planner;
    save('c:/temp/planner_lcs_after_build.mat', '-struct', 's', vn, '-v7');

    s = struct();
    s.(vn) = planner_copy;
    save('c:/temp/planner_lcs_without_vis.mat', '-struct', 's', vn, '-v7');

                
    % 5. Verify original NOT modified
    fprintf('\nChecking original object integrity...\n');
    fprintf('  Original Vis empty? %d (should be 0)\n', isempty(planner.Vis));
    fprintf('  Original Mclient empty? %d (should be 0)\n', isempty(planner.Mclient));
    
    % 6. Verify cleaned object
    fprintf('\nChecking cleaned object...\n');
    fprintf('  Copy Vis empty? %d (should be 1)\n', isempty(planner_copy.Vis));
    fprintf('  Copy Mclient empty? %d (should be 1)\n', isempty(planner_copy.Mclient));
    
    % 7. Measure new size
    fprintf('\nMeasuring cleaned size...\n');
    bytes_clean = numel(getByteStreamFromArray(planner_copy));
    fprintf('  Clean size: %.2f MB\n', bytes_clean/1e6);
    
    fprintf('\nSize reduction factor: %.2fx\n', bytes_orig / max(bytes_clean,1));
    
    % 8. Serialize to base64
    fprintf('\nEncoding to base64...\n');
    base64Str = ultrasat.api.utils.MatBase64Utils.matToBase64(planner_copy);
    fprintf('  Base64 length: %d\n', length(base64Str));
    
    % 9. Deserialize
    fprintf('\nDecoding from base64...\n');
    planner_loaded = ultrasat.api.utils.MatBase64Utils.base64ToMat(base64Str);
    
    % 10. Restore runtime
    fprintf('Restoring runtime fields...\n');
    
    % You must provide a client here (mock or real)   
    t0 = tic;
    planner_loaded = planner_loaded.restoreAfterLoad('Mclient', planner.Mclient);
    elapsed = toc(t0);
    fprintf('  restoreAfterLoad time: %.3f sec\n', elapsed);

    fprintf('  Loaded Vis empty? %d (should be 0 after restore)\n', isempty(planner_loaded.Vis));
    fprintf('  Loaded Mclient empty? %d (should be 0)\n', isempty(planner_loaded.Mclient));

    fprintf('  Original Vis hash: %s\n', hash_orig);    
    fprintf('\nCalculating Vis checksum (restored)...\n');
    hash_loaded = calcVisHash(planner_loaded.Vis);
    fprintf('  Restored Vis hash: %s\n', hash_loaded);
    
    if strcmp(hash_orig, hash_loaded)
        fprintf('OK - Vis MATCH (checksum)\n');
    else
        fprintf('FAIL - Vis MISMATCH (checksum)\n');
    end

    % DONE
    fprintf('\n--- debug_prepareForSave DONE ---\n');

end


function h = calcVisHash(vis)
    try
        bytes = getByteStreamFromArray(vis);
        md = java.security.MessageDigest.getInstance('MD5');
        md.update(bytes);
        h = sprintf('%08x', typecast(md.digest(), 'uint32'));
    catch
        h = '';
    end
end
