function Result = copyJavaJarToTempDir(JarFileName)
    % Copy Java .jar file to temporary folder and it to javaaddpath()
    % Required because in some cases we cannot use .jar files directly from this
    % location in the repository. Don't know yet the reason (it works fine with +yaml).
    % Input: JarFileName - .jar file
    % Example: tools.os.copyJavaJarToTempDir(fullfile(tools.os.getAstroPackExternalPath(), 'opencsv', 'opencsv-5.5.2.jar'));
    [~, FName, Ext] = fileparts(JarFileName);
    TargetJar = fullfile(tools.os.getTempDir(), strcat(FName, Ext));
    if ~copyfile(JarFileName, TargetJar)
        io.msgLog(LogLevel.Warning, 'Failed to copy .jar file (if already running, ignore this warning): %s to %s', JarFileName, TargetJar);
    end
    javaaddpath(TargetJar);                
    Result = true;
end
