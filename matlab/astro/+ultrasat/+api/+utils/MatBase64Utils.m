%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.utils.MatBase64Utils.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 18/02/2026
% Description : Utility functions to save/load MATLAB objects to/from base64.
%               For use with PlansManagerClient saveMatlabMat/getMatlabMat.
%==========================================================================

classdef MatBase64Utils
    methods (Static)

        function base64Str = matToBase64(matObj, varName)
            % Serializes a MATLAB object to a base64 string.
            %
            % :param matObj: MATLAB object to serialize.
            % :param varName: Variable name in the .mat file (default: 'planner').
            % :return: Base64-encoded string.
            arguments
                matObj
                varName (1,1) string = "planner"
            end
            base64Str = "";
            tmpFile = [tempname, '.mat'];
            vn = char(varName);
            try
                s = struct();
                s.(vn) = matObj;
                save(tmpFile, '-struct', 's', vn, '-v7');
                fid = fopen(tmpFile, 'rb');
                if fid == -1
                    return;
                end
                bytes = fread(fid, inf, 'uint8');
                fclose(fid);
                base64Str = matlab.net.base64encode(bytes');
            catch
            end
            if exist(tmpFile, 'file')
                delete(tmpFile);
            end
        end


        function matObj = base64ToMat(base64Str, varName)
            % Deserializes a base64 string to a MATLAB object.
            %
            % :param base64Str: Base64-encoded string.
            % :param varName: Variable name in the .mat file (default: 'planner').
            % :return: MATLAB object, or [] if empty/failed.
            arguments
                base64Str
                varName (1,1) string = "planner"
            end
            matObj = [];
            if isempty(base64Str)
                return;
            end
            tmpFile = [tempname, '.mat'];
            vn = char(varName);
            try
                bytes = matlab.net.base64decode(base64Str);
                fid = fopen(tmpFile, 'wb');
                if fid == -1
                    return;
                end
                fwrite(fid, bytes);
                fclose(fid);
                loaded = load(tmpFile, vn);
                matObj = loaded.(vn);
            catch
            end
            if exist(tmpFile, 'file')
                delete(tmpFile);
            end
        end

    end
end
