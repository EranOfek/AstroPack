
classdef Coadd < Component
    properties
        
        
    end
    
    methods % constructor
        function CObj = Coadd(Args)
            % Constructor for a Match object
            % Input  : * ...,key,val,...
            %            Can be any Match object property name followed by
            %            its new value.
            % Output : - A Match object
            % Author : Eran Ofek (Apr 2021)
            % Example: M = imProc.cat.Match('Radius',5,'RadiusUnits','arcsec');
           
            arguments
                Args.Cat              % catalaog
                Args.Ref              % ref catalog
                Args.Coo              % Search coordinates - used by coneSearch, match_catsHTM
                Args.CatName          % catsHTM name
                
                Args.CooUnits
                Args.Radius
                Args.RadiusUnits
                Args.Shape
                Args.AddDistCol
                Args.DistUnits
                Args.DistColName
                Args.DistColPos
                
                Args.OutIsSelfClass
                
                Args.CreateNewObj
            end
            
            FN = fieldnames(Args);
            for Ifn=1:1:numel(FN)
                Obj.(FN{Ifn}) = Args.(FN{Ifn});
            end
        end
    end
    
    methods % basic function
        % normalize
        % coadd
        % properCoadd
        
        function Result = subtractOffset(Obj, ImObj, Offset, Args)
            % Remove offset (constant) from AstroImage
            % Input  : - A Coadd object.
            %          - An AstroImage object.
            %          - An AstroImage object, or a cell array of matrices
            %            (images) or scalars, or a vector of scalars.
            %            If an array, then will be converted to a cell of
            %            scalars using num2cell. Each cell element will be
            %            subtracted from the AstroImage. Of one cell
            %            element then will be subtracted from all images.
            %          * ...,key,val,...
            %            'DataProp' - The data property in the AstroImage
            %                   (both input and offset) that contains the data.
            %                   Default is 'ImageData'.
            %            'DataPropIn' - The data property in the
            %                   ImageComponent that contains the image
            %                   data. Default is 'Data'.
            %            'CreateNewObj' - A logical indicating if the
            %                   output is a new object. If empty, then will
            %                   check the number of output argumnets. If
            %                   zero (e.g., C.subtractOffset(...)) then
            %                   will update the input image object. Else,
            %                   will create a new object.
            % Output : - An AstroImage object.
            % Author : Eran Ofek (Apr 2021)
            % Example: AI = AstroImage({ones(3,3), 3.*ones(4,4)});
            %          C  = imProc.image.Coadd;
            %          R  = C.subtractOffset(AI,1);
            %          R  = C.subtractOffset(AI,[1 2]);
            %          R  = C.subtractOffset(AI,{1 2}); % the same
            %          R  = C.subtractOffset(AI,AI); 
            
            arguments
                Obj(1,1) 
                ImObj
                Offset                    = [];   % AstroImage, cell of matrices, or array
                Args.DataProp             = 'ImageData';
                Args.DataPropIn           = 'Data';
                Args.CreateNewObj         = [];
            end
            
            if isempty(Args.CreateNewObj)
                if nargout==0
                    Args.CreateNewObj = false;
                else
                    Args.CreateNewObj = true;
                end
            end
                    
            
            if isnumeric(Offset)
                Offset = num2cell(Offset);
            end
            
            if Args.CreateNewObj
                Result = ImObj.copyObject;
            else
                Result = ImObj;
            end
            Nobj = numel(ImObj);
            Noff = numel(Offset);
            for Iobj=1:1:Nobj
                Ioff = min(Iobj, Noff);
                if iscell(Offset)
                    Tmp = Offset{Ioff};
                else
                    Tmp = Offset(Ioff).(Args.DataProp).(Args.DataPropIn);
                end
                
                Result(Iobj).(Args.DataProp).(Args.DataPropIn) = ImObj(Iobj).(Args.DataProp).(Args.DataPropIn) - Tmp;
            end
        end
        
        
        
    end
    
end