%
% CURRENTLY we focus on FITS files only
%


% @Chen
% Component base class
% Package: 
% Description:
%--------------------------------------------------------------------------

% Parent class for FitsDb, HdfDb
% https://support.hdfgroup.org/HDF5/doc/H5.intro.html
%
% Our Headers are stored as DataSet of Strings with 3 columns:
% Key, Value, Comment
% 
classdef HdfDb < ImageDb
    % Properties
    properties (SetAccess = public)
       ImageGroupPrefix = "image"
       ImageDataSetName = "Image"
       HeaderDataSetName = "Header"
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = HdfDb(FileName)
            Obj.FileName = FileName;
        end
        
        function Result = open(Obj, FileName)
            Obj.FileName = FileName;
            Result = true;
        end
        
        % Read dataset from HDF5 file
        function Data = read(Obj, DataSet)
            Data = h5read(Obj.FileName, DataSet);
        end
        
        
        %
        function Header = readHeader(Obj, Index)
            Path = Obj.getHeaderPath(Index);
            Data = h5read(Obj.FileName, Path);
            Header = Obj.headerToYaml(Data);
        end
        
        
        % Get number of images, assuming that we have group per image
        function Result = getImageCount(Obj)
            Info = h5info(Obj.FileName);
            Result = length(Info.Groups);
        end
    
        
        %
        function Path = getImagePath(Obj, Index)
            Path = "/" + ImageGroupPrefix + string(Index) + "/";
        end
        
        function Path = getImageDataSetPath(Obj, Index, DataSetName)
            Path = Obj.getImagePath(Index) + DataSetName;
        end
        
        %
        function Path = getHeaderPath(Obj, Index)
            Path = getImagePath(Index) + "Header";
        end
        

        function Path = getDefaultPath(Obj)
            Path = Obj.getImagePath(1);
            
        end
        
        function DataSetPath = getDefaultDataSetPath(Obj)
            DataSetPath = Obj.getImageDataSetPath(1, 
        end
        
        % Convert DataSet to cell array
        function Yaml = headerToYaml(Obj, Header)
            
        end
        
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            fprintf("Started\n");
            
            addpath("D:\Ultrasat\AstroPack.git\matlab\external");

            FileName = "D:\\Ultrasat\\AstroPack.git\\data\\testing\\test_hdf\\test1.h5";
            db = HdfDb(FileName);
            data = db.read(FileName, "/image1/header");
            
            %
            disp(data);
 
            Result = true;
        end
    end    
        
    
end

