# Class: db.AstroStore



    
    Data Storge Manager (works along with AstroDb and ImagePath)  
      
    For future development see:  
      
    https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part1  
    https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part2  
      
### AstroStore




    
### copyFileToStore

Copy (or move) file to storage


    
    Copy (or move) file to storage  
### createDestFolder

Create destination folder


    
    Create destination folder  
      
### getBasePath

Return storage base path, used by ImagePath


    
    Return storage base path, used by ImagePath  
### getDataPath

Get path data folder Currently we just return our DataPath, without doing anythin with the specified Impath


    
    Get path data folder  
    Currently we just return our DataPath, without doing anythin  
    with the specified Impath  
### getImageFileName

Get full path to image


    
    Get full path to image  
### getImagePath

Get full path to image


    
    Get full path to image  
### getSingleton




    
### insertFile

Insert file record to database @Todo: Need to define the table structure


    
    Insert file record to database  
    @Todo: Need to define the table structure  
### manageTimerEvent

Timer callback function


    
    Timer callback function  
### perfTest

AstroStore.perfTest


    
    AstroStore.perfTest  
      
### setup

Load settings from configuration Currently we work with single data folder, in the future we may enhance the functionality to support multiple data folders (need to decide the logic)


    
    Load settings from configuration  
    Currently we work with single data folder, in the future  
    we may enhance the functionality to support multiple data  
    folders (need to decide the logic)  
### startTimer

Setup and start timer https://www.mathworks.com/help/matlab/ref/timer-class.html https://www.mathworks.com/help/matlab/matlab_prog/timer-callback-functions.html


    
    Setup and start timer  
    https://www.mathworks.com/help/matlab/ref/timer-class.html  
    https://www.mathworks.com/help/matlab/matlab_prog/timer-callback-functions.html  
### stopTimer

Stop timer


    
    Stop timer  
### stressTest

AstroStore.stressTest


    
    AstroStore.stressTest  
      
### unitTest

AstroStore.unitTest


    
    AstroStore.unitTest  
      
