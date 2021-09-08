# Package: celestial.meteors


### celestial.meteors.meteor_multistation

Direction for detection of a meteor observed from another station Package: celestial.meteors Description: Given a list of observers geodetic coordinates in which the first point is a reference point; the azimuth and


    
    Direction for detection of a meteor observed from another station  
    Package: celestial.meteors  
    Description: Given a list of observers geodetic coordinates in which  
    the first point is a reference point; the azimuth and  
    altitude in which an observer (located in the reference  
    position) is looking to; and the height (H) of a meteor  
    trail - calculate the azimuth and altitude in which  
    observers in other points should look to, in order to  
    detect the same meteor. The function takes into acount  
    the Earth curvature (first order).  
    Input  : - CooList : matrix of geodetic coordinates [Long, Lat, Height]  
    in radians and meters, respectively. One raw per observer.  
    The first raw is considered as a reference point.  
    If only two columns are given, the hieght is set  
    to 0 meters.  
    - Meteor height [meters].  
    - [Az, Alt] in radians in which the reference observer  
    is looking to.  
    Output : - Vector of azimuths of meteor, per each coordinate [rad].  
    - Vector of altitudes of meteor, per each coordinate [rad].  
    - Observer-meteor distance [meters], per each coordinate.  
    - Coordinates [Long, Lat] for which meteor is in the  
    zenith [radians].  
    Tested : Matlab 5.3  
    By : Eran O. Ofek                    Dec 2001  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [Az,Alt,Dist,Coo]=celestial.meteors.meteor_multistation([34.763 30.596; 35 31]./(180./pi),100000,[1 1]);  
    Reliable: 2  
      
### celestial.meteors.meteors_db

Return a meteor shower database (incomplete) Package: celestial.meteors Description: Return a meteor showers database (not complete).


    
    Return a meteor shower database (incomplete)  
    Package: celestial.meteors  
    Description: Return a meteor showers database (not complete).  
    Input  : null  
    Output : - A structure array containing the meteor showers DB.  
    The following fields are available.  
    .Name  
    .Code  
    .CommentZHR  
    .Comet  
    .Data - a vector containing  
    begin date day  
    begin date month  
    end date day  
    end date day  
    peak date day  
    peak date day  
    solar long. [deg]  
    J2000 RA [deg]  
    J2000 Dec [deg]  
    Velocity [km/s]  
    r  
    ZHR  
    radiant daily motion RA  
    radiant daily motion Dec  
    Tested : Matlab 7.13  
    By : Eran Ofek                       Aug 2012  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: DB=celestial.meteors.meteors_db;  
    Reliable: 2  
      
      
