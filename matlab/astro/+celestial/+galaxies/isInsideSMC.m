function [Flag] = isInsideSMC(RA, Dec, Units)
    % Check if coordinates is within the SMC footprints.
    % Input  : - J2000 RA
    %          - J2000 Dec
    %          - Units: 'rad'|'deg'. Default is 'deg'. 
    % Output : - A vector of logical flags indicating if corrdinates are
    %            within the SMC footprints.
    % Author : Eran Ofek (2024 Nov) 
    % Example: [Flag] = celestial.galaxies.isInsideSMC(1.4, -1.2, 'rad')

    arguments
        RA
        Dec
        Units   = 'deg';
    end
    
    Conv = convert.angular(Units, 'rad');
    Long = RA(:).*Conv;
    Lat  = Dec(:).*Conv;
    

    Corners = [0.255763457469336         -1.24849219159328
         0.278016405432263         -1.25111018547127
         0.304996286786009         -1.26041860814857
          0.30477812062951         -1.27001791903454
         0.288124770683397         -1.28165344738117
         0.222965811942276         -1.28892565259781
         0.185441233024397         -1.28892565259781
         0.160424847079145         -1.28456232946783
          0.15198908902784         -1.27670834783385
         0.154098028540666         -1.27263591291253
         0.180350689372747         -1.26914525440854
         0.225147473507269          -1.2525646265146
         0.241728101401215         -1.25023752084527
         0.255763457469336         -1.24849219159328];
      
    Flag = celestial.htm.in_polysphere([Long, Lat],Corners);
    
end
