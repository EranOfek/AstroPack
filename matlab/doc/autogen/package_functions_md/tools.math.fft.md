# Package: tools.math.fft


### tools.math.fft.fft_freq

Return the frequencies corresponding to fftshift(fft(vec_of_size_N)) Package: tools.math.fft Description: Return the frequencies corresponding to fftshift(fft(vec_of_size_N)), without dviding by the total


    
    Return the frequencies corresponding to fftshift(fft(vec_of_size_N))  
    Package: tools.math.fft  
    Description: Return the frequencies corresponding to  
    fftshift(fft(vec_of_size_N)), without dviding by the total  
    time span.  
    Input  : - Number of points.  
    Output : - Vector of frequencies.  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: FreqVec=tools.math.fft.fft_freq(4)  
    Reliable: 2  
      
      
      
### tools.math.fft.fft_symmetric

Make a 1-D fft a complex-conjugate symmetric Package: Util.fit Description: Given a 1-D fft of a vector, or a matrix, force the negative frequency signal to be complex conjugate of the positive


    
    Make a 1-D fft a complex-conjugate symmetric  
    Package: Util.fit  
    Description: Given a 1-D fft of a vector, or a matrix, force the negative  
    frequency signal to be complex conjugate of the positive  
    frequency signal.  
    Input  : - fft of a data - e.g., fft(data).  
    * Arbitrary number of pairs of arguments: ...,keyword,value,...  
    where keyword are one of the followings:  
    Output : -  
    License: GNU general public license version 3  
    By : Eran O. Ofek                    Feb 2020  
    URL : http://weizmann.ac.il/home/eofek/matlab/  
    Example: [newFFT,Freq]=Util.fft.fft_symmetric(fftD,Dim)  
    Reliable: 2  
      
      
      
      
