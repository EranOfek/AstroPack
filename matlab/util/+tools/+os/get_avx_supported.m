function Result = get_avx_supported()
  persistent avx;
  if isempty(avx)
    avx2 = tools.os.mex.is_avx2_supported();
    avx512 = tools.os.mex.is_avx512_supported();
  
    if avx512
        avx = 512;
    elseif avx2
        avx = 2;
    else
        avx = 0;
    end
  end
  Result = avx;
end
