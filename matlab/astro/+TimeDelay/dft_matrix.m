function [Fjl]=dft_matrix(N)
% Return the discrete Fourier Transform matrix
% Package: +TimeDelay
% Description: 
% Input  : - Number of frequencies
% Output : - DFT matrix F_jl
% License: GNU general public license version 3
%     By : Eran O. Ofek                    Feb 2020
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [Fjl]=TimeDelay.dft_matrix(10)
% Reliable: 
%--------------------------------------------------------------------------

jV = (0:1:N-1);
lV = jV.';

Fjl = exp(-1i.*2.*pi.*jV.*lV./N); %./sqrt(N);
