function Mat=equal_diagonals_matrix(Vec)
% Populate a vector along the diagonals of a matrix (Toplitz matrix)
% Package: +TimeDelay
% Description: Given a vector, populate its values in the diagonals of a
%              matrix.
% Input  : - A vector. The first element will populate the diagonal of the
%            matrix, and the last element the corner.
% Output : - A matrix.
%     By : Eran O. Ofek                     Nov 2020
% Example: Mat=TimeDelay.equal_diagonals_matrix([1 2 3]);


N   = numel(Vec);
Mat = zeros(N,N);

% very slow:
% for I=1:1:N
%     if I==N
%         Mat = Mat + diag(ones(I,1).*Vec(N-I+1),0);
%     else
%         Mat = Mat + diag(ones(I,1).*Vec(N-I+1),N-I) + diag(ones(I,1).*Vec(N-I+1),I-N);
%     end
% end


for I=1:1:N
    Mat(I:N+1:N.^2-N.*(I-1)) = Vec(I);
    Mat(1+N.*(I-1):N+1:N.^2) = Vec(I);
end