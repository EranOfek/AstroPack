function LogDet=logdet(Matrix,Algo)
% Calculate the log of the determinant of a matrix
% Package: +TimeDelay
% Description: Based on 
% Input  : - A matrix.
%          - Algorithm. ['lu'] | 'chol'.
%            'chol' is faster but will work only for positive definite
%            matrix (will fail otherwise).
% Output : - The log of the determinent of the matrix.
% License: GNU general public license version 3
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: 
% Reliable: 2
%--------------------------------------------------------------------------

if nargin<2
    Algo = 'lu';
end

switch lower(Algo)
    case 'lu'
        [~,U,P] = lu(Matrix);
        DiagU = diag(U);
        C = det(P) * prod(sign(DiagU));
        LogDet = log(C) + sum(log(abs(DiagU)));
    case 'chol'
        LogDet = 2.*sum(log(diag(chol(Matrix))));
    otherwise
        error('Unknown Algorithm option');
end