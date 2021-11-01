
function y=blockmatrix(x,bs)
% blockmatrix: reshape N-D matrix such that blocks of elements are adjacent
%

% by Oded Aharonson
% June 28, 2016

if nargin==0,
    disp('blockmatrix: running demo')
    szr=[3,3]; bs=2; 
    x=reshape(1:(szr(1)*szr(2)*bs*bs),[szr(1)*bs,szr(2)*bs])
    y=blockmatrix(x,bs)
    p=peaks(300);
    q=blockmatrix(p,3); q=reshape(q,9,100,100); q=squeeze(mean(q,1));
    subplot(121); imagesc(p); axis image;
    subplot(122); imagesc(q); axis image;
return
end

sz=size(x);
szr=sz./bs;
if any(~isint(szr))
    error('blockmatrix: input matrix dimensions must be multiple of blocksize!')
end
y=permute(reshape(x,[bs,szr(1),bs,szr(2)]),[1,3,2,4]);