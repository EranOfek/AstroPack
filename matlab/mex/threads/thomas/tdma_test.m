clear;
% TDMA_TEST  A series of tests for TDMA to properly cover all use cases.
%
%     See also TDMA

%% Test REAL arrays:
n = 32;
main = 2*(1:n)'; lower = (1:n)'; upper = (1:n)'; f = ones(n, 1);

A = spdiags([lower+1, main, upper-1], -1:1, n, n);
y1 = A\f;

% test solving down a column:
y2 = tdma(main, lower, upper, f);
assert(isempty(find(abs(y2 - y1) > eps, 1)));

% test solving across a row:
y2 = tdma(main', lower', upper', f', 2)';
assert(isempty(find(abs(y2 - y1) > eps, 1)));

main = reshape(main, 1, 1, n);
lower = reshape(lower, 1, 1, n);
upper = reshape(upper, 1, 1, n);
f = reshape(f, 1, 1, n);

% test solving through a shaft:
y2 = squeeze(tdma(main, lower, upper, f, 3));
assert(isempty(find(abs(y2 - y1) > eps, 1)));

%% Test COMPLEX arrays:
main = 1i*2*(1:n)'; lower = 1i*(1:n)'; upper = 1i*(1:n)'; f = 1i*ones(n, 1);

A = spdiags([lower+1i, main, upper-1i], -1:1, n, n);
y1 = A\f;

% test solving down a column:
y2 = tdma(main, lower, upper, f);
assert(isempty(find(abs(y2 - y1) > eps, 1)));

% test solving across a row:
y2 = tdma(main', lower', upper', f', 2)';
assert(isempty(find(abs(y2 - y1) > eps, 1)));

main = reshape(main, 1, 1, n);
lower = reshape(lower, 1, 1, n);
upper = reshape(upper, 1, 1, n);
f = reshape(f, 1, 1, n);

% test solving through a shaft:
y2 = squeeze(tdma(main, lower, upper, f, 3));
assert(isempty(find(abs(y2 - y1) > eps, 1)));

%% Test REAL matrices:
main = 2*(1:n)'; lower = (1:n)'; upper = (1:n)'; f = ones(n, 1);

A = spdiags([lower+1, main, upper-1], -1:1, n, n);
y1 = repmat(A\f, 1, 11);

main = repmat(main, 1, 11); lower = repmat(lower, 1, 11);
upper = repmat(upper, 1, 11); f = repmat(f, 1, 11);

% test solving down some columns:
y2 = tdma(main, lower, upper, f);
assert(isempty(find(abs(y2 - y1) > eps, 1)));

% test solving across some rows:
y2 = tdma(main', lower', upper', f', 2)';
assert(isempty(find(abs(y2 - y1) > eps, 1)));

main = permute(main, [3, 2, 1]);
lower = permute(lower, [3, 2, 1]);
upper = permute(upper, [3, 2, 1]);
f = permute(f, [3, 2, 1]);

% test solving through some shafts:
y2 = squeeze(tdma(main, lower, upper, f, 3))';
assert(isempty(find(abs(y2 - y1) > eps, 1)));

%% Test COMPLEX matrices:
main = 1i*2*(1:n).'; lower = 1i*(1:n).'; upper = 1i*(1:n).'; f = 1i*ones(n, 1);

A = spdiags([lower+1i, main, upper-1i], -1:1, n, n);
y1 = repmat(A\f, 1, 11);

main = repmat(main, 1, 11); lower = repmat(lower, 1, 11);
upper = repmat(upper, 1, 11); f = repmat(f, 1, 11);

% test solving down some columns:
y2 = tdma(main, lower, upper, f);
assert(isempty(find(abs(y2 - y1) > eps, 1)));

% test solving across some rows:
y2 = tdma(main.', lower.', upper.', f.', 2).';
assert(isempty(find(abs(y2 - y1) > eps, 1)));

main = permute(main, [3, 2, 1]);
lower = permute(lower, [3, 2, 1]);
upper = permute(upper, [3, 2, 1]);
f = permute(f, [3, 2, 1]);

% test solving through some shafts:
y2 = squeeze(tdma(main, lower, upper, f, 3)).';
assert(isempty(find(abs(y2 - y1) > eps, 1)));