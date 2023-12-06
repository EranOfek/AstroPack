B = tools.array.trim(A, I1, I2, J1, J2)
where A is a single or double matrix or cube
I, J are scalars:

Perform:
B = A(I1:I2, J1:J2, :);

My impression is that matlab doesn't use AVX - see:
https://medium.com/swlh/copying-data-2000-faster-with-advanced-vector-extensions-dfc469ebac9f



