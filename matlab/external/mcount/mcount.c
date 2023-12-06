/*
 * mcount.c
 *
 * count the number of elements in a matrix or vector that satisfy
 * a given logical condition with a scalar. To compile it from the
 * Matlab Command Window use:
 * > mex mcount.c
 * Please note: you must have successfully run the command:
 * > mex -setup
 * in order to produce a mex file.
 *
 * This is a MEX-file for MATLAB.
 * Copyright 2016 Stefano Gianoli
 */

 /* $Revision: 2.1 $ */

#include "mex.h"

typedef enum typeCOND {
	EQ, /* equal */
	NE, /* not equal */
	GT, /* greater than */
	LT, /* less than */
	GE, /* greater or equal */
	LE  /* less or equal */
} tCOND;

unsigned char runtimeIsNaN(double value)
{
#if defined(_MSC_VER) && (_MSC_VER <= 1200)
	return _isnan((double)value) ? true : false;
#else
	return (value != value) ? 1U : 0U;
#endif
}

double mcount(double x, double *M, tCOND c, mwSize nIter)
{
	mwSize i;
	mwSize count = 0;

	switch (c)
	{
	case GE:
		for (i = 0; i < nIter; i++)
			if (*(M + i) >= x)
				count++;
		break;
	case GT:
		for (i = 0; i < nIter; i++)
			if (*(M + i) > x)
				count++;
		break;
	case LE:
		for (i = 0; i < nIter; i++)
			if (*(M + i) <= x)
				count++;
		break;
	case LT:
		for (i = 0; i < nIter; i++)
			if (*(M + i) < x)
				count++;
		break;
	case EQ:
		if (runtimeIsNaN(x)) {
			for (i = 0; i < nIter; i++)
				if (runtimeIsNaN(*(M + i)))
					count++;
		}
		else {
			for (i = 0; i < nIter; i++)
				if (*(M + i) == x)
					count++;
		}
		break;
	case NE:
		if (runtimeIsNaN(x)) {
			for (i = 0; i < nIter; i++)
				if (!runtimeIsNaN(*(M + i)))
					count++;
		}
		else {
			for (i = 0; i < nIter; i++)
				if (*(M + i) != x)
					count++;
		}
		break;
	}
	return count;
}

void errorInvalidCOND()
{
	mexErrMsgIdAndTxt("MATLAB:mcount:invalidInputCOND",
		"Third input 'COND': Invalid logic operator. Must be: '==', '>=', '<=', '<', '>' or '!='");
}

/* the gateway function */
void mexFunction(int nlhs, mxArray *plhs[],
	int nrhs, const mxArray *prhs[])
{
	double  *M, *z, x;	
	mwSize  sLen, mSize;
	char    c[5] = "     ";
	tCOND   COND;

	/*  check for proper number of arguments */
	if (nrhs != 3)
		mexErrMsgTxt("Three inputs required.\n"
			"RES = mcount(MATRIX, X, COND)\n"
			"Given a vector or matix MATRIX, counts the number of times that\n"
			"the scalar X satisfy the (string) condition COND. The result\n"
			"is returned in the variable RES. COND operators allowed are :\n"
			"'==', '>=', '<=', '<', '>', '!='.\n"
			"The function is generally faster than SUM(MATRIX'COND'X) or\n"
			"LENGTH(FIND(MATRIX'COND'X)).");
	if (nlhs > 1)
		mexErrMsgTxt("One output maximum allowed.");

	/* check to make sure the first input argument is a double-precision matrix */
	if (!mxIsDouble(prhs[0]) || mxIsComplex(prhs[0]))
		mexErrMsgTxt("The first input MATRIX  must be a double-precision matrix.");

	/* check to make sure the second input argument is a scalar */
	if (!mxIsDouble(prhs[1]) || mxIsComplex(prhs[1]) ||
		mxGetNumberOfElements(prhs[1]) != 1)
		mexErrMsgTxt("The second input X must be a double-precision scalar.");

	/* Check for proper input type */
	if (!mxIsChar(prhs[2]) || (mxGetM(prhs[2]) != 1))
		errorInvalidCOND();

	sLen = (mwSize)mxGetN(prhs[2]) * sizeof(mxChar) + 1;

	if (sLen > 5)
		errorInvalidCOND();

	/*  get the scalar input x */
	x = mxGetScalar(prhs[1]);

	/*  create a pointer to the input matrix M */
	M = mxGetPr(prhs[0]);

	/* Copy the string data from string_array_ptr and place it into buf. */
	if (mxGetString(prhs[2], c, sLen) != 0)
		errorInvalidCOND();

	/* Validate the logic operator */
	switch (sLen)
	{
	case 3:
		switch (*c) {
		case '>':
			COND = GT;
			break;
		case '<':
			COND = LT;
			break;
		default:
			errorInvalidCOND();
			break;
		}
		break;
	case 5:
		if (*(c + 1) == '=')
			switch (*c) {
			case '>':
				COND = GE;
				break;
			case '<':
				COND = LE;
				break;
			case '!':
				COND = NE;
				break;
			case '=':
				COND = EQ;
				break;
			default:
				errorInvalidCOND();
				break;
			}
		else
			errorInvalidCOND();
		break;
	}

	/*  get the dimensions of the matrix input M */
	mSize = (mwSize)mxGetNumberOfElements(prhs[0]);

	/*  set the output pointer to the output scalar */
	plhs[0] = mxCreateDoubleScalar(mxREAL);

	/*  create a C pointer to a copy of the output scalar */
	z = mxGetPr(plhs[0]);

	/*  call the C subroutine */
	*z = mcount(x, M, COND, mSize);
}