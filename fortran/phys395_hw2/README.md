To compile and run, simply:

    make

Outputs:

 - *.png files
 - stdout printing out other info

The stdout is reproduced here for reference:

	solver = svd
	n = 3

		Best fit params:   -1.17   4.18   2.00   0.71
	   Condition number:       2.01
	Chi-square (actual):    9418.07
	Chi-square (expect):    9126.00

	----

	solver = svd
	n = 7

		Best fit params:   -1.17   4.18   2.00   0.71   0.21   0.01  -0.09   0.20
	   Condition number:       2.02
	Chi-square (actual):    8992.42
	Chi-square (expect):    9122.00

	----

	solver = lss
	n = 3

		Best fit params:   -1.17   4.18   2.00   0.71
	   Condition number:       1.42
	Chi-square (actual):    9418.07
	Chi-square (expect):    9126.00

	----

	solver = lss
	n = 7

		Best fit params:   -1.17   4.18   2.00   0.71   0.21   0.01  -0.09   0.20
	   Condition number:       1.42
	Chi-square (actual):    8992.42
	Chi-square (expect):    9122.00

	----

	The condition number for the SVD matrix is larger than
	the condition number for the Linear Least Squares (LSS) matrix.
	Otherwise, the best fit parameters appear to be the same.

