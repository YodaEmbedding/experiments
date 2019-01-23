To compile and run, simply:

    make

Now look at the output plots *.png files, and the terminal output giving a table of maximal error values.

Example output:

    The max and argmax of the error is provided below for both
    Chebyshev polynomial with 10 terms and
    Chebyshev polynomial with 100 terms.

    Name         |    max err10 | argmax err10 |   max err100 | argmax err100
    f_uniform    |        0.173 |       -0.928 |        8.102 |       -0.987
    df_uniform   |          nan |       -1.000 |          nan |       -1.000
    f_zeros      |        0.089 |        0.000 |        0.000 |        0.000
    df_zeros     |          nan |       -1.000 |          nan |       -1.000
