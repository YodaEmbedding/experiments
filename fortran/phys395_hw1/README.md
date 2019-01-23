To compile and run, simply:

    make

Outputs:

 - *.png files
 - stdout giving a table of maximal error values

The stdout is reproduced here for reference:

    The max and argmax of the error is provided below for both
    Chebyshev polynomial with 10 terms and
    Chebyshev polynomial with 100 terms.

    Name           max err10  argmax err10    max err100 argmax err100
    f_uniform          0.173        -0.928        22.510        -0.988
    f_zeros            0.089         0.000         0.000         0.000
    df_uniform         5.675        -1.000     10247.316         0.996
    df_zeros           0.789        -0.122         0.000        -1.000
