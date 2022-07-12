*This is a simple MPS problem to test the MPS reader
NAME          simple
ROWS
 N  obj
 L  row1
 L  row2
COLUMNS
    X         obj       1               row1      3
    Y         obj       4               row1      1
    Y         row2      1
    Z         obj       8               row2      2
RHS
    rhs1      row1      8               row2      7
ENDATA
