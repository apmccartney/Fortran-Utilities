Resonance-Integral Program (RIP)

Requirements
=============
- A Fortran-2008 compiler
- GNU Make (Optional but recommended)

Installation:
=============
This code includes a Makefile for simple compilation. Simply modify the compiler variable in the script to a fortran 2008 compiler in your path. Afterwords, compilation is as easy as:
```
make
```

A demonstration of the quadrature module is also provided and may be compiled by:
```
make quadrature-demo
```

Use
============
When calling resonance-integral, it is necessary to supply an input filename and output filename (the requirement for an output filename will be removed in future revisions). A sample call:
```
$ ./resonance-integral resdata.inp resdata.out
```
In addition, two flags are supported:
* '-v' '--verbose' : Output is echoed to the console in addition to the specified file
* '-h' '--help'    : A prompt is echoed to the console with instructions on the program.

Thoery
============
This program approximates a definite integral using adaptive 2-point Gauss-Legendre numerical integration.  This is accompished by way of a recursive algorithm. 

1. A function and starting interval is specified, along with a desired LOCAL error tolerance. 
2. The integral is computed over this interval.
3. The interval is bisected into two subintervals.
4. The integral over each subinterval is computed.
5. The sum of the subinterval integrals is compared to the integral of the parent interval.
6. If the relative difference exceeds the tolerence, the algorithm is in turn, called on each of the subintervals
7. The integral is taken to be the sum of the value returned by the subintervals.

This algorithm provides a denser mesh over regions which 2-point Gauss-Legendre performs poorly, while maintaining a sparce mesh over well-behaved regions.
