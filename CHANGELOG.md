# Changelog

## Version 0.3

* Incompatible change: This wrapper is now SBCL-only (if it wasn't
  already). Adding support for other implementations which allow passing
  `(simple-array * (*))` vectors of floats to FFTW without recopying should be
  easy.
* Improvement: Now no tiny C wrapper is required for cl-fftw to work. You do not
  need a C compiler to build cl-fftw and can safely remove your fasl/so cache
  after you make an executable which uses cl-fftw.
* Impovement: SBCL's compiler has more understanding what `fft/rfft/irfft`
  functions return.

## Version 0.2

* Add support for single precision FFT. There are now `cl-fftw/single` and
  `cl-fftw/double` systems (and packages) with the same set of functions, but
  for different precisions. Loading `cl-fftw` loads both of them.
