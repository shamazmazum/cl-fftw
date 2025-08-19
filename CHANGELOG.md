# Changelog

## Version 0.2

* Add support for single precision FFT. There are now `cl-fftw/single` and
  `cl-fftw/double` systems (and packages) with the same set of functions, but
  for different precisions. Loading `cl-fftw` loads both of them.
