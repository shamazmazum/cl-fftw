#ifndef __FFTW3_WRAPPER__
#define __FFTW3_WRAPPER__

struct fftw3_plan;
struct fftw3_plan* create_fft_plan   (int rank, const int *dimensions, int sign);
struct fftw3_plan* create_rfft_plan  (int rank, const int *dimensions);
struct fftw3_plan* create_irfft_plan (int rank, const int *dimensions);
void destroy_plan (struct fftw3_plan* plan);

void fft   (struct fftw3_plan* plan, const fftw_complex *in, fftw_complex *out);
void rfft  (struct fftw3_plan* plan, const double *in, fftw_complex *out);
void irfft (struct fftw3_plan* plan, const fftw_complex *in, double *out);

#endif
