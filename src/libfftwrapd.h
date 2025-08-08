#ifndef __FFTW3_WRAPPER__
#define __FFTW3_WRAPPER__

struct fftw3d_plan;
struct fftw3d_plan* fftwrapd_create_fft_plan   (int rank, const int *dimensions, int sign);
struct fftw3d_plan* fftwrapd_create_rfft_plan  (int rank, const int *dimensions);
struct fftw3d_plan* fftwrapd_create_irfft_plan (int rank, const int *dimensions);
void fftwrapd_destroy_plan (struct fftw3d_plan *plan);

void* fftwrapd_get_input_pointer  (const struct fftw3d_plan *plan);
void* fftwrapd_get_output_pointer (const struct fftw3d_plan *plan);
void  fftwrapd_execute_plan       (const struct fftw3d_plan *plan);

#endif
