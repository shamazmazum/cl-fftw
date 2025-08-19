#ifndef __FFTW3_WRAPPER__
#define __FFTW3_WRAPPER__

struct fftw3s_plan;
struct fftw3s_plan* fftwraps_create_fft_plan   (int rank, const int *dimensions, int sign);
struct fftw3s_plan* fftwraps_create_rfft_plan  (int rank, const int *dimensions);
struct fftw3s_plan* fftwraps_create_irfft_plan (int rank, const int *dimensions);
void fftwraps_destroy_plan (struct fftw3s_plan *plan);

void* fftwraps_get_input_pointer  (const struct fftw3s_plan *plan);
void* fftwraps_get_output_pointer (const struct fftw3s_plan *plan);
void  fftwraps_execute_plan       (const struct fftw3s_plan *plan);

#endif
