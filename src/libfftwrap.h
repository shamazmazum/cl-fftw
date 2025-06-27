#ifndef __FFTW3_WRAPPER__
#define __FFTW3_WRAPPER__

struct fftw3_plan;
struct fftw3_plan* create_fft_plan   (int rank, const int *dimensions, int sign);
struct fftw3_plan* create_rfft_plan  (int rank, const int *dimensions);
struct fftw3_plan* create_irfft_plan (int rank, const int *dimensions);
void destroy_plan (struct fftw3_plan *plan);

void* get_input_pointer  (const struct fftw3_plan *plan);
void* get_output_pointer (const struct fftw3_plan *plan);
void execute_plan (const struct fftw3_plan *plan);

#endif
