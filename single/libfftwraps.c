#include <fftw3.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "libfftwraps.h"

struct fftw3s_plan {
    fftwf_plan plan;
    void      *in;
    void      *out;
    size_t     total_in;
    size_t     total_out;
};

static size_t multiply_dimensions (int rank, const int *dimensions) {
    size_t total = 1;
    for (int i = 0; i < rank; i++) {
        total *= dimensions[i];
    }
    return total;
}

struct fftw3s_plan* fftwraps_create_fft_plan (int rank, const int *dimensions, int sign) {
    size_t total = multiply_dimensions (rank, dimensions);
    fftwf_complex *array  = malloc (sizeof(fftwf_complex) * total);
    fftwf_plan plan = fftwf_plan_dft (rank, dimensions, array, array, sign, FFTW_ESTIMATE);

    if (plan == NULL) {
        goto bad;
    }

    struct fftw3s_plan *res = malloc (sizeof(struct fftw3s_plan));
    res->plan      = plan;
    res->in        = array;
    res->out       = NULL;
    res->total_in  = total;
    res->total_out = 0;

    return res;
bad:
    free (array);
    return NULL;
}

struct fftw3s_plan* fftwraps_create_rfft_plan (int rank, const int *dimensions) {
    size_t total_input  = multiply_dimensions (rank, dimensions);
    size_t total_output = total_input / dimensions[rank-1] * (dimensions[rank-1] / 2 + 1);
    float *input = malloc (sizeof(float) * total_input);
    fftwf_complex *output = malloc (sizeof(fftwf_complex) * total_output);
    fftwf_plan plan = fftwf_plan_dft_r2c (rank, dimensions, input, output, FFTW_ESTIMATE);

    if (plan == NULL) {
        goto bad;
    }

    struct fftw3s_plan *res = malloc (sizeof(struct fftw3s_plan));
    res->plan      = plan;
    res->in        = input;
    res->out       = output;
    res->total_in  = total_input;
    res->total_out = total_output;

    return res;
bad:
    free (input);
    free (output);
    return NULL;
}

struct fftw3s_plan* fftwraps_create_irfft_plan (int rank, const int *dimensions) {
    size_t total_output  = multiply_dimensions (rank, dimensions);
    size_t total_input = total_output / dimensions[rank-1] * (dimensions[rank-1] / 2 + 1);
    fftwf_complex *input = malloc (sizeof(fftwf_complex) * total_input);
    float *output = malloc (sizeof(float) * total_output);
    fftwf_plan plan = fftwf_plan_dft_c2r (rank, dimensions, input, output, FFTW_ESTIMATE);

    if (plan == NULL) {
        goto bad;
    }

    struct fftw3s_plan *res = malloc (sizeof(struct fftw3s_plan));
    res->plan      = plan;
    res->in        = input;
    res->out       = output;
    res->total_in  = total_input;
    res->total_out = total_output;

    return res;
bad:
    free (input);
    free (output);
    return NULL;
}

void fftwraps_destroy_plan (struct fftw3s_plan* plan) {
    fftwf_destroy_plan (plan->plan);
    free (plan->in);
    free (plan->out);
    free (plan);
}

void* fftwraps_get_input_pointer (const struct fftw3s_plan* plan) {
    return plan->in;
}

void* fftwraps_get_output_pointer (const struct fftw3s_plan* plan) {
    return (plan->out != NULL)? plan->out: plan->in;
}

void fftwraps_execute_plan (const struct fftw3s_plan *plan) {
    fftwf_execute (plan->plan);
}
