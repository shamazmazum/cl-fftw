#include <fftw3.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "libfftwrap.h"

struct fftw3_plan {
    fftw_plan plan;
    void     *in;
    void     *out;
    size_t    total_in;
    size_t    total_out;
};

static size_t multiply_dimensions (int rank, const int *dimensions) {
    size_t total = 1;
    for (int i = 0; i < rank; i++) {
        total *= dimensions[i];
    }
    return total;
}

struct fftw3_plan* create_fft_plan (int rank, const int *dimensions, int sign) {
    size_t total = multiply_dimensions (rank, dimensions);
    fftw_complex *array  = malloc (sizeof(fftw_complex) * total);
    fftw_plan plan = fftw_plan_dft (rank, dimensions, array, array, sign, FFTW_ESTIMATE);

    if (plan == NULL) {
        goto bad;
    }

    struct fftw3_plan *res = malloc (sizeof(struct fftw3_plan));
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

struct fftw3_plan* create_rfft_plan (int rank, const int *dimensions) {
    size_t total_input  = multiply_dimensions (rank, dimensions);
    size_t total_output = total_input / dimensions[rank-1] * (dimensions[rank-1] / 2 + 1);
    double *input = malloc (sizeof(double) * total_input);
    fftw_complex *output = malloc (sizeof(fftw_complex) * total_output);
    fftw_plan plan = fftw_plan_dft_r2c (rank, dimensions, input, output, FFTW_ESTIMATE);

    if (plan == NULL) {
        goto bad;
    }

    struct fftw3_plan *res = malloc (sizeof(struct fftw3_plan));
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

struct fftw3_plan* create_irfft_plan (int rank, const int *dimensions) {
    size_t total_output  = multiply_dimensions (rank, dimensions);
    size_t total_input = total_output / dimensions[rank-1] * (dimensions[rank-1] / 2 + 1);
    fftw_complex *input = malloc (sizeof(fftw_complex) * total_input);
    double *output = malloc (sizeof(double) * total_output);
    fftw_plan plan = fftw_plan_dft_c2r (rank, dimensions, input, output, FFTW_ESTIMATE);

    if (plan == NULL) {
        goto bad;
    }

    struct fftw3_plan *res = malloc (sizeof(struct fftw3_plan));
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

void destroy_plan (struct fftw3_plan* plan) {
    fftw_destroy_plan (plan->plan);
    free (plan->in);
    free (plan->out);
    free (plan);
}

void fft (struct fftw3_plan* plan, const fftw_complex *in, fftw_complex *out) {
    assert (plan->out == NULL && plan->total_out == 0);

    memcpy (plan->in, in, sizeof(fftw_complex) * plan->total_in);
    fftw_execute (plan->plan);
    memcpy (out, plan->in, sizeof(fftw_complex) * plan->total_in);
}

void rfft (struct fftw3_plan* plan, const double *in, fftw_complex *out) {
    memcpy (plan->in, in, sizeof(double) * plan->total_in);
    fftw_execute (plan->plan);
    memcpy (out, plan->out, sizeof(fftw_complex) * plan->total_out);
}

void irfft (struct fftw3_plan* plan, const fftw_complex *in, double *out) {
    memcpy (plan->in, in, sizeof(fftw_complex) * plan->total_in);
    fftw_execute (plan->plan);
    memcpy (out, plan->out, sizeof(double) * plan->total_out);
}
