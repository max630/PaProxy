#include <pulse/sample.h>

#include <assert.h>

int pa_sample_spec_valid(const pa_sample_spec *spec)
{
    assert(spec);
    return (spec->format != PA_SAMPLE_INVALID && spec->channels > 0 && spec->rate > 0);
}
