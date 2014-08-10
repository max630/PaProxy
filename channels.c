#include <pulse/channelmap.h>

#include <assert.h>
#include <string.h>

pa_channel_map* pa_channel_map_init_extend(pa_channel_map *m, unsigned channels, pa_channel_map_def_t def)
{
    assert(channels);

    switch (def) {
    case PA_CHANNEL_MAP_DEFAULT:
        if (channels == 1) {
            return pa_channel_map_init_mono(m);
        } else if (channels == 2) {
            return pa_channel_map_init_stereo(m);
        }
        break;
    default:
        return pa_channel_map_init_mono(m);
        break;
    }
}

pa_channel_map* pa_channel_map_init_mono(pa_channel_map *m)
{
    assert(m);
    memset(m, 0, sizeof(pa_channel_map));
    m->channels = 1;
    m->map[0] = PA_CHANNEL_POSITION_MONO;
    return m;
}

pa_channel_map* pa_channel_map_init_stereo(pa_channel_map *m)
{
    assert(m);
    memset(m, 0, sizeof(pa_channel_map));
    m->channels = 2;
    m->map[0] = PA_CHANNEL_POSITION_FRONT_LEFT;
    m->map[1] = PA_CHANNEL_POSITION_FRONT_RIGHT;
    return m;
}

int pa_channel_map_compatible(const pa_channel_map *map, const pa_sample_spec *ss)
{
    return (map->channels >= ss->channels);
}
