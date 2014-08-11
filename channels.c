/*
PaProxy, re-implementation of pulseaudio client API over other backend
Copyright (C) 2014 Max Kirillov

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
*/

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
