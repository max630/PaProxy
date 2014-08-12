#include <pulse/scache.h>
#include <pulse/pulseaudio.h>
#include <pulse/proplist.h>
#include <pulse/stream.h>
#include <pulse/mainloop-signal.h>
#include <pulse/ext-device-restore.h>
#include <pulse/channelmap.h>
#include <pulse/xmalloc.h>
#include <pulse/simple.h>
#include <pulse/cdecl.h>
#include <pulse/ext-device-manager.h>
#include <pulse/thread-mainloop.h>
#include <pulse/operation.h>
#include <pulse/mainloop.h>
#include <pulse/error.h>
#include <pulse/timeval.h>
#include <pulse/subscribe.h>
#include <pulse/introspect.h>
#include <pulse/version.h>
#include <pulse/utf8.h>
#include <pulse/mainloop-api.h>
#include <pulse/format.h>
#include <pulse/context.h>
#include <pulse/util.h>
#include <pulse/sample.h>
#include <pulse/rtclock.h>
#include <pulse/volume.h>
#include <pulse/gccmacro.h>
#include <pulse/ext-stream-restore.h>
#include <pulse/def.h>

#include "default_macros.h"

char * pa_ascii_filter(const char * str)
{
    PAP_DEFAULT_NULL();
}
char * pa_ascii_valid(const char * str)
{
    PAP_DEFAULT_NULL();
}
size_t pa_bytes_per_second(const pa_sample_spec * spec)
{
    PAP_DEFAULT_ZERO();
}
char * pa_bytes_snprint(char * s, size_t l, unsigned v)
{
    PAP_DEFAULT_NULL();
}
pa_usec_t pa_bytes_to_usec(uint64_t length,
                           const pa_sample_spec * spec)
{
    PAP_DEFAULT_ZERO();
}
int pa_channel_map_can_balance(const pa_channel_map * map)
{
    PAP_DEFAULT_ZERO();
}
int pa_channel_map_can_fade(const pa_channel_map * map)
{
    PAP_DEFAULT_ZERO();
}
int pa_channel_map_equal(const pa_channel_map * a,
                         const pa_channel_map * b)
{
    PAP_DEFAULT_ZERO();
}
int pa_channel_map_has_position(const pa_channel_map * map,
                                pa_channel_position_t p)
{
    PAP_DEFAULT_ZERO();
}
pa_channel_map * pa_channel_map_init(pa_channel_map * m)
{
    PAP_DEFAULT_NULL();
}
pa_channel_map * pa_channel_map_init_auto(pa_channel_map * m,
                                          unsigned channels,
                                          pa_channel_map_def_t def)
{
    PAP_DEFAULT_NULL();
}
pa_channel_position_mask_t pa_channel_map_mask(const pa_channel_map * map)
{
    PAP_DEFAULT_CHANNEL_POSITION_MASK();
}
pa_channel_map * pa_channel_map_parse(pa_channel_map * map,
                                      const char * s)
{
    PAP_DEFAULT_NULL();
}
char * pa_channel_map_snprint(char * s,
                              size_t l,
                              const pa_channel_map * map)
{
    PAP_DEFAULT_NULL();
}
int pa_channel_map_superset(const pa_channel_map * a,
                            const pa_channel_map * b)
{
    PAP_DEFAULT_ZERO();
}
const char * pa_channel_map_to_name(const pa_channel_map * map)
{
    PAP_DEFAULT_NULL();
}
const char * pa_channel_map_to_pretty_name(const pa_channel_map * map)
{
    PAP_DEFAULT_NULL();
}
int pa_channel_map_valid(const pa_channel_map * map)
{
    PAP_DEFAULT_ZERO();
}
pa_channel_position_t pa_channel_position_from_string(const char * s)
{
    PAP_DEFAULT_CHANNEL_POSITION();
}
const char * pa_channel_position_to_pretty_string(pa_channel_position_t pos)
{
    PAP_DEFAULT_NULL();
}
const char * pa_channel_position_to_string(pa_channel_position_t pos)
{
    PAP_DEFAULT_NULL();
}
int pa_context_connect(pa_context * c,
                       const char * server,
                       pa_context_flags_t flags,
                       const pa_spawn_api * api)
{
    PAP_DEFAULT_ZERO();
}
void pa_context_disconnect(pa_context * c)
{
    PAP_DEFAULT_VOID();
}
pa_operation * pa_context_drain(pa_context * c,
                                pa_context_notify_cb_t cb,
                                void * userdata)
{
    PAP_DEFAULT_NULL();
}
int pa_context_errno(pa_context * c)
{
    PAP_DEFAULT_ZERO();
}
pa_operation * pa_context_exit_daemon(pa_context * c,
                                      pa_context_success_cb_t cb,
                                      void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_autoload_info_by_index(pa_context * c,
                                                     uint32_t idx,
                                                     pa_autoload_info_cb_t cb,
                                                     void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_autoload_info_by_name(pa_context * c,
                                                    const char * name,
                                                    pa_autoload_type_t type,
                                                    pa_autoload_info_cb_t cb,
                                                    void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_autoload_info_list(pa_context * c,
                                                 pa_autoload_info_cb_t cb,
                                                 void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_card_info_by_index(pa_context * c,
                                                 uint32_t idx,
                                                 pa_card_info_cb_t cb,
                                                 void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_card_info_by_name(pa_context * c,
                                                const char * name,
                                                pa_card_info_cb_t cb,
                                                void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_card_info_list(pa_context * c,
                                             pa_card_info_cb_t cb,
                                             void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_client_info(pa_context * c,
                                          uint32_t idx,
                                          pa_client_info_cb_t cb,
                                          void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_client_info_list(pa_context * c,
                                               pa_client_info_cb_t cb,
                                               void * userdata)
{
    PAP_DEFAULT_NULL();
}
uint32_t pa_context_get_index(pa_context * s)
{
    PAP_DEFAULT_ZERO();
}
pa_operation * pa_context_get_module_info(pa_context * c,
                                          uint32_t idx,
                                          pa_module_info_cb_t cb,
                                          void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_module_info_list(pa_context * c,
                                               pa_module_info_cb_t cb,
                                               void * userdata)
{
    PAP_DEFAULT_NULL();
}
uint32_t pa_context_get_protocol_version(pa_context * c)
{
    PAP_DEFAULT_ZERO();
}
pa_operation * pa_context_get_sample_info_by_index(pa_context * c,
                                                   uint32_t idx,
                                                   pa_sample_info_cb_t cb,
                                                   void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_sample_info_by_name(pa_context * c,
                                                  const char * name,
                                                  pa_sample_info_cb_t cb,
                                                  void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_sample_info_list(pa_context * c,
                                               pa_sample_info_cb_t cb,
                                               void * userdata)
{
    PAP_DEFAULT_NULL();
}
const char * pa_context_get_server(pa_context * c)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_server_info(pa_context * c,
                                          pa_server_info_cb_t cb,
                                          void * userdata)
{
    PAP_DEFAULT_NULL();
}
uint32_t pa_context_get_server_protocol_version(pa_context * c)
{
    PAP_DEFAULT_ZERO();
}
pa_operation * pa_context_get_sink_info_by_index(pa_context * c,
                                                 uint32_t idx,
                                                 pa_sink_info_cb_t cb,
                                                 void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_sink_info_by_name(pa_context * c,
                                                const char * name,
                                                pa_sink_info_cb_t cb,
                                                void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_sink_info_list(pa_context * c,
                                             pa_sink_info_cb_t cb,
                                             void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_sink_input_info(pa_context * c,
                                              uint32_t idx,
                                              pa_sink_input_info_cb_t cb,
                                              void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_sink_input_info_list(pa_context * c,
                                                   pa_sink_input_info_cb_t cb,
                                                   void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_source_info_by_index(pa_context * c,
                                                   uint32_t idx,
                                                   pa_source_info_cb_t cb,
                                                   void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_source_info_by_name(pa_context * c,
                                                  const char * name,
                                                  pa_source_info_cb_t cb,
                                                  void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_source_info_list(pa_context * c,
                                               pa_source_info_cb_t cb,
                                               void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_source_output_info(pa_context * c,
                                                 uint32_t idx,
                                                 pa_source_output_info_cb_t cb,
                                                 void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_get_source_output_info_list(pa_context * c,
                                                      pa_source_output_info_cb_t cb,
                                                      void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_context_state_t pa_context_get_state(pa_context * c)
{
    PAP_DEFAULT_CONTEXT_STATE();
}
size_t pa_context_get_tile_size(pa_context * c,
                                const pa_sample_spec * ss)
{
    PAP_DEFAULT_ZERO();
}
int pa_context_is_local(pa_context * c)
{
    PAP_DEFAULT_ZERO();
}
int pa_context_is_pending(pa_context * c)
{
    PAP_DEFAULT_ZERO();
}
pa_operation * pa_context_kill_client(pa_context * c,
                                      uint32_t idx,
                                      pa_context_success_cb_t cb,
                                      void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_kill_sink_input(pa_context * c,
                                          uint32_t idx,
                                          pa_context_success_cb_t cb,
                                          void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_kill_source_output(pa_context * c,
                                             uint32_t idx,
                                             pa_context_success_cb_t cb,
                                             void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_load_module(pa_context * c,
                                      const char * name,
                                      const char * argument,
                                      pa_context_index_cb_t cb,
                                      void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_move_sink_input_by_index(pa_context * c,
                                                   uint32_t idx,
                                                   uint32_t sink_idx,
                                                   pa_context_success_cb_t cb,
                                                   void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_move_sink_input_by_name(pa_context * c,
                                                  uint32_t idx,
                                                  const char * sink_name,
                                                  pa_context_success_cb_t cb,
                                                  void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_move_source_output_by_index(pa_context * c,
                                                      uint32_t idx,
                                                      uint32_t source_idx,
                                                      pa_context_success_cb_t cb,
                                                      void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_move_source_output_by_name(pa_context * c,
                                                     uint32_t idx,
                                                     const char * source_name,
                                                     pa_context_success_cb_t cb,
                                                     void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_context * pa_context_new(pa_mainloop_api * mainloop,
                            const char * name)
{
    PAP_DEFAULT_NULL();
}
pa_context * pa_context_new_with_proplist(pa_mainloop_api * mainloop,
                                          const char * name,
                                          pa_proplist * proplist)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_play_sample(pa_context * c,
                                      const char * name,
                                      const char * dev,
                                      pa_volume_t volume,
                                      pa_context_success_cb_t cb,
                                      void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_play_sample_with_proplist(pa_context * c,
                                                    const char * name,
                                                    const char * dev,
                                                    pa_volume_t volume,
                                                    pa_proplist * proplist,
                                                    pa_context_play_sample_cb_t cb,
                                                    void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_proplist_remove(pa_context * c,
                                          const char * const keys[],
                                          pa_context_success_cb_t cb,
                                          void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_proplist_update(pa_context * c,
                                          pa_update_mode_t mode,
                                          pa_proplist * p,
                                          pa_context_success_cb_t cb,
                                          void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_context * pa_context_ref(pa_context * c)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_remove_autoload_by_index(pa_context * c,
                                                   uint32_t idx,
                                                   pa_context_success_cb_t cb,
                                                   void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_remove_autoload_by_name(pa_context * c,
                                                  const char * name,
                                                  pa_autoload_type_t type,
                                                  pa_context_success_cb_t cb,
                                                  void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_remove_sample(pa_context * c,
                                        const char * name,
                                        pa_context_success_cb_t cb,
                                        void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_time_event * pa_context_rttime_new(pa_context * c,
                                      pa_usec_t usec,
                                      pa_time_event_cb_t cb,
                                      void * userdata)
{
    PAP_DEFAULT_NULL();
}
void pa_context_rttime_restart(pa_context * c,
                               pa_time_event * e,
                               pa_usec_t usec)
{
    PAP_DEFAULT_VOID();
}
pa_operation * pa_context_set_card_profile_by_index(pa_context * c,
                                                    uint32_t idx,
                                                    const char * profile,
                                                    pa_context_success_cb_t cb,
                                                    void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_card_profile_by_name(pa_context * c,
                                                   const char * name,
                                                   const char * profile,
                                                   pa_context_success_cb_t cb,
                                                   void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_default_sink(pa_context * c,
                                           const char * name,
                                           pa_context_success_cb_t cb,
                                           void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_default_source(pa_context * c,
                                             const char * name,
                                             pa_context_success_cb_t cb,
                                             void * userdata)
{
    PAP_DEFAULT_NULL();
}
void pa_context_set_event_callback(pa_context * p,
                                   pa_context_event_cb_t cb,
                                   void * userdata)
{
    PAP_DEFAULT_VOID();
}
pa_operation * pa_context_set_name(pa_context * c,
                                   const char * name,
                                   pa_context_success_cb_t cb,
                                   void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_sink_input_mute(pa_context * c,
                                              uint32_t idx,
                                              int mute,
                                              pa_context_success_cb_t cb,
                                              void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_sink_input_volume(pa_context * c,
                                                uint32_t idx,
                                                const pa_cvolume * volume,
                                                pa_context_success_cb_t cb,
                                                void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_sink_mute_by_index(pa_context * c,
                                                 uint32_t idx,
                                                 int mute,
                                                 pa_context_success_cb_t cb,
                                                 void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_sink_mute_by_name(pa_context * c,
                                                const char * name,
                                                int mute,
                                                pa_context_success_cb_t cb,
                                                void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_sink_port_by_index(pa_context * c,
                                                 uint32_t idx,
                                                 const char * port,
                                                 pa_context_success_cb_t cb,
                                                 void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_sink_port_by_name(pa_context * c,
                                                const char * name,
                                                const char * port,
                                                pa_context_success_cb_t cb,
                                                void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_sink_volume_by_index(pa_context * c,
                                                   uint32_t idx,
                                                   const pa_cvolume * volume,
                                                   pa_context_success_cb_t cb,
                                                   void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_sink_volume_by_name(pa_context * c,
                                                  const char * name,
                                                  const pa_cvolume * volume,
                                                  pa_context_success_cb_t cb,
                                                  void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_source_mute_by_index(pa_context * c,
                                                   uint32_t idx,
                                                   int mute,
                                                   pa_context_success_cb_t cb,
                                                   void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_source_mute_by_name(pa_context * c,
                                                  const char * name,
                                                  int mute,
                                                  pa_context_success_cb_t cb,
                                                  void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_source_output_mute(pa_context * c,
                                                 uint32_t idx,
                                                 int mute,
                                                 pa_context_success_cb_t cb,
                                                 void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_source_output_volume(pa_context * c,
                                                   uint32_t idx,
                                                   const pa_cvolume * volume,
                                                   pa_context_success_cb_t cb,
                                                   void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_source_port_by_index(pa_context * c,
                                                   uint32_t idx,
                                                   const char * port,
                                                   pa_context_success_cb_t cb,
                                                   void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_source_port_by_name(pa_context * c,
                                                  const char * name,
                                                  const char * port,
                                                  pa_context_success_cb_t cb,
                                                  void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_source_volume_by_index(pa_context * c,
                                                     uint32_t idx,
                                                     const pa_cvolume * volume,
                                                     pa_context_success_cb_t cb,
                                                     void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_set_source_volume_by_name(pa_context * c,
                                                    const char * name,
                                                    const pa_cvolume * volume,
                                                    pa_context_success_cb_t cb,
                                                    void * userdata)
{
    PAP_DEFAULT_NULL();
}
void pa_context_set_state_callback(pa_context * c,
                                   pa_context_notify_cb_t cb,
                                   void * userdata)
{
    PAP_DEFAULT_VOID();
}
void pa_context_set_subscribe_callback(pa_context * c,
                                       pa_context_subscribe_cb_t cb,
                                       void * userdata)
{
    PAP_DEFAULT_VOID();
}
pa_operation * pa_context_stat(pa_context * c,
                               pa_stat_info_cb_t cb,
                               void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_subscribe(pa_context * c,
                                    pa_subscription_mask_t m,
                                    pa_context_success_cb_t cb,
                                    void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_suspend_sink_by_index(pa_context * c,
                                                uint32_t idx,
                                                int suspend,
                                                pa_context_success_cb_t cb,
                                                void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_suspend_sink_by_name(pa_context * c,
                                               const char * sink_name,
                                               int suspend,
                                               pa_context_success_cb_t cb,
                                               void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_suspend_source_by_index(pa_context * c,
                                                  uint32_t idx,
                                                  int suspend,
                                                  pa_context_success_cb_t cb,
                                                  void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_suspend_source_by_name(pa_context * c,
                                                 const char * source_name,
                                                 int suspend,
                                                 pa_context_success_cb_t cb,
                                                 void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_context_unload_module(pa_context * c,
                                        uint32_t idx,
                                        pa_context_success_cb_t cb,
                                        void * userdata)
{
    PAP_DEFAULT_NULL();
}
void pa_context_unref(pa_context * c)
{
    PAP_DEFAULT_VOID();
}
pa_volume_t pa_cvolume_avg(const pa_cvolume * a)
{
    PAP_DEFAULT_VOLUME();
}
pa_volume_t pa_cvolume_avg_mask(const pa_cvolume * a,
                                const pa_channel_map * cm,
                                pa_channel_position_mask_t mask)
{
    PAP_DEFAULT_VOLUME();
}
int pa_cvolume_channels_equal_to(const pa_cvolume * a,
                                 pa_volume_t v)
{
    PAP_DEFAULT_ZERO();
}
int pa_cvolume_compatible(const pa_cvolume * v,
                          const pa_sample_spec * ss)
{
    PAP_DEFAULT_ZERO();
}
int pa_cvolume_compatible_with_channel_map(const pa_cvolume * v,
                                           const pa_channel_map * cm)
{
    PAP_DEFAULT_ZERO();
}
pa_cvolume * pa_cvolume_dec(pa_cvolume * v, pa_volume_t dec)
{
    PAP_DEFAULT_NULL();
}
int pa_cvolume_equal(const pa_cvolume * a, const pa_cvolume * b)
{
    PAP_DEFAULT_ZERO();
}
float pa_cvolume_get_balance(const pa_cvolume * v,
                             const pa_channel_map * map)
{
    PAP_DEFAULT_ZERO();
}
float pa_cvolume_get_fade(const pa_cvolume * v,
                          const pa_channel_map * map)
{
    PAP_DEFAULT_ZERO();
}
pa_volume_t pa_cvolume_get_position(pa_cvolume * cv,
                                    const pa_channel_map * map,
                                    pa_channel_position_t t)
{
    PAP_DEFAULT_VOLUME();
}
pa_cvolume * pa_cvolume_inc(pa_cvolume * v, pa_volume_t inc)
{
    PAP_DEFAULT_NULL();
}
pa_cvolume * pa_cvolume_inc_clamp(pa_cvolume * v,
                                  pa_volume_t inc,
                                  pa_volume_t limit)
{
    PAP_DEFAULT_NULL();
}
pa_cvolume * pa_cvolume_init(pa_cvolume * a)
{
    PAP_DEFAULT_NULL();
}
pa_volume_t pa_cvolume_max(const pa_cvolume * a)
{
    PAP_DEFAULT_VOLUME();
}
pa_volume_t pa_cvolume_max_mask(const pa_cvolume * a,
                                const pa_channel_map * cm,
                                pa_channel_position_mask_t mask)
{
    PAP_DEFAULT_VOLUME();
}
pa_cvolume * pa_cvolume_merge(pa_cvolume * dest,
                              const pa_cvolume * a,
                              const pa_cvolume * b)
{
    PAP_DEFAULT_NULL();
}
pa_volume_t pa_cvolume_min(const pa_cvolume * a)
{
    PAP_DEFAULT_VOLUME();
}
pa_volume_t pa_cvolume_min_mask(const pa_cvolume * a,
                                const pa_channel_map * cm,
                                pa_channel_position_mask_t mask)
{
    PAP_DEFAULT_VOLUME();
}
pa_cvolume * pa_cvolume_remap(pa_cvolume * v,
                              const pa_channel_map * from,
                              const pa_channel_map * to)
{
    PAP_DEFAULT_NULL();
}
pa_cvolume * pa_cvolume_scale(pa_cvolume * v, pa_volume_t max)
{
    PAP_DEFAULT_NULL();
}
pa_cvolume * pa_cvolume_scale_mask(pa_cvolume * v,
                                   pa_volume_t max,
                                   pa_channel_map * cm,
                                   pa_channel_position_mask_t mask)
{
    PAP_DEFAULT_NULL();
}
pa_cvolume * pa_cvolume_set(pa_cvolume * a,
                            unsigned channels,
                            pa_volume_t v)
{
    PAP_DEFAULT_NULL();
}
pa_cvolume * pa_cvolume_set_balance(pa_cvolume * v,
                                    const pa_channel_map * map,
                                    float new_balance)
{
    PAP_DEFAULT_NULL();
}
pa_cvolume * pa_cvolume_set_fade(pa_cvolume * v,
                                 const pa_channel_map * map,
                                 float new_fade)
{
    PAP_DEFAULT_NULL();
}
pa_cvolume * pa_cvolume_set_position(pa_cvolume * cv,
                                     const pa_channel_map * map,
                                     pa_channel_position_t t,
                                     pa_volume_t v)
{
    PAP_DEFAULT_NULL();
}
char * pa_cvolume_snprint(char * s, size_t l, const pa_cvolume * c)
{
    PAP_DEFAULT_NULL();
}
int pa_cvolume_valid(const pa_cvolume * v)
{
    PAP_DEFAULT_ZERO();
}
pa_encoding_t pa_encoding_from_string(const char * encoding)
{
    PAP_DEFAULT_ENCODING();
}
const char * pa_encoding_to_string(pa_encoding_t e)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_ext_device_manager_delete(pa_context * c,
                                            const char * const s[],
                                            pa_context_success_cb_t cb,
                                            void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_ext_device_manager_enable_role_device_priority_routing(pa_context * c,
                                                                         int enable,
                                                                         pa_context_success_cb_t cb,
                                                                         void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_ext_device_manager_read(pa_context * c,
                                          pa_ext_device_manager_read_cb_t cb,
                                          void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_ext_device_manager_reorder_devices_for_role(pa_context * c,
                                                              const char * role,
                                                              const char * * devices,
                                                              pa_context_success_cb_t cb,
                                                              void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_ext_device_manager_set_device_description(pa_context * c,
                                                            const char * device,
                                                            const char * description,
                                                            pa_context_success_cb_t cb,
                                                            void * userdata)
{
    PAP_DEFAULT_NULL();
}
void pa_ext_device_manager_set_subscribe_cb(pa_context * c,
                                            pa_ext_device_manager_subscribe_cb_t cb,
                                            void * userdata)
{
    PAP_DEFAULT_VOID();
}
pa_operation * pa_ext_device_manager_subscribe(pa_context * c,
                                               int enable,
                                               pa_context_success_cb_t cb,
                                               void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_ext_device_manager_test(pa_context * c,
                                          pa_ext_device_manager_test_cb_t cb,
                                          void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_ext_device_restore_read_formats(pa_context * c,
                                                  pa_device_type_t type,
                                                  uint32_t idx,
                                                  pa_ext_device_restore_read_device_formats_cb_t cb,
                                                  void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_ext_device_restore_read_formats_all(pa_context * c,
                                                      pa_ext_device_restore_read_device_formats_cb_t cb,
                                                      void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_ext_device_restore_save_formats(pa_context * c,
                                                  pa_device_type_t type,
                                                  uint32_t idx,
                                                  uint8_t n_formats,
                                                  pa_format_info * * formats,
                                                  pa_context_success_cb_t cb,
                                                  void * userdata)
{
    PAP_DEFAULT_NULL();
}
void pa_ext_device_restore_set_subscribe_cb(pa_context * c,
                                            pa_ext_device_restore_subscribe_cb_t cb,
                                            void * userdata)
{
    PAP_DEFAULT_VOID();
}
pa_operation * pa_ext_device_restore_subscribe(pa_context * c,
                                               int enable,
                                               pa_context_success_cb_t cb,
                                               void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_ext_device_restore_test(pa_context * c,
                                          pa_ext_device_restore_test_cb_t cb,
                                          void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_ext_stream_restore_delete(pa_context * c,
                                            const char * const s[],
                                            pa_context_success_cb_t cb,
                                            void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_ext_stream_restore_read(pa_context * c,
                                          pa_ext_stream_restore_read_cb_t cb,
                                          void * userdata)
{
    PAP_DEFAULT_NULL();
}
void pa_ext_stream_restore_set_subscribe_cb(pa_context * c,
                                            pa_ext_stream_restore_subscribe_cb_t cb,
                                            void * userdata)
{
    PAP_DEFAULT_VOID();
}
pa_operation * pa_ext_stream_restore_subscribe(pa_context * c,
                                               int enable,
                                               pa_context_success_cb_t cb,
                                               void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_ext_stream_restore_test(pa_context * c,
                                          pa_ext_stream_restore_test_cb_t cb,
                                          void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_ext_stream_restore_write(pa_context * c,
                                           pa_update_mode_t mode,
                                           const pa_ext_stream_restore_info data[],
                                           unsigned n,
                                           int apply_immediately,
                                           pa_context_success_cb_t cb,
                                           void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_format_info * pa_format_info_copy(const pa_format_info * src)
{
    PAP_DEFAULT_NULL();
}
void pa_format_info_free(pa_format_info * f)
{
    PAP_DEFAULT_VOID();
}
void pa_format_info_free_string_array(char * * values,
                                      int n_values)
{
    PAP_DEFAULT_VOID();
}
pa_format_info * pa_format_info_from_sample_spec(pa_sample_spec * ss,
                                                 pa_channel_map * map)
{
    PAP_DEFAULT_NULL();
}
pa_format_info * pa_format_info_from_string(const char * str)
{
    PAP_DEFAULT_NULL();
}
int pa_format_info_get_prop_int(pa_format_info * f,
                                const char * key,
                                int * v)
{
    PAP_DEFAULT_ZERO();
}
int pa_format_info_get_prop_int_array(pa_format_info * f,
                                      const char * key,
                                      int * * values,
                                      int * n_values)
{
    PAP_DEFAULT_ZERO();
}
int pa_format_info_get_prop_int_range(pa_format_info * f,
                                      const char * key,
                                      int * min,
                                      int * max)
{
    PAP_DEFAULT_ZERO();
}
int pa_format_info_get_prop_string(pa_format_info * f,
                                   const char * key,
                                   char * * v)
{
    PAP_DEFAULT_ZERO();
}
int pa_format_info_get_prop_string_array(pa_format_info * f,
                                         const char * key,
                                         char * * * values,
                                         int * n_values)
{
    PAP_DEFAULT_ZERO();
}
pa_prop_type_t pa_format_info_get_prop_type(pa_format_info * f,
                                            const char * key)
{
    PAP_DEFAULT_PROP_TYPE();
}
int pa_format_info_is_compatible(pa_format_info * first,
                                 pa_format_info * second)
{
    PAP_DEFAULT_ZERO();
}
int pa_format_info_is_pcm(const pa_format_info * f)
{
    PAP_DEFAULT_ZERO();
}
pa_format_info * pa_format_info_new(void)
{
    PAP_DEFAULT_NULL();
}
void pa_format_info_set_channel_map(pa_format_info * f,
                                    const pa_channel_map * map)
{
    PAP_DEFAULT_VOID();
}
void pa_format_info_set_channels(pa_format_info * f, int channels)
{
    PAP_DEFAULT_VOID();
}
void pa_format_info_set_prop_int(pa_format_info * f,
                                 const char * key,
                                 int value)
{
    PAP_DEFAULT_VOID();
}
void pa_format_info_set_prop_int_array(pa_format_info * f,
                                       const char * key,
                                       const int * values,
                                       int n_values)
{
    PAP_DEFAULT_VOID();
}
void pa_format_info_set_prop_int_range(pa_format_info * f,
                                       const char * key,
                                       int min,
                                       int max)
{
    PAP_DEFAULT_VOID();
}
void pa_format_info_set_prop_string(pa_format_info * f,
                                    const char * key,
                                    const char * value)
{
    PAP_DEFAULT_VOID();
}
void pa_format_info_set_prop_string_array(pa_format_info * f,
                                          const char * key,
                                          const char * * values,
                                          int n_values)
{
    PAP_DEFAULT_VOID();
}
void pa_format_info_set_rate(pa_format_info * f, int rate)
{
    PAP_DEFAULT_VOID();
}
void pa_format_info_set_sample_format(pa_format_info * f,
                                      pa_sample_format_t sf)
{
    PAP_DEFAULT_VOID();
}
char * pa_format_info_snprint(char * s,
                              size_t l,
                              const pa_format_info * f)
{
    PAP_DEFAULT_NULL();
}
int pa_format_info_to_sample_spec(pa_format_info * f,
                                  pa_sample_spec * ss,
                                  pa_channel_map * map)
{
    PAP_DEFAULT_ZERO();
}
int pa_format_info_valid(const pa_format_info * f)
{
    PAP_DEFAULT_ZERO();
}
size_t pa_frame_size(const pa_sample_spec * spec)
{
    PAP_DEFAULT_ZERO();
}
char * pa_get_fqdn(char * s, size_t l)
{
    PAP_DEFAULT_NULL();
}
char * pa_get_home_dir(char * s, size_t l)
{
    PAP_DEFAULT_NULL();
}
char * pa_get_host_name(char * s, size_t l)
{
    PAP_DEFAULT_NULL();
}
char * pa_get_user_name(char * s, size_t l)
{
    PAP_DEFAULT_NULL();
}
struct timeval * pa_gettimeofday(struct timeval * tv)
{
    PAP_DEFAULT_NULL();
}
char * pa_locale_to_utf8(const char * str)
{
    PAP_DEFAULT_NULL();
}
void pa_mainloop_api_once(pa_mainloop_api * m,
                          void (* callback)(pa_mainloop_api * m, void * userdata),
                          void * userdata)
{
    PAP_DEFAULT_VOID();
}
int pa_mainloop_dispatch(pa_mainloop * m)
{
    PAP_DEFAULT_ZERO();
}
void pa_mainloop_free(pa_mainloop * m)
{
    PAP_DEFAULT_VOID();
}
pa_mainloop_api * pa_mainloop_get_api(pa_mainloop * m)
{
    PAP_DEFAULT_NULL();
}
int pa_mainloop_get_retval(pa_mainloop * m)
{
    PAP_DEFAULT_ZERO();
}
int pa_mainloop_iterate(pa_mainloop * m, int block, int * retval)
{
    PAP_DEFAULT_ZERO();
}
pa_mainloop * pa_mainloop_new(void)
{
    PAP_DEFAULT_NULL();
}
int pa_mainloop_poll(pa_mainloop * m)
{
    PAP_DEFAULT_ZERO();
}
int pa_mainloop_prepare(pa_mainloop * m, int timeout)
{
    PAP_DEFAULT_ZERO();
}
void pa_mainloop_quit(pa_mainloop * m, int r)
{
    PAP_DEFAULT_VOID();
}
int pa_mainloop_run(pa_mainloop * m, int * retval)
{
    PAP_DEFAULT_ZERO();
}
void pa_mainloop_set_poll_func(pa_mainloop * m,
                               pa_poll_func poll_func,
                               void * userdata)
{
    PAP_DEFAULT_VOID();
}
void pa_mainloop_wakeup(pa_mainloop * m)
{
    PAP_DEFAULT_VOID();
}
int pa_msleep(unsigned long t)
{
    PAP_DEFAULT_ZERO();
}
void pa_operation_cancel(pa_operation * o)
{
    PAP_DEFAULT_VOID();
}
pa_operation_state_t pa_operation_get_state(pa_operation * o)
{
    PAP_DEFAULT_OPERATION_STATE();
}
pa_operation * pa_operation_ref(pa_operation * o)
{
    PAP_DEFAULT_NULL();
}
void pa_operation_unref(pa_operation * o)
{
    PAP_DEFAULT_VOID();
}
pa_sample_format_t pa_parse_sample_format(const char * format)
{
    PAP_DEFAULT_SAMPLE_FORMAT();
}
void pa_proplist_clear(pa_proplist * p)
{
    PAP_DEFAULT_VOID();
}
int pa_proplist_contains(pa_proplist * p, const char * key)
{
    PAP_DEFAULT_ZERO();
}
pa_proplist * pa_proplist_copy(const pa_proplist * p)
{
    PAP_DEFAULT_NULL();
}
int pa_proplist_equal(pa_proplist * a, pa_proplist * b)
{
    PAP_DEFAULT_ZERO();
}
pa_proplist * pa_proplist_from_string(const char * str)
{
    PAP_DEFAULT_NULL();
}
int pa_proplist_get(pa_proplist * p,
                    const char * key,
                    const void * * data,
                    size_t * nbytes)
{
    PAP_DEFAULT_ZERO();
}
const char * pa_proplist_gets(pa_proplist * p, const char * key)
{
    PAP_DEFAULT_NULL();
}
int pa_proplist_isempty(pa_proplist * p)
{
    PAP_DEFAULT_ZERO();
}
const char * pa_proplist_iterate(pa_proplist * p, void * * state)
{
    PAP_DEFAULT_NULL();
}
int pa_proplist_set(pa_proplist * p,
                    const char * key,
                    const void * data,
                    size_t nbytes)
{
    PAP_DEFAULT_ZERO();
}
int pa_proplist_setf(pa_proplist * p,
                     const char * key,
                     const char * format, ...)
{
    PAP_DEFAULT_ZERO();
}
int pa_proplist_setp(pa_proplist * p, const char * pair)
{
    PAP_DEFAULT_ZERO();
}
unsigned pa_proplist_size(pa_proplist * p)
{
    PAP_DEFAULT_ZERO();
}
char * pa_proplist_to_string(pa_proplist * p)
{
    PAP_DEFAULT_NULL();
}
char * pa_proplist_to_string_sep(pa_proplist * p, const char * sep)
{
    PAP_DEFAULT_NULL();
}
int pa_proplist_unset(pa_proplist * p, const char * key)
{
    PAP_DEFAULT_ZERO();
}
int pa_proplist_unset_many(pa_proplist * p,
                           const char * const keys[])
{
    PAP_DEFAULT_ZERO();
}
void pa_proplist_update(pa_proplist * p,
                        pa_update_mode_t mode,
                        const pa_proplist * other)
{
    PAP_DEFAULT_VOID();
}
pa_usec_t pa_rtclock_now(void)
{
    PAP_DEFAULT_ZERO();
}
int pa_sample_format_is_be(pa_sample_format_t f)
{
    PAP_DEFAULT_ZERO();
}
int pa_sample_format_is_le(pa_sample_format_t f)
{
    PAP_DEFAULT_ZERO();
}
const char * pa_sample_format_to_string(pa_sample_format_t f)
{
    PAP_DEFAULT_NULL();
}
size_t pa_sample_size(const pa_sample_spec * spec)
{
    PAP_DEFAULT_ZERO();
}
size_t pa_sample_size_of_format(pa_sample_format_t f)
{
    PAP_DEFAULT_ZERO();
}
int pa_sample_spec_equal(const pa_sample_spec * a,
                         const pa_sample_spec * b)
{
    PAP_DEFAULT_ZERO();
}
pa_sample_spec * pa_sample_spec_init(pa_sample_spec * spec)
{
    PAP_DEFAULT_NULL();
}
char * pa_sample_spec_snprint(char * s,
                              size_t l,
                              const pa_sample_spec * spec)
{
    PAP_DEFAULT_NULL();
}
void pa_signal_done(void)
{
    PAP_DEFAULT_VOID();
}
void pa_signal_free(pa_signal_event * e)
{
    PAP_DEFAULT_VOID();
}
int pa_signal_init(pa_mainloop_api * api)
{
    PAP_DEFAULT_ZERO();
}
pa_signal_event * pa_signal_new(int sig,
                                pa_signal_cb_t callback,
                                void * userdata)
{
    PAP_DEFAULT_NULL();
}
void pa_signal_set_destroy(pa_signal_event * e,
                           pa_signal_destroy_cb_t callback)
{
    PAP_DEFAULT_VOID();
}
int pa_stream_begin_write(pa_stream * p,
                          void * * data,
                          size_t * nbytes)
{
    PAP_DEFAULT_ZERO();
}
int pa_stream_cancel_write(pa_stream * p)
{
    PAP_DEFAULT_ZERO();
}
int pa_stream_connect_playback(pa_stream * s,
                               const char * dev,
                               const pa_buffer_attr * attr,
                               pa_stream_flags_t flags,
                               const pa_cvolume * volume,
                               pa_stream * sync_stream)
{
    PAP_DEFAULT_ZERO();
}
int pa_stream_connect_record(pa_stream * s,
                             const char * dev,
                             const pa_buffer_attr * attr,
                             pa_stream_flags_t flags)
{
    PAP_DEFAULT_ZERO();
}
int pa_stream_connect_upload(pa_stream * s, size_t length)
{
    PAP_DEFAULT_ZERO();
}
pa_operation * pa_stream_cork(pa_stream * s,
                              int b,
                              pa_stream_success_cb_t cb,
                              void * userdata)
{
    PAP_DEFAULT_NULL();
}
int pa_stream_disconnect(pa_stream * s)
{
    PAP_DEFAULT_ZERO();
}
pa_operation * pa_stream_drain(pa_stream * s,
                               pa_stream_success_cb_t cb,
                               void * userdata)
{
    PAP_DEFAULT_NULL();
}
int pa_stream_drop(pa_stream * p)
{
    PAP_DEFAULT_ZERO();
}
int pa_stream_finish_upload(pa_stream * s)
{
    PAP_DEFAULT_ZERO();
}
pa_operation * pa_stream_flush(pa_stream * s,
                               pa_stream_success_cb_t cb,
                               void * userdata)
{
    PAP_DEFAULT_NULL();
}
const pa_buffer_attr * pa_stream_get_buffer_attr(pa_stream * s)
{
    PAP_DEFAULT_NULL();
}
const pa_channel_map * pa_stream_get_channel_map(pa_stream * s)
{
    PAP_DEFAULT_NULL();
}
pa_context * pa_stream_get_context(pa_stream * p)
{
    PAP_DEFAULT_NULL();
}
uint32_t pa_stream_get_device_index(pa_stream * s)
{
    PAP_DEFAULT_ZERO();
}
const char * pa_stream_get_device_name(pa_stream * s)
{
    PAP_DEFAULT_NULL();
}
const pa_format_info * pa_stream_get_format_info(pa_stream * s)
{
    PAP_DEFAULT_NULL();
}
uint32_t pa_stream_get_index(pa_stream * s)
{
    PAP_DEFAULT_ZERO();
}
int pa_stream_get_latency(pa_stream * s,
                          pa_usec_t * r_usec,
                          int * negative)
{
    PAP_DEFAULT_ZERO();
}
uint32_t pa_stream_get_monitor_stream(pa_stream * s)
{
    PAP_DEFAULT_ZERO();
}
const pa_sample_spec * pa_stream_get_sample_spec(pa_stream * s)
{
    PAP_DEFAULT_NULL();
}
pa_stream_state_t pa_stream_get_state(pa_stream * p)
{
    PAP_DEFAULT_STREAM_STATE();
}
int pa_stream_get_time(pa_stream * s, pa_usec_t * r_usec)
{
    PAP_DEFAULT_ZERO();
}
const pa_timing_info * pa_stream_get_timing_info(pa_stream * s)
{
    PAP_DEFAULT_NULL();
}
int64_t pa_stream_get_underflow_index(pa_stream * p)
{
    PAP_DEFAULT_ZERO();
}
int pa_stream_is_corked(pa_stream * s)
{
    PAP_DEFAULT_ZERO();
}
int pa_stream_is_suspended(pa_stream * s)
{
    PAP_DEFAULT_ZERO();
}
pa_stream * pa_stream_new(pa_context * c,
                          const char * name,
                          const pa_sample_spec * ss,
                          const pa_channel_map * map)
{
    PAP_DEFAULT_NULL();
}
pa_stream * pa_stream_new_extended(pa_context * c,
                                   const char * name,
                                   pa_format_info * const * formats,
                                   unsigned int n_formats,
                                   pa_proplist * p)
{
    PAP_DEFAULT_NULL();
}
pa_stream * pa_stream_new_with_proplist(pa_context * c,
                                        const char * name,
                                        const pa_sample_spec * ss,
                                        const pa_channel_map * map,
                                        pa_proplist * p)
{
    PAP_DEFAULT_NULL();
}
int pa_stream_peek(pa_stream * p,
                   const void * * data,
                   size_t * nbytes)
{
    PAP_DEFAULT_ZERO();
}
pa_operation * pa_stream_prebuf(pa_stream * s,
                                pa_stream_success_cb_t cb,
                                void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_stream_proplist_remove(pa_stream * s,
                                         const char * const keys[],
                                         pa_stream_success_cb_t cb,
                                         void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_stream_proplist_update(pa_stream * s,
                                         pa_update_mode_t mode,
                                         pa_proplist * p,
                                         pa_stream_success_cb_t cb,
                                         void * userdata)
{
    PAP_DEFAULT_NULL();
}
size_t pa_stream_readable_size(pa_stream * p)
{
    PAP_DEFAULT_ZERO();
}
pa_stream * pa_stream_ref(pa_stream * s)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_stream_set_buffer_attr(pa_stream * s,
                                         const pa_buffer_attr * attr,
                                         pa_stream_success_cb_t cb,
                                         void * userdata)
{
    PAP_DEFAULT_NULL();
}
void pa_stream_set_buffer_attr_callback(pa_stream * p,
                                        pa_stream_notify_cb_t cb,
                                        void * userdata)
{
    PAP_DEFAULT_VOID();
}
void pa_stream_set_event_callback(pa_stream * p,
                                  pa_stream_event_cb_t cb,
                                  void * userdata)
{
    PAP_DEFAULT_VOID();
}
void pa_stream_set_latency_update_callback(pa_stream * p,
                                           pa_stream_notify_cb_t cb,
                                           void * userdata)
{
    PAP_DEFAULT_VOID();
}
int pa_stream_set_monitor_stream(pa_stream * s,
                                 uint32_t sink_input_idx)
{
    PAP_DEFAULT_ZERO();
}
void pa_stream_set_moved_callback(pa_stream * p,
                                  pa_stream_notify_cb_t cb,
                                  void * userdata)
{
    PAP_DEFAULT_VOID();
}
pa_operation * pa_stream_set_name(pa_stream * s,
                                  const char * name,
                                  pa_stream_success_cb_t cb,
                                  void * userdata)
{
    PAP_DEFAULT_NULL();
}
void pa_stream_set_overflow_callback(pa_stream * p,
                                     pa_stream_notify_cb_t cb,
                                     void * userdata)
{
    PAP_DEFAULT_VOID();
}
void pa_stream_set_read_callback(pa_stream * p,
                                 pa_stream_request_cb_t cb,
                                 void * userdata)
{
    PAP_DEFAULT_VOID();
}
void pa_stream_set_started_callback(pa_stream * p,
                                    pa_stream_notify_cb_t cb,
                                    void * userdata)
{
    PAP_DEFAULT_VOID();
}
void pa_stream_set_state_callback(pa_stream * s,
                                  pa_stream_notify_cb_t cb,
                                  void * userdata)
{
    PAP_DEFAULT_VOID();
}
void pa_stream_set_suspended_callback(pa_stream * p,
                                      pa_stream_notify_cb_t cb,
                                      void * userdata)
{
    PAP_DEFAULT_VOID();
}
void pa_stream_set_underflow_callback(pa_stream * p,
                                      pa_stream_notify_cb_t cb,
                                      void * userdata)
{
    PAP_DEFAULT_VOID();
}
void pa_stream_set_write_callback(pa_stream * p,
                                  pa_stream_request_cb_t cb,
                                  void * userdata)
{
    PAP_DEFAULT_VOID();
}
pa_operation * pa_stream_trigger(pa_stream * s,
                                 pa_stream_success_cb_t cb,
                                 void * userdata)
{
    PAP_DEFAULT_NULL();
}
void pa_stream_unref(pa_stream * s)
{
    PAP_DEFAULT_VOID();
}
pa_operation * pa_stream_update_sample_rate(pa_stream * s,
                                            uint32_t rate,
                                            pa_stream_success_cb_t cb,
                                            void * userdata)
{
    PAP_DEFAULT_NULL();
}
pa_operation * pa_stream_update_timing_info(pa_stream * p,
                                            pa_stream_success_cb_t cb,
                                            void * userdata)
{
    PAP_DEFAULT_NULL();
}
size_t pa_stream_writable_size(pa_stream * p)
{
    PAP_DEFAULT_ZERO();
}
int pa_stream_write(pa_stream * p,
                    const void * data,
                    size_t nbytes,
                    pa_free_cb_t free_cb,
                    int64_t offset,
                    pa_seek_mode_t seek)
{
    PAP_DEFAULT_ZERO();
}
const char * pa_strerror(int error)
{
    PAP_DEFAULT_NULL();
}
pa_cvolume * pa_sw_cvolume_divide(pa_cvolume * dest,
                                  const pa_cvolume * a,
                                  const pa_cvolume * b)
{
    PAP_DEFAULT_NULL();
}
pa_cvolume * pa_sw_cvolume_divide_scalar(pa_cvolume * dest,
                                         const pa_cvolume * a,
                                         pa_volume_t b)
{
    PAP_DEFAULT_NULL();
}
pa_cvolume * pa_sw_cvolume_multiply(pa_cvolume * dest,
                                    const pa_cvolume * a,
                                    const pa_cvolume * b)
{
    PAP_DEFAULT_NULL();
}
pa_cvolume * pa_sw_cvolume_multiply_scalar(pa_cvolume * dest,
                                           const pa_cvolume * a,
                                           pa_volume_t b)
{
    PAP_DEFAULT_NULL();
}
char * pa_sw_cvolume_snprint_dB(char * s,
                                size_t l,
                                const pa_cvolume * c)
{
    PAP_DEFAULT_NULL();
}
pa_volume_t pa_sw_volume_divide(pa_volume_t a, pa_volume_t b)
{
    PAP_DEFAULT_VOLUME();
}
pa_volume_t pa_sw_volume_from_dB(double f)
{
    PAP_DEFAULT_VOLUME();
}
pa_volume_t pa_sw_volume_from_linear(double v)
{
    PAP_DEFAULT_VOLUME();
}
pa_volume_t pa_sw_volume_multiply(pa_volume_t a, pa_volume_t b)
{
    PAP_DEFAULT_VOLUME();
}
char * pa_sw_volume_snprint_dB(char * s, size_t l, pa_volume_t v)
{
    PAP_DEFAULT_NULL();
}
double pa_sw_volume_to_dB(pa_volume_t v)
{
    PAP_DEFAULT_ZERO();
}
double pa_sw_volume_to_linear(pa_volume_t v)
{
    PAP_DEFAULT_ZERO();
}
void pa_threaded_mainloop_accept(pa_threaded_mainloop * m)
{
    PAP_DEFAULT_VOID();
}
void pa_threaded_mainloop_free(pa_threaded_mainloop * m)
{
    PAP_DEFAULT_VOID();
}
pa_mainloop_api * pa_threaded_mainloop_get_api(pa_threaded_mainloop * m)
{
    PAP_DEFAULT_NULL();
}
int pa_threaded_mainloop_get_retval(pa_threaded_mainloop * m)
{
    PAP_DEFAULT_ZERO();
}
int pa_threaded_mainloop_in_thread(pa_threaded_mainloop * m)
{
    PAP_DEFAULT_ZERO();
}
void pa_threaded_mainloop_lock(pa_threaded_mainloop * m)
{
    PAP_DEFAULT_VOID();
}
pa_threaded_mainloop * pa_threaded_mainloop_new(void)
{
    PAP_DEFAULT_NULL();
}
void pa_threaded_mainloop_signal(pa_threaded_mainloop * m,
                                 int wait_for_accept)
{
    PAP_DEFAULT_VOID();
}
int pa_threaded_mainloop_start(pa_threaded_mainloop * m)
{
    PAP_DEFAULT_ZERO();
}
void pa_threaded_mainloop_stop(pa_threaded_mainloop * m)
{
    PAP_DEFAULT_VOID();
}
void pa_threaded_mainloop_unlock(pa_threaded_mainloop * m)
{
    PAP_DEFAULT_VOID();
}
void pa_threaded_mainloop_wait(pa_threaded_mainloop * m)
{
    PAP_DEFAULT_VOID();
}
struct timeval * pa_timeval_add(struct timeval * tv, pa_usec_t v)
{
    PAP_DEFAULT_NULL();
}
pa_usec_t pa_timeval_age(const struct timeval * tv)
{
    PAP_DEFAULT_ZERO();
}
int pa_timeval_cmp(const struct timeval * a,
                   const struct timeval * b)
{
    PAP_DEFAULT_ZERO();
}
pa_usec_t pa_timeval_diff(const struct timeval * a,
                          const struct timeval * b)
{
    PAP_DEFAULT_ZERO();
}
pa_usec_t pa_timeval_load(const struct timeval * tv)
{
    PAP_DEFAULT_ZERO();
}
struct timeval * pa_timeval_store(struct timeval * tv, pa_usec_t v)
{
    PAP_DEFAULT_NULL();
}
struct timeval * pa_timeval_sub(struct timeval * tv, pa_usec_t v)
{
    PAP_DEFAULT_NULL();
}
size_t pa_usec_to_bytes(pa_usec_t t, const pa_sample_spec * spec)
{
    PAP_DEFAULT_ZERO();
}
char * pa_utf8_to_locale(const char * str)
{
    PAP_DEFAULT_NULL();
}
char * pa_volume_snprint(char * s, size_t l, pa_volume_t v)
{
    PAP_DEFAULT_NULL();
}
