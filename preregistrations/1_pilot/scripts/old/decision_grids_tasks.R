# Change Detection Task
decision_grid_change <- expand_grid(
  ex_arb_low_res              = c("meta_resolution_height > 0",            "meta_resolution_height > 700"),        
  ex_arb_no_resize            = c("scale_factor > 0",                      "round(scale_factor, 4) != '0.3081'"),
  ex_arb_no_fullscreen        = c("fullscreenenter %in% c(0,1)",           "fullscreenenter == 1"),
  ex_arb_exit_fullscreen      = c("fullscreenexit %in% c(0,1)",            "fullscreenexit == 0"),
  ex_arb_event_during_change  = c("is.logical(event_during_change)",       "event_during_change == TRUE"),
  ex_arb_captcha              = c("meta_captcha > 0",                      "meta_captcha > 0.4"),
  ex_arb_interrupt            = c("attention_interrupt_sum %in% c(0,1,2)", "attention_interrupt_sum < 2"),
  ex_arb_noise                = c("att_noise %in% c(0,1,2,3,4)",           "att_noise %in% c(0,1,2)")
)

decision_grid_change_key <- tribble(
  ~variable,                      ~variable_group,      ~exp,                                    ~name,
  "ex_arb_low_res",               "resolution",         "meta_resolution_height > 0",            "Resolution too low",
  "ex_arb_low_res",               "resolution",         "meta_resolution_height > 700",          "All resolutions",
  "ex_arb_no_resize",             "resize",             "scale_factor > 0",                      "All scalings",
  "ex_arb_no_resize",             "resize",             "round(scale_factor, 4) != '0.3081'",    "No scaling",
  "ex_arb_no_fullscreen",         "fullscreen",         "fullscreenenter %in% c(0,1)",           "No fullscr. incl.",
  "ex_arb_no_fullscreen",         "fullscreen",         "fullscreenenter == 1",                  "No fullscr. excl.",
  "ex_arb_exit_fullscreen",       "fullscreen",         "fullscreenexit %in% c(0,1)",            "Exit fullscr. incl.",
  "ex_arb_exit_fullscreen",       "fullscreen",         "fullscreenexit == 0",                   "Exit fullscr. excl.",
  "ex_arb_event_during_change",   "blur event",         "is.logical(event_during_change)",       "Blur events incl.",
  "ex_arb_event_during_change",   "blur event",         "event_during_change == TRUE",           "Blur events excl.",
  "ex_arb_captcha",               "captcha",            "meta_captcha > 0",                      "All captcha scores",
  "ex_arb_captcha",               "captcha",            "meta_captcha > 0.4",                    "Captcha > 0.4",
  "ex_arb_interrupt",             "attention",          "attention_interrupt_sum %in% c(0,1,2)", "All interruptions", 
  "ex_arb_interrupt",             "attention",          "attention_interrupt_sum < 2",           "No interruptions",
  "ex_arb_noise",                 "noise",              "att_noise %in% c(0,1,2,3,4)",           "All noise levels",
  "ex_arb_noise",                 "noise",              "att_noise %in% c(0,1,2)",               "Low noise levels",
)



# Attention Cueing Task
decision_grid_cueing <- expand_grid(
  ex_arb_low_res              = c("meta_resolution_height > 0",            "meta_resolution_height > 700"),        
  ex_arb_no_resize            = c("scale_factor > 0",                      "round(scale_factor, 4) != '0.3081'"),
  ex_arb_no_fullscreen        = c("fullscreenenter %in% c(0,1)",           "fullscreenenter == 1"),
  ex_arb_exit_fullscreen      = c("fullscreenexit %in% c(0,1)",            "fullscreenexit == 0"),
  ex_arb_event_during_cueing  = c("is.logical(event_during_cueing)",       "event_during_cueing == TRUE"),
  ex_arb_captcha              = c("meta_captcha > 0",                      "meta_captcha > 0.4"),
  ex_arb_interrupt            = c("attention_interrupt_sum %in% c(0,1,2)", "attention_interrupt_sum < 2"),
  ex_arb_noise                = c("att_noise %in% c(0,1,2,3,4)",           "att_noise %in% c(0,1,2)")
)

decision_grid_cueing_key <- tribble(
  ~variable,                      ~variable_group,      ~exp,                                    ~name,
  "ex_arb_low_res",               "resolution",         "meta_resolution_height > 0",            "Resolution too low",
  "ex_arb_low_res",               "resolution",         "meta_resolution_height > 700",          "All resolutions",
  "ex_arb_no_resize",             "resize",             "scale_factor > 0",                      "All scalings",
  "ex_arb_no_resize",             "resize",             "round(scale_factor, 4) != '0.3081'",    "No scaling",
  "ex_arb_no_fullscreen",         "fullscreen",         "fullscreenenter %in% c(0,1)",           "No fullscr. incl.",
  "ex_arb_no_fullscreen",         "fullscreen",         "fullscreenenter == 1",                  "No fullscr. excl.",
  "ex_arb_exit_fullscreen",       "fullscreen",         "fullscreenexit %in% c(0,1)",            "Exit fullscr. incl.",
  "ex_arb_exit_fullscreen",       "fullscreen",         "fullscreenexit == 0",                   "Exit fullscr. excl.",
  "ex_arb_event_during_cueing",   "blur event",         "is.logical(event_during_cueing)",       "Blur events incl.",
  "ex_arb_event_during_cueing",   "blur event",         "event_during_cueing == TRUE",           "Blur events excl.",
  "ex_arb_captcha",               "captcha",            "meta_captcha > 0",                      "All captcha scores",
  "ex_arb_captcha",               "captcha",            "meta_captcha > 0.4",                    "Captcha > 0.4",
  "ex_arb_interrupt",             "attention",          "attention_interrupt_sum %in% c(0,1,2)", "All interruptions", 
  "ex_arb_interrupt",             "attention",          "attention_interrupt_sum < 2",           "No interruptions",
  "ex_arb_noise",                 "noise",              "att_noise %in% c(0,1,2,3,4)",           "All noise levels",
  "ex_arb_noise",                 "noise",              "att_noise %in% c(0,1,2)",               "Low noise levels",
)




# Attention Cueing Task
decision_grid_flanker <- expand_grid(
  ex_arb_low_res              = c("meta_resolution_height > 0",            "meta_resolution_height > 700"),        
  ex_arb_no_resize            = c("scale_factor > 0",                      "round(scale_factor, 4) != '0.3081'"),
  ex_arb_no_fullscreen        = c("fullscreenenter %in% c(0,1)",           "fullscreenenter == 1"),
  ex_arb_exit_fullscreen      = c("fullscreenexit %in% c(0,1)",            "fullscreenexit == 0"),
  ex_arb_event_during_flanker = c("is.logical(event_during_flanker)",      "event_during_flanker == TRUE"),
  ex_arb_captcha              = c("meta_captcha > 0",                      "meta_captcha > 0.4"),
  ex_arb_interrupt            = c("attention_interrupt_sum %in% c(0,1,2)", "attention_interrupt_sum < 2"),
  ex_arb_noise                = c("att_noise %in% c(0,1,2,3,4)",           "att_noise %in% c(0,1,2)")
)

decision_grid_flanker_key <- tribble(
  ~variable,                      ~variable_group,      ~exp,                                    ~name,
  "ex_arb_low_res",               "resolution",         "meta_resolution_height > 0",            "Resolution too low",
  "ex_arb_low_res",               "resolution",         "meta_resolution_height > 700",          "All resolutions",
  "ex_arb_no_resize",             "resize",             "scale_factor > 0",                      "All scalings",
  "ex_arb_no_resize",             "resize",             "round(scale_factor, 4) != '0.3081'",    "No scaling",
  "ex_arb_no_fullscreen",         "fullscreen",         "fullscreenenter %in% c(0,1)",           "No fullscr. incl.",
  "ex_arb_no_fullscreen",         "fullscreen",         "fullscreenenter == 1",                  "No fullscr. excl.",
  "ex_arb_exit_fullscreen",       "fullscreen",         "fullscreenexit %in% c(0,1)",            "Exit fullscr. incl.",
  "ex_arb_exit_fullscreen",       "fullscreen",         "fullscreenexit == 0",                   "Exit fullscr. excl.",
  "ex_arb_event_during_flanker",  "blur event",         "is.logical(event_during_flanker)",      "Blur events incl.",
  "ex_arb_event_during_flanker",  "blur event",         "event_during_flanker == TRUE",          "Blur events excl.",
  "ex_arb_captcha",               "captcha",            "meta_captcha > 0",                      "All captcha scores",
  "ex_arb_captcha",               "captcha",            "meta_captcha > 0.4",                    "Captcha > 0.4",
  "ex_arb_interrupt",             "attention",          "attention_interrupt_sum %in% c(0,1,2)", "All interruptions", 
  "ex_arb_interrupt",             "attention",          "attention_interrupt_sum < 2",           "No interruptions",
  "ex_arb_noise",                 "noise",              "att_noise %in% c(0,1,2,3,4)",           "All noise levels",
  "ex_arb_noise",                 "noise",              "att_noise %in% c(0,1,2)",               "Low noise levels",
)
