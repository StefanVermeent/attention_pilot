library(officer)
library(magrittr)

primary <- read_pptx() %>%
  # Title page
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
  ph_with(value = "Pilot Study", location = ph_location_type(type = "ctrTitle")) %>%
  ph_with(value = "Analyses on raw performance\n(RT and accuracy)", location = ph_location_type(type = "subTitle")) %>%
  
  
  # Change Detection distributions:
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with("Change Detection Task", location = ph_location_type("title")) %>%
  ph_with(supp_fig1, location = ph_location_type(type="body")) %>%
  # Cued Attention distributions:
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with("Cued Attention Task", location = ph_location_type("title")) %>%
  ph_with(supp_fig2, location = ph_location_type(type="body")) %>%
  # Flanker distributions:
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with("Flanker Task", location = ph_location_type("title")) %>%
  ph_with(supp_fig3, location = ph_location_type(type="body")) %>%
  
  
  # Primary analyses: Effect of violence on raw performance
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
  ph_with(value = "Primary analyses: Violence on Raw Performance", location = ph_location_type(type='ctrTitle')) %>%
  
  
  # Covariates
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Identifying potential covariates", location = ph_location_type("title")) %>%
  ph_with(flextable::flextable(covariates, cwidth = c(1.6, rep(0.6, 18))), location = ph_location(left=0, top = 1.5)) %>%

  # Change Detection Task
  add_slide(layout = "Two Content", master = "Office Theme") %>%
  ph_with("Change Detection Task", location = ph_location_type("title")) %>%
  ph_with(flextable::flextable(primary_raw_vio_change_rt, cwidth = c(1.6,0.6,0.6,0.6,0.6,0.6)), location = ph_location(left = 0.5, top = 4)) %>%
  ph_with(flextable::flextable(primary_raw_vio_change_acc, cwidth = c(1.6,0.6,0.6,0.6,0.6,0.6)), location = ph_location(left = 5.5, top = 4)) %>%
  # Change Detection Task 
  add_slide(layout = "Two Content", master = "Office Theme") %>%
  ph_with("Cued Attention Task - Reaction Time", location = ph_location_type("title")) %>%
  ph_with(flextable::flextable(primary_raw_vio_cueing_rt, cwidth = c(1.6,0.7,0.7,0.7,0.7,0.7)), location = ph_location(left = 0.5, top = 2.5, width = 4.5)) %>%
  ph_with(fig_primary_vio_cueing_interaction, location = ph_location(left = 5.5, top = 2.5)) %>%
  # Flanker Task
  add_slide(layout = "Two Content", master = "Office Theme") %>%
  ph_with("Flanker Task - Reaction Time", location = ph_location_type("title")) %>%
  ph_with(flextable::flextable(primary_raw_vio_flanker_rt, cwidth = c(1.6,0.7,0.7,0.7,0.7,0.7)), location = ph_location(left = 0.5, top = 2.5, width = 4.5)) %>%
  ph_with(fig_primary_vio_flanker_interaction, location = ph_location(left = 5.5, top = 2.5)) %>%
  
  
  # Secondary analyses: Effect of Unpredictability on raw performance
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
  ph_with(value = "Secondary analyses: Unpredictability on Raw Performance", location = ph_location_type(type='ctrTitle')) %>%
  
  # Correlations
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with("Correlations between unpredictability measures", location = ph_location_type("title")) %>%
  ph_with(flextable::flextable(unp_cors, cwidth = c(1.5, rep(0.7, 8))), location = ph_location_type("body")) %>%
  
  # Change Detection Task
  add_slide(layout = "Two Content", master = "Office Theme") %>%
  ph_with("Change Detection Task", location = ph_location_type("title")) %>%
  ph_with(flextable::flextable(secondary_raw_unp_change_rt, cwidth = c(1.6,0.6,0.6,0.6,0.6,0.6)), location = ph_location(left = 0.5, top = 4)) %>%
  ph_with(flextable::flextable(secondary_raw_unp_change_acc, cwidth = c(1.6,0.6,0.6,0.6,0.6,0.6)), location = ph_location(left = 5.5, top = 4)) %>%
  # Change Detection Task 
  add_slide(layout = "Two Content", master = "Office Theme") %>%
  ph_with("Cued Attention Task - Reaction Time", location = ph_location_type("title")) %>%
  ph_with(flextable::flextable(secondary_raw_unp_cueing_rt, cwidth = c(1.6,0.7,0.7,0.7,0.7,0.7)), location = ph_location(left = 0.5, top = 2.5, width = 4.5)) %>%
  ph_with(fig_secondary_unp_cueing_interaction, location = ph_location(left = 5.5, top = 2.5)) %>%
  # Flanker Task
  add_slide(layout = "Two Content", master = "Office Theme") %>%
  ph_with("Flanker Task - Reaction Time", location = ph_location_type("title")) %>%
  ph_with(flextable::flextable(secondary_raw_unp_flanker_rt, cwidth = c(1.6,0.7,0.7,0.7,0.7,0.7)), location = ph_location(left = 0.5, top = 2.5, width = 4.5)) %>%
  ph_with(fig_secondary_unp_flanker_interaction, location = ph_location(left = 5.5, top = 2.5)) 
  
  
print(primary, here("pilot_analyses.pptx"))


