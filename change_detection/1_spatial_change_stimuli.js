//------------------------- Stimulus objects  
  

  // Fixation cross
  var fixation_cross = {
    obj_type: 'cross',
    line_length: 25,
    startX: 0,
    startY: 0,
    show_start_time: 0,
    origin_center: true
  }
  
  
  // Memory stimuli
  var mem_circle1 = {
    obj_type: "circle",
    startX: jsPsych.timelineVariable("mem_stim1_x"),
    startY: jsPsych.timelineVariable("mem_stim1_y"),
    radius: 20,
    line_color: jsPsych.timelineVariable('stim1_color'),
    fill_color: jsPsych.timelineVariable('stim1_color'),
    show_start_time: 1000,
   // show_end_time: 3000,
    origin_center: true
  }
  
  var mem_circle2 = {
    obj_type: "circle",
    startX: jsPsych.timelineVariable("mem_stim2_x"),
    startY: jsPsych.timelineVariable("mem_stim2_y"),
    radius: 20,
    line_color: jsPsych.timelineVariable('stim2_color'),
    fill_color: jsPsych.timelineVariable('stim2_color'),
    show_start_time: 1000,
  //  show_end_time: 3000,
    origin_center: true
  }
  
  var mem_circle3 = {
    obj_type: "circle",
    startX: jsPsych.timelineVariable("mem_stim3_x"),
    startY: jsPsych.timelineVariable("mem_stim3_y"),
    radius: 20,
    line_color: jsPsych.timelineVariable('stim3_color'),
    fill_color: jsPsych.timelineVariable('stim3_color'),
    show_start_time: 1000,
  //  show_end_time: 3000,
    origin_center: true
  }
  
  var mem_circle4 = {
    obj_type: "circle",
    startX: jsPsych.timelineVariable("mem_stim4_x"),
    startY: jsPsych.timelineVariable("mem_stim4_y"),
    radius: 20,
    line_color: jsPsych.timelineVariable('stim4_color'),
    fill_color: jsPsych.timelineVariable('stim4_color'),
    show_start_time: 1000,
  //  show_end_time: 3000,
    origin_center: true
  }
  
  var mem_circle5 = {
    obj_type: "circle",
     startX: jsPsych.timelineVariable("mem_stim5_x"),
    startY: jsPsych.timelineVariable("mem_stim5_y"),
    radius: 20,
    line_color: jsPsych.timelineVariable('stim5_color'),
    fill_color: jsPsych.timelineVariable('stim5_color'),
    show_start_time: 1000,
  //  show_end_time: 3000,
    origin_center: true
  }
  
  var mem_circle6 = {
    obj_type: "circle",
    startX: jsPsych.timelineVariable("mem_stim6_x"),
    startY: jsPsych.timelineVariable("mem_stim6_y"),
    radius: 20,
    line_color: jsPsych.timelineVariable('stim6_color'),
    fill_color: jsPsych.timelineVariable('stim6_color'),
    show_start_time: 1000,
  //  show_end_time: 3000,
    origin_center: true
  }
  
  // Visual masks to cover up the circles after the initial memory phase 
  var test_circle1 = {
    obj_type: "circle",
    startX: jsPsych.timelineVariable("test_stim1_x"),
    startY: jsPsych.timelineVariable("test_stim1_y"),
    radius: 20,
    line_color: jsPsych.timelineVariable('stim1_color'),
    fill_color: jsPsych.timelineVariable('stim1_color'),
    show_start_time: 2000,
   // show_end_time: 2500,
    origin_center: true
  }
  
  var test_circle2 = {
    obj_type: "circle",
    startX: jsPsych.timelineVariable("test_stim2_x"),
    startY: jsPsych.timelineVariable("test_stim2_y"),
    radius: 20,
    line_color: jsPsych.timelineVariable('stim2_color'),
    fill_color: jsPsych.timelineVariable('stim2_color'),
    show_start_time: 2000,
  //  show_end_time: 2500,
    origin_center: true
  }
  
  var test_circle3 = {
    obj_type: "circle",
    startX: jsPsych.timelineVariable("test_stim3_x"),
    startY: jsPsych.timelineVariable("test_stim3_y"),
    radius: 20,
    line_color: jsPsych.timelineVariable('stim3_color'),
    fill_color: jsPsych.timelineVariable('stim3_color'),
    show_start_time: 2000,
   // show_end_time: 2500,
    origin_center: true
  }
  
    var test_circle4 = {
    obj_type: "circle",
    startX: jsPsych.timelineVariable("test_stim4_x"),
    startY: jsPsych.timelineVariable("test_stim4_y"),
    radius: 20,
    line_color: jsPsych.timelineVariable('stim4_color'),
    fill_color: jsPsych.timelineVariable('stim4_color'),
    show_start_time: 2000,
   // show_end_time: 2500,
    origin_center: true
  }
  
    var test_circle5 = {
    obj_type: "circle",
     startX: jsPsych.timelineVariable("test_stim5_x"),
    startY: jsPsych.timelineVariable("test_stim5_y"),
    radius: 20,
    line_color: jsPsych.timelineVariable('stim5_color'),
    fill_color: jsPsych.timelineVariable('stim5_color'),
    show_start_time: 2000,
  //  show_end_time: 2500,
    origin_center: true
  }
  
    var test_circle6 = {
    obj_type: "circle",
    startX: jsPsych.timelineVariable("test_stim6_x"),
    startY: jsPsych.timelineVariable("test_stim6_y"),
    radius: 20,
    line_color: jsPsych.timelineVariable('stim6_color'),
    fill_color: jsPsych.timelineVariable('stim6_color'),
    show_start_time: 2000,
  //  show_end_time: 2500,
    origin_center: true
  }