//------------------------- Global task settings  
  
var n_trials = 50 // 8 practice trials and 50 test trials
var movement_px = 40 // Number of pixels of displacement of moving circle
var endtime_memory_set = 1800 // presentation of memory set starts at 1000 and ends at the time specified here
var interstim_interval = 500 // time between memory set and test set
  
  
  
//------------------------- Stimulus locations  
  
  
// Randomly determine the location of each circle within a pre-specified area to prevent overlap
var mem_x = []
var mem_y = []
  
for (var i = 0; i < (n_trials); i++) {
    
mem_x[i] = [
  jsPsych.randomization.sampleWithoutReplacement([-25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25], 1)[0], // x-coordinate of first circle,
  jsPsych.randomization.sampleWithoutReplacement([-125, -130, -135, -140, -145, -150, -155, -160, -165, -170, -175], 1)[0], // x-coordinate of second circle,
  jsPsych.randomization.sampleWithoutReplacement([-45, -50, -55, -60, -65, -70, -75, -80, -85, -90, -95], 1)[0], // x-coordinate of third circle,
  jsPsych.randomization.sampleWithoutReplacement([45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95], 1)[0], // x-coordinate of fourth circle,
  jsPsych.randomization.sampleWithoutReplacement([125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175], 1)[0], // x-coordinate of fifth circle
]
    
mem_y[i] = [
  jsPsych.randomization.sampleWithoutReplacement([-125, -130, -135, -140, -145, -150, -155, -160, -165, -170, -175], 1)[0], // y-coordinate of first circle,
  jsPsych.randomization.sampleWithoutReplacement([-25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25], 1)[0], // y-coordinate of second circle,
  jsPsych.randomization.sampleWithoutReplacement([125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175], 1)[0], // y-coordinate of third circle,
  jsPsych.randomization.sampleWithoutReplacement([125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175], 1)[0], // y-coordinate of fourth circle,
  jsPsych.randomization.sampleWithoutReplacement([45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95], 1)[0], // y-coordinate of fifth circle
  ]
}
  
  
//------------------------- Control spatial displacement of stimulus


var circle_to_change = []
var test_x = []
var test_y = []
  
for (var i = 0; i < (n_trials); i++) {
    
  // Determine which circle is going to move
  circle_to_change[i] = jsPsych.randomization.sampleWithoutReplacement(['circle1', 'circle2', 'circle3', 'circle4', 'circle5'], 1)
    
  // Randomly determine movement along the x-axis (minimal movement = 0px, maximal movement = 40px; either positive or negative)
  change_circle_x = (Math.floor(Math.random() * (movement_px+1))) * jsPsych.randomization.sampleWithoutReplacement([-1, 1], 1)[0]
  
  
  // Given movement across x-axis and total movement, solve for y 
  change_circle_y = Math.sqrt((movement_px ** 2) - (change_circle_x ** 2)) * jsPsych.randomization.sampleWithoutReplacement([-1, 1], 1)[0]
  
  // Change the x/y coordinates of the moving circle and keep all other coordinates the same as in the memory set
    
  // ** X-coordinate
    
  test_x[i] = [null, null, null, null, null]
  test_y[i] = [null, null, null, null, null]
    
  // ** X-coordinate
  if(circle_to_change[i] == "circle1") {
    test_x[i][0] = mem_x[i][0] + change_circle_x
    test_y[i][0] = mem_y[i][0] + change_circle_y
  } else {
    test_x[i][0] = mem_x[i][0]
    test_y[i][0] = mem_y[i][0]
  }

  if(circle_to_change[i] == "circle2") {
    test_x[i][1] = mem_x[i][1] + change_circle_x
    test_y[i][1] = mem_y[i][1] + change_circle_y
  } else {
    test_x[i][1] = mem_x[i][1]
    test_y[i][1] = mem_y[i][1]
  }
      
  if(circle_to_change[i] == "circle3") {
    test_x[i][2] = mem_x[i][2] + change_circle_x
    test_y[i][2] = mem_y[i][2] + change_circle_y
  } else {
    test_x[i][2] = mem_x[i][2]
    test_y[i][2] = mem_y[i][2]
  }

  if(circle_to_change[i] == "circle4") {
    test_x[i][3] = mem_x[i][3] + change_circle_x
    test_y[i][3] = mem_y[i][3] + change_circle_y
  } else {
    test_x[i][3] = mem_x[i][3]
    test_y[i][3] = mem_y[i][3]
  }
  
  if(circle_to_change[i] == "circle5") {
    test_x[i][4] = mem_x[i][4] + change_circle_x
    test_y[i][4] = mem_y[i][4] + change_circle_y
  } else {
    test_x[i][4] = mem_x[i][4]
    test_y[i][4] = mem_y[i][4]
  }
      
} 

//------------------------- Stimulus objects

var fullscreenmode = {
  type: 'fullscreen',
  fullscreen_mode: true
};

var cursor_off = {
    type: 'call-function',
    func: function() {
        document.body.style.cursor= "none";
    }
}

var cursor_on = {
    type: 'call-function',
    func: function() {
        document.body.style.cursor= "auto";
    }
}



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
  radius: 15,
  line_color: jsPsych.timelineVariable('stim1_color'),
  fill_color: jsPsych.timelineVariable('stim1_color'),
  show_start_time: 1000,
  show_end_time: endtime_memory_set,
  origin_center: true
}
  
var mem_circle2 = {
  obj_type: "circle",
  startX: jsPsych.timelineVariable("mem_stim2_x"),
  startY: jsPsych.timelineVariable("mem_stim2_y"),
  radius: 15,
  line_color: jsPsych.timelineVariable('stim2_color'),
  fill_color: jsPsych.timelineVariable('stim2_color'),
  show_start_time: 1000,
  show_end_time: endtime_memory_set,
  origin_center: true
}
  
var mem_circle3 = {
  obj_type: "circle",
  startX: jsPsych.timelineVariable("mem_stim3_x"),
  startY: jsPsych.timelineVariable("mem_stim3_y"),
  radius: 15,
  line_color: jsPsych.timelineVariable('stim3_color'),
  fill_color: jsPsych.timelineVariable('stim3_color'),
  show_start_time: 1000,
  show_end_time: endtime_memory_set,
  origin_center: true
}
  
var mem_circle4 = {
  obj_type: "circle",
  startX: jsPsych.timelineVariable("mem_stim4_x"),
  startY: jsPsych.timelineVariable("mem_stim4_y"),
  radius: 15,
  line_color: jsPsych.timelineVariable('stim4_color'),
  fill_color: jsPsych.timelineVariable('stim4_color'),
  show_start_time: 1000,
  show_end_time: endtime_memory_set,
  origin_center: true
}
  
var mem_circle5 = {
  obj_type: "circle",
  startX: jsPsych.timelineVariable("mem_stim5_x"),
  startY: jsPsych.timelineVariable("mem_stim5_y"),
  radius: 15,
  line_color: jsPsych.timelineVariable('stim5_color'),
  fill_color: jsPsych.timelineVariable('stim5_color'),
  show_start_time: 1000,
  show_end_time: endtime_memory_set,
  origin_center: true
}
  
// Visual masks to cover up the circles after the initial memory phase 
var test_circle1 = {
  obj_type: "circle",
  startX: jsPsych.timelineVariable("test_stim1_x"),
  startY: jsPsych.timelineVariable("test_stim1_y"),
  radius: 15,
  line_color: jsPsych.timelineVariable('stim1_color'),
  fill_color: jsPsych.timelineVariable('stim1_color'),
  show_start_time: endtime_memory_set + interstim_interval,
  origin_center: true
}
  
var test_circle2 = {
  obj_type: "circle",
  startX: jsPsych.timelineVariable("test_stim2_x"),
  startY: jsPsych.timelineVariable("test_stim2_y"),
  radius: 15,
  line_color: jsPsych.timelineVariable('stim2_color'),
  fill_color: jsPsych.timelineVariable('stim2_color'),
  show_start_time: endtime_memory_set + interstim_interval,
  origin_center: true
}
  
var test_circle3 = {
  obj_type: "circle",
  startX: jsPsych.timelineVariable("test_stim3_x"),
  startY: jsPsych.timelineVariable("test_stim3_y"),
  radius: 15,
  line_color: jsPsych.timelineVariable('stim3_color'),
  fill_color: jsPsych.timelineVariable('stim3_color'),
  show_start_time: endtime_memory_set + interstim_interval,
  origin_center: true
}
  
var test_circle4 = {
  obj_type: "circle",
  startX: jsPsych.timelineVariable("test_stim4_x"),
  startY: jsPsych.timelineVariable("test_stim4_y"),
  radius: 15,
  line_color: jsPsych.timelineVariable('stim4_color'),
  fill_color: jsPsych.timelineVariable('stim4_color'),
  show_start_time: endtime_memory_set + interstim_interval,
  origin_center: true
}
  
var test_circle5 = {
  obj_type: "circle",
  startX: jsPsych.timelineVariable("test_stim5_x"),
  startY: jsPsych.timelineVariable("test_stim5_y"),
  radius: 15,
  line_color: jsPsych.timelineVariable('stim5_color'),
  fill_color: jsPsych.timelineVariable('stim5_color'),
  show_start_time: endtime_memory_set + interstim_interval,
  origin_center: true
}

var key_reminders = {
  obj_type: "text",
  font: "15px 'Arial'",
  startY: 280,
  startX: 0,
  content: "SAME                                               DIFFERENT",
  origin_center: true
}

