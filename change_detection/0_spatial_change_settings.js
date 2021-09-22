//------------------------- Global task settings  
  
  var color_set = ["#0070C0", "#00B050", "#BB0703", "#E836D3", "#FFC000", "#904090", "#33E2EB", "#ED7D31"]; // blue, green, red, pink, yellow, purple
  var n_trials = 50
  var movement_px = 60 // Number of pixels of displacement of moving circle
  
  
  
//------------------------- Stimulus color and locations  
  
  // Randomly sample six colors from the full set for the current memory items
  var memory_set_colors = [];

  for (var i = 0; i < (n_trials-1); i++) {
    memory_set_colors[i] = jsPsych.randomization.sampleWithoutReplacement(color_set, 8);
  }
  
  
  // Randomly determine the location of each circle within a pre-specified area to prevent overlap
  var memory_set_coord_x = []
  var memory_set_coord_y = []
  
  for (var i = 0; i < (n_trials-1); i++) {
    
    memory_set_coord_x[i] = [
      jsPsych.randomization.sampleWithoutReplacement([-360, -340, -320, -300, -280, -260, -240, -220, -200, -180, -160, -140, -120, -100, -80, -60], 1)[0], // x-coordinate of first circle,
      jsPsych.randomization.sampleWithoutReplacement([-360, -340, -320, -300, -280, -260, -240, -220, -200, -180, -160, -140, -120, -100, -80, -60], 1)[0], // x-coordinate of second circle,
      jsPsych.randomization.sampleWithoutReplacement([-360, -340, -320, -300, -280, -260, -240, -220, -200, -180, -160, -140, -120, -100, -80, -60], 1)[0], // x-coordinate of third circle,
      jsPsych.randomization.sampleWithoutReplacement([360, 340, 320, 300, 280, 260, 240, 220, 200, 180, 160, 140, 120, 100, 80, 60], 1)[0], // x-coordinate of fourth circle,
      jsPsych.randomization.sampleWithoutReplacement([360, 340, 320, 300, 280, 260, 240, 220, 200, 180, 160, 140, 120, 100, 80, 60], 1)[0], // x-coordinate of fifth circle,
      jsPsych.randomization.sampleWithoutReplacement([360, 340, 320, 300, 280, 260, 240, 220, 200, 180, 160, 140, 120, 100, 80, 60], 1)[0] // x-coordinate of sixth circle
    ]
    
    memory_set_coord_y[i] = [
      jsPsych.randomization.sampleWithoutReplacement([-300, -280, -260, -240, -220, -200, -180], 1)[0], // y-coordinate of first circle,
      jsPsych.randomization.sampleWithoutReplacement([-60, -40, -20, 0, 20, 40, 60], 1)[0], // y-coordinate of second circle,
      jsPsych.randomization.sampleWithoutReplacement([180, 200, 220, 240, 260, 280, 300], 1)[0], // y-coordinate of third circle,
      jsPsych.randomization.sampleWithoutReplacement([-300, -280, -260, -240, -220, -200, -180], 1)[0], // y-coordinate of fourth circle,
      jsPsych.randomization.sampleWithoutReplacement([-60, -40, -20, 0, 20, 40, 60, 1], 1)[0], // y-coordinate of fifth circle,
      jsPsych.randomization.sampleWithoutReplacement([-300, -280, -260, -240, -220, -200, -180], 1)[0] // y-coordinate of sixth circle
    ]
  }
  
  
//------------------------- Control spatial displacement of stimulus
  
  var circle_to_change = []
  var test_set_coord_x = []
  var test_set_coord_y = []
  
  for (var i = 0; i < ((n_trials/2)-1); i++) {
    
    // Determine which circle is going to move
    circle_to_change[i] = jsPsych.randomization.sampleWithoutReplacement(['circle1', 'circle2', 'circle3', 'circle4', 'circle5', 'circle6'], 1)[0]
    
    // Randomly determine movement along the x-axis (minimal movement = 0px, maximal movement = 60px; either positive or negative)
    change_circle_x = (Math.floor(Math.random() * (60 - 0)) + 0) * jsPsych.randomization.sampleWithoutReplacement([-1, 1], 1)[0]
    
    // Given movement across x-axis and total movement, solve for y 
    change_circle_y = Math.sqrt((movement_px^2) - (change_circle_x^2)) * jsPsych.randomization.sampleWithoutReplacement([-1, 1], 1)[0]
  
    
    // Change the x/y coordinates of the moving circle and keep all other coordinates the same as in the memory set
    
    // ** X-coordinate
    if(circle_to_change[i] == "circle1") {
      test_set_coord_x[i][0] = memory_set_coord_x[i][0] + change_circle_x
    } else {
      test_set_coord_x[i][0] = memory_set_coord_x[i][0]
    }

    if(circle_to_change[i] == "circle2") {
      test_set_coord_x[i][1] = memory_set_coord_x[i][1] + change_circle_x
    } else {
      test_set_coord_x[i][1] = memory_set_coord_x[i][1]
    }
      
    if(circle_to_change[i] == "circle3") {
      test_set_coord_x[i][2] = memory_set_coord_x[i][2] + change_circle_x
    } else {
      test_set_coord_x[i][2] = memory_set_coord_x[i][2]
    }

    if(circle_to_change[i] == "circle4") {
      test_set_coord_x[i][3] = memory_set_coord_x[i][3] + change_circle_x
    } else {
      test_set_coord_x[i][3] = memory_set_coord_x[i][3]
    }
  
    if(circle_to_change[i] == "circle5") {
      test_set_coord_x[i][4] = memory_set_coord_x[i][4] + change_circle_x
    } else {
      test_set_coord_x[i][4] = memory_set_coord_x[i][4]
    }
      
    if(circle_to_change[i] == "circle6") {
      test_set_coord_x[i][5] = memory_set_coord_x[i][5] + change_circle_x
    } else {
      test_set_coord_x[i][5] = memory_set_coord_x[i][5]
    }      
      
    // ** Y-coordinate **
    if(circle_to_change[i] == "circle1") {
      test_set_coord_y[i][0] = memory_set_coord_y[i][0] + change_circle_y
    } else {
      test_set_coord_y[i][0] = memory_set_coord_y[i][0]
    }

    if(circle_to_change[i] == "circle2") {
      test_set_coord_y[i][1] = memory_set_coord_y[i][1] + change_circle_y
    } else {
      test_set_coord_y[i][1] = memory_set_coord_y[i][1]
    }
      
    if(circle_to_change[i] == "circle3") {
      test_set_coord_y[i][2] = memory_set_coord_y[i][2] + change_circle_y
    } else {
      test_set_coord_y[i][2] = memory_set_coord_y[i][2]
    }

    if(circle_to_change[i] == "circle4") {
      test_set_coord_y[i][3] = memory_set_coord_y[i][3] + change_circle_y
    } else {
      test_set_coord_y[i][3] = memory_set_coord_y[i][3]
    }
  
    if(circle_to_change[i] == "circle5") {
      test_set_coord_y[i][4] = memory_set_coord_y[i][4] + change_circle_y
    } else {
      test_set_coord_y[i][4] = memory_set_coord_y[i][4]
    }
      
    if(circle_to_change[i] == "circle6") {
      test_set_coord_x[i][5] = memory_set_coord_x[i][5] + change_circle_x
    } else {
      test_set_coord_x[i][5] = memory_set_coord_x[i][5]
    }  
  }