//-------------------- Welcome
var change_welcome = {
  type: 'instructions',
  pages: [
    "Welcome to the <b>Change Detection</b> game!"
  ],
  show_clickable_nav: true,
  allow_backward: true,
  key_forward: -1,
  key_backward: -1,
  button_label_next: "continue",
  button_label_previous: "go back",
  data: {variable: 'welcome', task: "change"}
};

//-------------------- Instructions
var change_instructions1 = {
  type: 'instructions',
  pages: [
    "<p style = 'text-align: left; padding: 0 20% 0 20%;'>"+ 
      "You will be playing the <strong>Change Detection</strong> game.<br><br>" + 
      "In this game you will see a set of five colored circles, scattered randomly around the center of the screen.<br><br>" +
      "The circles are presented for one second, then briefly disappear from the screen, and then reappear again.<br><br>" +
      "On some trials, one of the circles has moved to another location after reappearing. On other trials, all circles are still in the same location.<br><br>" +
      "Click 'continue' for further instructions</p>"
  ],
  show_clickable_nav: true,
  allow_backward: true,
  key_forward: -1,
  key_backward: -1,
  button_label_next: "continue",
  button_label_previous: "go back",
  data: {variable: "instructions", task: "change"}
};


var change_instructions2 = {
  type: 'instructions',
  pages: [
    "<p style = 'text-align: left; padding: 0 20% 0 20%;'>"+ 
      "Your job is to indicate whether there has been a change in location, or whether all circles are still in the same location.<br><br>" +
      "If you think the circle locations are still <strong>the same</strong>, press the (&larr;) arrow on your keyboard.<br>" +
      "If you think one of the circles has <strong>changed</strong> location, press the (&rarr;) arrow on your keyboard.<br><br>" +
      "Click 'continue' for further instructions</p>"
  ],
  show_clickable_nav: true,
  allow_backward: true,
  key_forward: -1,
  key_backward: -1,
  button_label_next: "continue",
  button_label_previous: "go back",
  data: {variable: "instructions", task: "change"}
};

var change_instructions3 = {
  type: 'instructions',
  pages: [
    "<p style = 'text-align: left; padding: 0 20% 0 20%;'>"+ 
      "While memorizing the five circles, please try to keep your eyes fixated on the '+' presented at the center of the screen.<br><br>" +
      "To help you remember the correct keys for <strong>SAME</strong> (&larr;) and <strong>DIFFERENT</strong> (&rarr;) trials, the word 'SAME' is always presented on the lower-left side of the screen, " +
      "and 'DIFFERENT' on the lower-right side of the screen<br><br>" +
      "Please try to respond as quickly and accurately as possible.<br><br>"+
      "Click the 'continue' button to practice the <strong>Change Detection</strong> game.<br></p>"
  ],
  show_clickable_nav: true,
  allow_backward: true,
  key_forward: -1,
  key_backward: -1,
  button_label_next: "continue",
  button_label_previous: "go back",
  data: {variable: "instructions", task: "change"}
};

var change_practice_start = {
  type: 'html-keyboard-response',
  stimulus: "You are about to start the practice round.<br><br><strong>Press any key when you are ready!</strong><br><br>",
  choices: jsPsych.ALL_KEYS,
  data: {variable: "practice_start", task: "change"}
};


//-------------------- Practice trials

var change_practice1 = {
    timeline: [
      {
        type: 'psychophysics',
        stimuli: [
        key_reminders,
        change_fixation_cross, 
        mem_circle1, 
        mem_circle2, 
        mem_circle3, 
        mem_circle4, 
        mem_circle5, 
        test_circle1,
        test_circle2,
        test_circle3,
        test_circle4,
        test_circle5,
        ],
          choices: ["ArrowLeft", "ArrowRight"],
          response_start_time: endtime_memory_set + interstim_interval,
          canvas_width: 800,
          canvas_height: 800,
          background_color: '#8E8C8C',
      },
      ],
      data: {
        type: jsPsych.timelineVariable('type'),
        mem_stim1_x: jsPsych.timelineVariable('mem_stim1_x'),
        mem_stim2_x: jsPsych.timelineVariable('mem_stim2_x'),
        mem_stim3_x: jsPsych.timelineVariable('mem_stim3_x'),
        mem_stim4_x: jsPsych.timelineVariable('mem_stim4_x'),
        mem_stim5_x: jsPsych.timelineVariable('mem_stim5_x'),
        mem_stim1_y: jsPsych.timelineVariable('mem_stim1_y'),
        mem_stim2_y: jsPsych.timelineVariable('mem_stim2_y'),
        mem_stim3_y: jsPsych.timelineVariable('mem_stim3_y'),
        mem_stim4_y: jsPsych.timelineVariable('mem_stim4_y'),
        mem_stim5_y: jsPsych.timelineVariable('mem_stim5_y'),
        test_stim1_x: jsPsych.timelineVariable('test_stim1_x'),
        test_stim2_x: jsPsych.timelineVariable('test_stim2_x'),
        test_stim3_x: jsPsych.timelineVariable('test_stim3_x'),
        test_stim4_x: jsPsych.timelineVariable('test_stim4_x'),
        test_stim5_x: jsPsych.timelineVariable('test_stim5_x'),
        test_stim1_y: jsPsych.timelineVariable('test_stim1_y'),
        test_stim2_y: jsPsych.timelineVariable('test_stim2_y'),
        test_stim3_y: jsPsych.timelineVariable('test_stim3_y'),
        test_stim4_y: jsPsych.timelineVariable('test_stim4_y'),
        test_stim5_y: jsPsych.timelineVariable('test_stim5_y'),
        stim1_color: jsPsych.timelineVariable('stim1_color'),
        stim2_color: jsPsych.timelineVariable('stim2_color'),
        stim3_color: jsPsych.timelineVariable('stim3_color'),
        stim4_color: jsPsych.timelineVariable('stim4_color'),
        stim5_color: jsPsych.timelineVariable('stim5_color'),
        stim6_color: jsPsych.timelineVariable('stim6_color'),
        correct_response: jsPsych.timelineVariable('correct_response')
    },
    on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    },      
      timeline_variables: [
{type: 'different', correct_response: 'ArrowRight', mem_stim1_x: mem_x[25][0], mem_stim2_x: mem_x[25][1], mem_stim3_x: mem_x[25][2], mem_stim4_x: mem_x[25][3], mem_stim5_x: mem_x[25][4],
mem_stim1_y: mem_y[25][0], mem_stim2_y: mem_y[25][1], mem_stim3_y: mem_y[25][2], mem_stim4_y: mem_y[25][3], mem_stim5_y: mem_y[25][4],
test_stim1_x: test_x[25][0], test_stim2_x: mem_x[25][1], test_stim3_x: mem_x[25][2], test_stim4_x: mem_x[25][3], test_stim5_x: mem_x[25][4],
test_stim1_y: test_y[25][0], test_stim2_y: mem_y[25][1], test_stim3_y: mem_y[25][2], test_stim4_y: mem_y[25][3], test_stim5_y: mem_y[25][4],
stim1_color: '#D40E0E', stim2_color: '#4D0063', stim3_color: '#0457A0', stim4_color: '#25D9E4', stim5_color: '#FBF250'},
]
}

var change_practice2 = {
    timeline: [
      {
        type: 'psychophysics',
        stimuli: [
        key_reminders,
        change_fixation_cross, 
        mem_circle1, 
        mem_circle2, 
        mem_circle3, 
        mem_circle4, 
        mem_circle5, 
        test_circle1,
        test_circle2,
        test_circle3,
        test_circle4,
        test_circle5,
        ],
          choices: ["ArrowLeft", "ArrowRight"],
          response_start_time: endtime_memory_set + interstim_interval,
          canvas_width: 800,
          canvas_height: 800,
          background_color: '#8E8C8C',
      },
      ],
      data: {
        type: jsPsych.timelineVariable('type'),
        mem_stim1_x: jsPsych.timelineVariable('mem_stim1_x'),
        mem_stim2_x: jsPsych.timelineVariable('mem_stim2_x'),
        mem_stim3_x: jsPsych.timelineVariable('mem_stim3_x'),
        mem_stim4_x: jsPsych.timelineVariable('mem_stim4_x'),
        mem_stim5_x: jsPsych.timelineVariable('mem_stim5_x'),
        mem_stim1_y: jsPsych.timelineVariable('mem_stim1_y'),
        mem_stim2_y: jsPsych.timelineVariable('mem_stim2_y'),
        mem_stim3_y: jsPsych.timelineVariable('mem_stim3_y'),
        mem_stim4_y: jsPsych.timelineVariable('mem_stim4_y'),
        mem_stim5_y: jsPsych.timelineVariable('mem_stim5_y'),
        test_stim1_x: jsPsych.timelineVariable('test_stim1_x'),
        test_stim2_x: jsPsych.timelineVariable('test_stim2_x'),
        test_stim3_x: jsPsych.timelineVariable('test_stim3_x'),
        test_stim4_x: jsPsych.timelineVariable('test_stim4_x'),
        test_stim5_x: jsPsych.timelineVariable('test_stim5_x'),
        test_stim1_y: jsPsych.timelineVariable('test_stim1_y'),
        test_stim2_y: jsPsych.timelineVariable('test_stim2_y'),
        test_stim3_y: jsPsych.timelineVariable('test_stim3_y'),
        test_stim4_y: jsPsych.timelineVariable('test_stim4_y'),
        test_stim5_y: jsPsych.timelineVariable('test_stim5_y'),
        stim1_color: jsPsych.timelineVariable('stim1_color'),
        stim2_color: jsPsych.timelineVariable('stim2_color'),
        stim3_color: jsPsych.timelineVariable('stim3_color'),
        stim4_color: jsPsych.timelineVariable('stim4_color'),
        stim5_color: jsPsych.timelineVariable('stim5_color'),
        stim6_color: jsPsych.timelineVariable('stim6_color'),
        correct_response: jsPsych.timelineVariable('correct_response')
    },
    on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    },      
      timeline_variables: [
{type: 'same', correct_response: 'ArrowLeft', mem_stim1_x: mem_x[18][0], mem_stim2_x: mem_x[18][1], mem_stim3_x: mem_x[18][2], mem_stim4_x: mem_x[18][3], mem_stim5_x: mem_x[18][4],
mem_stim1_y: mem_y[18][0], mem_stim2_y: mem_y[18][1], mem_stim3_y: mem_y[18][2], mem_stim4_y: mem_y[18][3], mem_stim5_y: mem_y[18][4],
test_stim1_x: mem_x[18][0], test_stim2_x: mem_x[18][1], test_stim3_x: mem_x[18][2], test_stim4_x: mem_x[18][3], test_stim5_x: mem_x[18][4],
test_stim1_y: mem_y[18][0], test_stim2_y: mem_y[18][1], test_stim3_y: mem_y[18][2], test_stim4_y: mem_y[18][3], test_stim5_y: mem_y[18][4],
stim1_color: '#0457A0', stim2_color: '#FBF250', stim3_color: '#D40E0E', stim4_color: '#25D9E4', stim5_color: '#4D0063'},
]
}




var change_practice3 = {
    timeline: [
      {
        type: 'psychophysics',
        stimuli: [
        key_reminders,
        change_fixation_cross, 
        mem_circle1, 
        mem_circle2, 
        mem_circle3, 
        mem_circle4, 
        mem_circle5, 
        test_circle1,
        test_circle2,
        test_circle3,
        test_circle4,
        test_circle5,
        ],
          choices: ["ArrowLeft", "ArrowRight"],
          response_start_time: endtime_memory_set + interstim_interval,
          canvas_width: 800,
          canvas_height: 800,
          background_color: '#8E8C8C',
      },
      ],
      data: {
        type: jsPsych.timelineVariable('type'),
        mem_stim1_x: jsPsych.timelineVariable('mem_stim1_x'),
        mem_stim2_x: jsPsych.timelineVariable('mem_stim2_x'),
        mem_stim3_x: jsPsych.timelineVariable('mem_stim3_x'),
        mem_stim4_x: jsPsych.timelineVariable('mem_stim4_x'),
        mem_stim5_x: jsPsych.timelineVariable('mem_stim5_x'),
        mem_stim1_y: jsPsych.timelineVariable('mem_stim1_y'),
        mem_stim2_y: jsPsych.timelineVariable('mem_stim2_y'),
        mem_stim3_y: jsPsych.timelineVariable('mem_stim3_y'),
        mem_stim4_y: jsPsych.timelineVariable('mem_stim4_y'),
        mem_stim5_y: jsPsych.timelineVariable('mem_stim5_y'),
        test_stim1_x: jsPsych.timelineVariable('test_stim1_x'),
        test_stim2_x: jsPsych.timelineVariable('test_stim2_x'),
        test_stim3_x: jsPsych.timelineVariable('test_stim3_x'),
        test_stim4_x: jsPsych.timelineVariable('test_stim4_x'),
        test_stim5_x: jsPsych.timelineVariable('test_stim5_x'),
        test_stim1_y: jsPsych.timelineVariable('test_stim1_y'),
        test_stim2_y: jsPsych.timelineVariable('test_stim2_y'),
        test_stim3_y: jsPsych.timelineVariable('test_stim3_y'),
        test_stim4_y: jsPsych.timelineVariable('test_stim4_y'),
        test_stim5_y: jsPsych.timelineVariable('test_stim5_y'),
        stim1_color: jsPsych.timelineVariable('stim1_color'),
        stim2_color: jsPsych.timelineVariable('stim2_color'),
        stim3_color: jsPsych.timelineVariable('stim3_color'),
        stim4_color: jsPsych.timelineVariable('stim4_color'),
        stim5_color: jsPsych.timelineVariable('stim5_color'),
        stim6_color: jsPsych.timelineVariable('stim6_color'),
        correct_response: jsPsych.timelineVariable('correct_response')
    },
    on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    },      
      timeline_variables: [
{type: 'same', correct_response: 'ArrowLeft', mem_stim1_x: mem_x[44][0], mem_stim2_x: mem_x[44][1], mem_stim3_x: mem_x[44][2], mem_stim4_x: mem_x[44][3], mem_stim5_x: mem_x[44][4],
mem_stim1_y: mem_y[44][0], mem_stim2_y: mem_y[44][1], mem_stim3_y: mem_y[44][2], mem_stim4_y: mem_y[44][3], mem_stim5_y: mem_y[44][4],
test_stim1_x: mem_x[44][0], test_stim2_x: mem_x[44][1], test_stim3_x: mem_x[44][2], test_stim4_x: mem_x[44][3], test_stim5_x: mem_x[44][4],
test_stim1_y: mem_y[44][0], test_stim2_y: mem_y[44][1], test_stim3_y: mem_y[44][2], test_stim4_y: mem_y[44][3], test_stim5_y: mem_y[44][4],
stim1_color: '#FBF250', stim2_color: '#0457A0', stim3_color: '#D40E0E', stim4_color: '#25D9E4', stim5_color: '#4D0063'},
]
}




var change_practice4 = {
    timeline: [
      {
        type: 'psychophysics',
        stimuli: [
        key_reminders,
        change_fixation_cross, 
        mem_circle1, 
        mem_circle2, 
        mem_circle3, 
        mem_circle4, 
        mem_circle5, 
        test_circle1,
        test_circle2,
        test_circle3,
        test_circle4,
        test_circle5,
        ],
          choices: ["ArrowLeft", "ArrowRight"],
          response_start_time: endtime_memory_set + interstim_interval,
          canvas_width: 800,
          canvas_height: 800,
          background_color: '#8E8C8C',
      },
      ],
      data: {
        type: jsPsych.timelineVariable('type'),
        mem_stim1_x: jsPsych.timelineVariable('mem_stim1_x'),
        mem_stim2_x: jsPsych.timelineVariable('mem_stim2_x'),
        mem_stim3_x: jsPsych.timelineVariable('mem_stim3_x'),
        mem_stim4_x: jsPsych.timelineVariable('mem_stim4_x'),
        mem_stim5_x: jsPsych.timelineVariable('mem_stim5_x'),
        mem_stim1_y: jsPsych.timelineVariable('mem_stim1_y'),
        mem_stim2_y: jsPsych.timelineVariable('mem_stim2_y'),
        mem_stim3_y: jsPsych.timelineVariable('mem_stim3_y'),
        mem_stim4_y: jsPsych.timelineVariable('mem_stim4_y'),
        mem_stim5_y: jsPsych.timelineVariable('mem_stim5_y'),
        test_stim1_x: jsPsych.timelineVariable('test_stim1_x'),
        test_stim2_x: jsPsych.timelineVariable('test_stim2_x'),
        test_stim3_x: jsPsych.timelineVariable('test_stim3_x'),
        test_stim4_x: jsPsych.timelineVariable('test_stim4_x'),
        test_stim5_x: jsPsych.timelineVariable('test_stim5_x'),
        test_stim1_y: jsPsych.timelineVariable('test_stim1_y'),
        test_stim2_y: jsPsych.timelineVariable('test_stim2_y'),
        test_stim3_y: jsPsych.timelineVariable('test_stim3_y'),
        test_stim4_y: jsPsych.timelineVariable('test_stim4_y'),
        test_stim5_y: jsPsych.timelineVariable('test_stim5_y'),
        stim1_color: jsPsych.timelineVariable('stim1_color'),
        stim2_color: jsPsych.timelineVariable('stim2_color'),
        stim3_color: jsPsych.timelineVariable('stim3_color'),
        stim4_color: jsPsych.timelineVariable('stim4_color'),
        stim5_color: jsPsych.timelineVariable('stim5_color'),
        stim6_color: jsPsych.timelineVariable('stim6_color'),
        correct_response: jsPsych.timelineVariable('correct_response')
    },
    on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    },      
      timeline_variables: [
{type: 'different', correct_response: 'ArrowRight', mem_stim1_x: mem_x[49][0], mem_stim2_x: mem_x[49][1], mem_stim3_x: mem_x[49][2], mem_stim4_x: mem_x[49][3], mem_stim5_x: mem_x[49][4],
mem_stim1_y: mem_y[49][0], mem_stim2_y: mem_y[49][1], mem_stim3_y: mem_y[49][2], mem_stim4_y: mem_y[49][3], mem_stim5_y: mem_y[49][4],
test_stim1_x: mem_x[49][0], test_stim2_x: mem_x[49][1], test_stim3_x: mem_x[49][2], test_stim4_x: mem_x[49][3], test_stim5_x: test_x[49][4],
test_stim1_y: mem_y[49][0], test_stim2_y: mem_y[49][1], test_stim3_y: mem_y[49][2], test_stim4_y: mem_y[49][3], test_stim5_y: test_y[49][4],
stim1_color: '#25D9E4', stim2_color: '#0457A0', stim3_color: '#D40E0E', stim4_color: '#FBF250', stim5_color: '#4D0063'},
]
}




var change_practice5 = {
    timeline: [
      {
        type: 'psychophysics',
        stimuli: [
        key_reminders,
        change_fixation_cross, 
        mem_circle1, 
        mem_circle2, 
        mem_circle3, 
        mem_circle4, 
        mem_circle5, 
        test_circle1,
        test_circle2,
        test_circle3,
        test_circle4,
        test_circle5,
        ],
          choices: ["ArrowLeft", "ArrowRight"],
          response_start_time: endtime_memory_set + interstim_interval,
          canvas_width: 800,
          canvas_height: 800,
          background_color: '#8E8C8C',
      },
      ],
      data: {
        type: jsPsych.timelineVariable('type'),
        mem_stim1_x: jsPsych.timelineVariable('mem_stim1_x'),
        mem_stim2_x: jsPsych.timelineVariable('mem_stim2_x'),
        mem_stim3_x: jsPsych.timelineVariable('mem_stim3_x'),
        mem_stim4_x: jsPsych.timelineVariable('mem_stim4_x'),
        mem_stim5_x: jsPsych.timelineVariable('mem_stim5_x'),
        mem_stim1_y: jsPsych.timelineVariable('mem_stim1_y'),
        mem_stim2_y: jsPsych.timelineVariable('mem_stim2_y'),
        mem_stim3_y: jsPsych.timelineVariable('mem_stim3_y'),
        mem_stim4_y: jsPsych.timelineVariable('mem_stim4_y'),
        mem_stim5_y: jsPsych.timelineVariable('mem_stim5_y'),
        test_stim1_x: jsPsych.timelineVariable('test_stim1_x'),
        test_stim2_x: jsPsych.timelineVariable('test_stim2_x'),
        test_stim3_x: jsPsych.timelineVariable('test_stim3_x'),
        test_stim4_x: jsPsych.timelineVariable('test_stim4_x'),
        test_stim5_x: jsPsych.timelineVariable('test_stim5_x'),
        test_stim1_y: jsPsych.timelineVariable('test_stim1_y'),
        test_stim2_y: jsPsych.timelineVariable('test_stim2_y'),
        test_stim3_y: jsPsych.timelineVariable('test_stim3_y'),
        test_stim4_y: jsPsych.timelineVariable('test_stim4_y'),
        test_stim5_y: jsPsych.timelineVariable('test_stim5_y'),
        stim1_color: jsPsych.timelineVariable('stim1_color'),
        stim2_color: jsPsych.timelineVariable('stim2_color'),
        stim3_color: jsPsych.timelineVariable('stim3_color'),
        stim4_color: jsPsych.timelineVariable('stim4_color'),
        stim5_color: jsPsych.timelineVariable('stim5_color'),
        stim6_color: jsPsych.timelineVariable('stim6_color'),
        correct_response: jsPsych.timelineVariable('correct_response')
    },
    on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    },      
      timeline_variables: [
{type: 'different', correct_response: 'ArrowRight', mem_stim1_x: mem_x[43][0], mem_stim2_x: mem_x[43][1], mem_stim3_x: mem_x[43][2], mem_stim4_x: mem_x[43][3], mem_stim5_x: mem_x[43][4],
mem_stim1_y: mem_y[43][0], mem_stim2_y: mem_y[43][1], mem_stim3_y: mem_y[43][2], mem_stim4_y: mem_y[43][3], mem_stim5_y: mem_y[43][4],
test_stim1_x: mem_x[43][0], test_stim2_x: mem_x[43][1], test_stim3_x: mem_x[43][2], test_stim4_x: test_x[43][3], test_stim5_x: mem_x[43][4],
test_stim1_y: mem_y[43][0], test_stim2_y: mem_y[43][1], test_stim3_y: mem_y[43][2], test_stim4_y: test_y[43][3], test_stim5_y: mem_y[43][4],
stim1_color: '#0457A0', stim2_color: '#FBF250', stim3_color: '#D40E0E', stim4_color: '#25D9E4', stim5_color: '#4D0063'},
]
}




var change_practice_finish = {
  type: "html-keyboard-response",
  stimulus: "<div style = 'text-align: left;'>" +
  "Great job!<br><br>" +
  "You are now finished practicing the <strong>Change Detection</strong> game.<br><br>" +
  "Next, you will play the actual game.<br><br>" +
  "Remember to focus on the central '+'. Press (&larr;) when a circle has changed, and press (&rarr;) when all circles are in the same position.<br><br>" +
  "The game will last for about three minutes. From now on you will not receive feedback after each response.<br><br>" +
  "Press any key to begin! <br><br></div>",
  choices: jsPsych.ALL_KEYS,
  data: {variable: "practice_finish", task: "change"}
};


var change_break = {
  type: "html-button-response",
  stimulus: "<div style = 'text-align: left;'>" +
  "Nice work!<br><br>" +
  "You are now halfway through the game.<br><br>" +
  "If you need to, you can take a break to give your eyes some rest and regain your focus.<br><br>" +
  "Take as long as you need!<br><br>" +
  "Press 'continue' when you feel ready to continue with the second half of the trials.",
  choices: ['Continue'],
  data: {variable: "break", task: "change"}
};

var change_break_continue = {
  type: "html-keyboard-response",
  stimulus: "<div style = 'text-align: left;'>" +
  "You will now start the second half of the trials<br><br>" +
  "Press any key to continue!",
  choices: jsPsych.ALL_KEYS,
  data: {variable: "break", task: "change"}
};


var change_end = {
  type: "html-button-response",
  stimulus: 
  "Great job!<br><br>" +
  "You are now finished playing the <strong>Change Detection</strong> game.<br><br>" +
  "Click 'finish' to continue.<br><br>",
  choices: ['Finish'],
  data: {variable: "end", task: "change"}
};