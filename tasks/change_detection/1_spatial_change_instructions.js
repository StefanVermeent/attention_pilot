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
var change_instructions = {
  type: 'instructions',
  pages: [
    "<p style = 'text-align: center;'>" + 
      "Each round has three parts.<br><br><br>" +
      "<img width = 600 src = 'img/instructions1.png'></img><br><br>",
      
    "<p style = 'text-align: center;'>" + 
      "Sometimes all the circles will be in the SAME locations as before.<br><br><br>" +
      "<img width = 600 src = 'img/instructions2.png'></img><br><br>",
      
    "<p style = 'text-align: center;'>" + 
      "Other times <strong>one circle</strong> will be in a DIFFERENT <strong>location</strong>.<br><br>" + 
      "For example, the yellow circle below changed locations.<br><br>" +
      "<img width = 600 src = 'img/instructions3.png'></img><br><br>",
      
    "<p style = 'text-align: center;'>" + 
      "Your job is to figure out if the circles are the SAME or DIFFERENT.<br><br>" +
      "<div style = 'float: left;'>If they are the SAME<br>press the LEFT (<) key.</div>" +
      "<div style = 'float: right;'>If they are DIFFERENT<br>press the RIGHT (>) key.</div><br><br><br><br>",
      
    "<p style = 'text-align: center;'>" +
      "The words 'SAME' and 'DIFFERENT' will be on the bottom<br>" +
      "of the screen to remind you.<br><br><br>" +
      "<img width = 190 src = 'img/instructions4.png'></img><br><br>",
      
    "<p style = 'text-align: center;'>" +
      "Click the 'continue' button to practice the<br><strong>Change Detection</strong> game.",
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
  stimulus:    "<p style = 'text-align: center;'>" +
      "You are about to start the <strong>Change Detection</strong> game.<br><br>" +
      "Place your fingers on the left (<) and right (>) arrow keys.<br><br>" +
      "When you are ready, press any key to start the practice round.",
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