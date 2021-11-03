var cueing_instruction_image1 = '../img/cueing_instructions1.png'
var cueing_instruction_image2 = '../img/cueing_instructions2.png'
var cueing_instruction_image3 = '../img/cueing_instructions3.png'

//-------------------- Welcome
var cueing_welcome = {
  type: 'instructions',
  pages: [
    "Welcome to the <b>Arrow Detection</b> game!"
  ],
  show_clickable_nav: true,
  allow_backward: true,
  key_forward: -1,
  key_backward: -1,
  button_label_next: "continue",
  data: {variable: 'welcome', task: "cueing"}
};

//-------------------- Instructions
var cueing_instructions = {
  type: 'instructions',
  pages: [
    "<p style = 'text-align: center;'>" + 
      "In this game, an arrow will appear somewhere on the screen.<br><br>" +
      "Your job is to decide which way the arrow is pointing.<br><br><br>",
      
    "<p style = 'text-align: center;'>" + 
      "Each round has three parts.<br><br><br>" +
      "<img width = 600 src = " + cueing_instruction_image1 + "></img><br><br>",
    
    "<p style = 'text-align: center;'>" +   
      "Sometimes, the * will appear in the middle of the screen.<br><br>" +
      "Other times, it will appear <strong>in the same place as the arrow.</strong><br><br><br>" +
      "<img width = 490 src = " + cueing_instruction_image2 + "></img><br><br><br>",
      
    "<p style = 'text-align: center;'>" + 
      "When the arrow appears, press the arrow key that matches the way it points.<br><br>" +
      "<div style = 'float: left;'>If it points LEFT<br>press the LEFT (&larr;) key.</div>" +
      "<div style = 'float: right;'>If it points RIGHT<br>press the RIGHT (&rarr;) key.</div><br><br><br><br>" + 
      "In the example below, you would press the left key.<br><br><br>" + 
      "<img width = 160 src = " + cueing_instruction_image3 + "></img><br><br><br>",
      
   "<p style = 'text-align: center;'>" +    
      "Click 'continue' button to practice the game.",
      
  ],
  show_clickable_nav: true,
  allow_backward: true,
  key_forward: -1,
  key_backward: -1,
  button_label_next: "continue",
  button_label_previous: "go back",
  data: {variable: "instructions", task: "cueing"}
};


//-------------------- Practice

var cueing_practice_start = {
  type: 'html-keyboard-response',
  stimulus:    "<p style = 'text-align: center;'>" +
      "You will practice the game <strong>8 times</strong>.<br><br>" +
      "Place your fingers on the left (&larr;) and right (&rarr;) arrow keys.<br><br>" +
      "When you are ready to practice, press any key to start.",
  choices: jsPsych.ALL_KEYS,
  data: {variable: "practice_start", task: "cueing"}
};


var cueing_practice1 = {
    timeline: [
      {
        type: 'psychophysics',
        stimuli: [
        fixation,
        cue,
        target,
        ],
          choices: ["ArrowLeft", "ArrowRight"],
          response_start_time: 1250,
          canvas_width: 900,
          canvas_height: 900,
          background_color: '#ffffff',
          data: {
            variable: 'practice',
            task: 'cueing',
            condition: jsPsych.timelineVariable('type'),
            target: jsPsych.timelineVariable('target'),
            cue_x: jsPsych.timelineVariable('cue_x'),
            cue_y: jsPsych.timelineVariable('cue_y'),
            target_x: jsPsych.timelineVariable('target_x'),
            target_y: jsPsych.timelineVariable('target_y'),
            correct_response: jsPsych.timelineVariable('correct_response')
          },
      }
      ],
      on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    }, 
      timeline_variables: [{type: 'cued', target: '\u2190', correct_response: 'ArrowLeft', cue_x: location2_x, cue_y: location2_y, target_x: location2_x, target_y: location2_y}],
}

var cueing_practice2 = {
    timeline: [
      {
        type: 'psychophysics',
        stimuli: [
        fixation,
        cue,
        target,
        ],
          choices: ["ArrowLeft", "ArrowRight"],
          response_start_time: 1250,
          canvas_width: 900,
          canvas_height: 900,
          background_color: '#ffffff',
          data: {
            variable: 'practice',
            task: 'cueing',
            condition: jsPsych.timelineVariable('type'),
            target: jsPsych.timelineVariable('target'),
            cue_x: jsPsych.timelineVariable('cue_x'),
            cue_y: jsPsych.timelineVariable('cue_y'),
            target_x: jsPsych.timelineVariable('target_x'),
            target_y: jsPsych.timelineVariable('target_y'),
            correct_response: jsPsych.timelineVariable('correct_response')
          },
      },
      ],
      on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    }, 
      timeline_variables: [{type: 'neutral', target: '\u2192', correct_response: 'ArrowRight', cue_x: 0, cue_y: 12, target_x: location8_x, target_y: location8_y}],
}

var cueing_practice3 = {
    timeline: [
      {
        type: 'psychophysics',
        stimuli: [
        fixation,
        cue,
        target,
        ],
          choices: ["ArrowLeft", "ArrowRight"],
          response_start_time: 1250,
          canvas_width: 900,
          canvas_height: 900,
          background_color: '#ffffff',
          data: {
            variable: 'practice',
            task: 'cueing',
            condition: jsPsych.timelineVariable('type'),
            target: jsPsych.timelineVariable('target'),
            cue_x: jsPsych.timelineVariable('cue_x'),
            cue_y: jsPsych.timelineVariable('cue_y'),
            target_x: jsPsych.timelineVariable('target_x'),
            target_y: jsPsych.timelineVariable('target_y'),
            correct_response: jsPsych.timelineVariable('correct_response')
          },
      },
      ],
      on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    }, 
      timeline_variables: [{type: 'cued', target: '\u2192', correct_response: 'ArrowRight', cue_x: location3_x, cue_y: location3_y, target_x: location3_x, target_y: location3_y}],
}

var cueing_practice4 = {
    timeline: [
      {
        type: 'psychophysics',
        stimuli: [
        fixation,
        cue,
        target,
        ],
          choices: ["ArrowLeft", "ArrowRight"],
          response_start_time: 1250,
          canvas_width: 900,
          canvas_height: 900,
          background_color: '#ffffff',
          data: {
            variable: 'practice',
            task: 'cueing',
            condition: jsPsych.timelineVariable('type'),
            target: jsPsych.timelineVariable('target'),
            cue_x: jsPsych.timelineVariable('cue_x'),
            cue_y: jsPsych.timelineVariable('cue_y'),
            target_x: jsPsych.timelineVariable('target_x'),
            target_y: jsPsych.timelineVariable('target_y'),
            correct_response: jsPsych.timelineVariable('correct_response')
          },
      },
      ],
      on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    }, 
      timeline_variables: [{type: 'neutral', target: '\u2190', correct_response: 'ArrowLeft', cue_x: 0, cue_y: 12, target_x: location4_x, target_y: location4_y}],
}

var cueing_practice5 = {
    timeline: [
      {
        type: 'psychophysics',
        stimuli: [
        fixation,
        cue,
        target,
        ],
          choices: ["ArrowLeft", "ArrowRight"],
          response_start_time: 1250,
          canvas_width: 900,
          canvas_height: 900,
          background_color: '#ffffff',
          data: {
            variable: 'practice',
            task: 'cueing',
            condition: jsPsych.timelineVariable('type'),
            target: jsPsych.timelineVariable('target'),
            cue_x: jsPsych.timelineVariable('cue_x'),
            cue_y: jsPsych.timelineVariable('cue_y'),
            target_x: jsPsych.timelineVariable('target_x'),
            target_y: jsPsych.timelineVariable('target_y'),
            correct_response: jsPsych.timelineVariable('correct_response')
          },
      },
      ],
      on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    }, 
      timeline_variables: [{type: 'neutral', target: '\u2192', correct_response: 'ArrowRight', cue_x: 0, cue_y: 12, target_x: location1_x, target_y: location1_y}],
}

var cueing_practice6 = {
    timeline: [
      {
        type: 'psychophysics',
        stimuli: [
        fixation,
        cue,
        target,
        ],
          choices: ["ArrowLeft", "ArrowRight"],
          response_start_time: 1250,
          canvas_width: 900,
          canvas_height: 900,
          background_color: '#ffffff',
          data: {
            variable: 'practice',
            task: 'cueing',
            condition: jsPsych.timelineVariable('type'),
            target: jsPsych.timelineVariable('target'),
            cue_x: jsPsych.timelineVariable('cue_x'),
            cue_y: jsPsych.timelineVariable('cue_y'),
            target_x: jsPsych.timelineVariable('target_x'),
            target_y: jsPsych.timelineVariable('target_y'),
            correct_response: jsPsych.timelineVariable('correct_response')
          },
      },
      ],
      on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    }, 
      timeline_variables: [{type: 'cued', target: '\u2192', correct_response: 'ArrowRight', cue_x: location7_x, cue_y: location7_y, target_x: location7_x, target_y: location7_y}],
}

var cueing_practice7 = {
    timeline: [
      {
        type: 'psychophysics',
        stimuli: [
        fixation,
        cue,
        target,
        ],
          choices: ["ArrowLeft", "ArrowRight"],
          response_start_time: 1250,
          canvas_width: 900,
          canvas_height: 900,
          background_color: '#ffffff',
          data: {
            variable: 'practice',
            task: 'cueing',
            condition: jsPsych.timelineVariable('type'),
            target: jsPsych.timelineVariable('target'),
            cue_x: jsPsych.timelineVariable('cue_x'),
            cue_y: jsPsych.timelineVariable('cue_y'),
            target_x: jsPsych.timelineVariable('target_x'),
            target_y: jsPsych.timelineVariable('target_y'),
            correct_response: jsPsych.timelineVariable('correct_response')
          },
      },
      ],
      on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    }, 
      timeline_variables: [{type: 'cued', target: '\u2190', correct_response: 'ArrowLeft', cue_x: location5_x, cue_y: location5_y, target_x: location5_x, target_y: location5_y}],
}

var cueing_practice8 = {
    timeline: [
      {
        type: 'psychophysics',
        stimuli: [
        fixation,
        cue,
        target,
        ],
          choices: ["ArrowLeft", "ArrowRight"],
          response_start_time: 1250,
          canvas_width: 900,
          canvas_height: 900,
          background_color: '#ffffff',
          data: {
            variable: 'practice',
            task: 'cueing',
            condition: jsPsych.timelineVariable('type'),
            target: jsPsych.timelineVariable('target'),
            cue_x: jsPsych.timelineVariable('cue_x'),
            cue_y: jsPsych.timelineVariable('cue_y'),
            target_x: jsPsych.timelineVariable('target_x'),
            target_y: jsPsych.timelineVariable('target_y'),
            correct_response: jsPsych.timelineVariable('correct_response')
          },
      },
      ],
      on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    }, 
      timeline_variables: [{type: 'neutral', target: '\u2190', correct_response: 'ArrowLeft', cue_x: 0, cue_y: 12, target_x: location6_x, target_y: location6_y}],
}


// Finish Practice trials
var cueing_practice_finish = {
  type: "html-keyboard-response",
  stimulus: "<p style = 'text-align: center;'>" +
  "Great job!<br><br>" +
  "You will now play the actual <strong>Arrow Detection</strong> game.<br><br>" +
  "The game will last for about three minutes. From now on you will not receive feedback after each response.<br><br>" +
  "Press any key to begin! <br><br>",
  choices: jsPsych.ALL_KEYS,
  data: {variable: "practice_finish", task: "cueing"}
};

var cueing_end = {
  type: "html-button-response",
  stimulus: 
  "Great job!<br><br>" +
  "You are now finished playing the <strong>Arrow Detection</strong> game.<br><br>" +
  "Click 'finish' to continue.<br><br>",
  choices: ['Finish'],
  data: {variable: "end", task: "cueing"}
};
