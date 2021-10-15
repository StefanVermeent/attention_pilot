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
var cueing_instructions1 = {
  type: 'instructions',
  pages: [
    "<p style = 'text-align: left; padding: 0 20% 0 20%;'>"+ 
      "You will be playing the <strong>Arrow Detection</strong> game.<br><br>" + 
      "In this game, a left arrow (<) or right arrow (>) will be presented in one of nine locations around the center of the screen.<br>" +
      "Your job is to indicate the direction of the arrow.<br>" +
      "If the arrow points left, press the (&larr;) arrow on your keyboard. If the arrow points right, press the (&rarr;) arrow.<br><br><br>" +
      "Click the 'continue' button for the next page of instructions.</p>"
  ],
  show_clickable_nav: true,
  allow_backward: true,
  key_forward: -1,
  key_backward: -1,
  button_label_next: "continue",
  button_label_previous: "go back",
  data: {variable: "instructions", task: "cueing"}
};


var cueing_instructions2 = {
  type: 'instructions',
  pages: [
    "<p style = 'text-align: left; padding: 0 20% 0 20%;'>"+ 
    "Before the arrow is presented, you will briefly see an '*' on the screen." +
    "Sometimes the '*' appears in the same location as where the arrow will appear.<br>" +
    "Other times, the '*' appears in the center of the screen and does not predict the location of the arrow<br><br>" +
    "Regardless of the location of the '*', your job is to indicate the direction of the arrow using the &larr; and &rarr; keys on your keyboard.<br><br>" +
    "Please try to respond as fast and accurate as possible.<br><br><br>" +
    "Click the 'continue' button to practice the <strong>Arrow Detection</strong> game.<br></p>"
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
  stimulus: "You are about to start the practice round.<br><br><strong>Press any key when you are ready!</strong><br><br>",
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
  stimulus: "<div style = 'text-align: left;'>" +
  "Great job!<br><br>" +
  "You are now finished practicing the <strong>Arrow Detection</strong> game.<br><br>" +
  "Next, you will play the actual game.<br><br>" +
  "Remember to indicate the direction of the arrow as fast and accurate as possible.<br><br>" +
  "The game will last for about three minutes. From now on you will not receive feedback after each response.<br><br>" +
  "Press any key to begin! <br><br></div>",
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
