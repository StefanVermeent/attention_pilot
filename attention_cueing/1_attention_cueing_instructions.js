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

timeline.push(cueing_welcome)

//-------------------- Instructions
var cueing_instructions1 = {
  type: 'instructions',
  pages: [
    "<p style = 'text-align: left; padding: 0 20% 0 20%;'>"+ 
      "You will be playing the <strong>Arrow Detection</strong> game.<br><br>" + 
      "In this game, a left arrow (&larr;) or right arrow (&rarr;) will be presented in one of nine locations around the center of the screen.<br>" +
      "Your job is to indicate the direction of the arrow.<br>" +
      "If the arrow points left, press the (&larr;) arrow. If the arrow points right, press the (&rarr;) arrow.<br><br><br>" +
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

timeline.push(cueing_instructions1)


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

timeline.push(cueing_instructions2)


//-------------------- Practice

// Remove cursor during cueing trials
timeline.push(cursor_off)

var cueing_practice_start = {
  type: 'html-keyboard-response',
  stimulus: "You are about to start the practice round.<br><br><strong>Press any key when you are ready!</strong><br><br>",
  choices: jsPsych.ALL_KEYS,
  data: {variable: "practice_start", task: "cueing"}
};

timeline.push(cueing_practice_start)


var practice_cue_presentation = {
  type: 'html-keyboard-response',
  stimulus: jsPsych.timelineVariable('cue'),
  choices: jsPsych.NO_KEYS,
  trial_duration: 250,
  data: {variable: 'practice_cue', task: 'cueing'}
}
  
var practice_target_presentation = {
  type: 'html-keyboard-response',
  stimulus: jsPsych.timelineVariable('target'),
  choices: ['ArrowLeft', 'ArrowRight'],
  trial_duration: 3000,
  data: {
    variable: 'practice_target', task: 'cueing',
    cue: jsPsych.timelineVariable('cue'),
    target: jsPsych.timelineVariable('target')
  },
    on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    }
};

var feedback = {
  type: 'html-keyboard-response',
  stimulus: function(){
    var last_trial_correct = jsPsych.data.get().last(1).values()[0].correct;
    if(last_trial_correct){
      return "<p style = 'color:green;font-size:40px'>Correct!</p>"; // the parameter value has to be returned from the function
    } else {
      return "<p style = 'color:red;font-size:40px'>Incorrect!</p>"; // the parameter value has to be returned from the function
    }
  },
  trial_duration: 2000,
  data: {variable: 'feedback'}
}

var practice_procedure = {
    timeline: [fixation, practice_cue_presentation, practice_target_presentation, feedback],
    timeline_variables: [
      // Cued left arrow trials
      {cue: cue_topleft,      target: leftarrow_topleft,       correct_response: 'ArrowLeft'},
      {cue: cue_middleright,  target: leftarrow_middleright,   correct_response: 'ArrowLeft'},
      // Cued right arrow trials
      {cue: cue_middleleft,   target: rightarrow_middleleft,   correct_response: 'ArrowRight'},
      {cue: cue_topright,     target: rightarrow_topright,     correct_response: 'ArrowRight'},
      // Uncued left arrow trials
      {cue: cue_neutral,      target: leftarrow_bottommiddle,  correct_response: 'ArrowLeft'},
      {cue: cue_neutral,      target: leftarrow_topmiddle,     correct_response: 'ArrowLeft'},
      // Uncued right arrow trials
      {cue: cue_neutral,      target: rightarrow_bottomleft,   correct_response: 'ArrowRight'},
      {cue: cue_neutral,      target: rightarrow_bottomright,  correct_response: 'ArrowRight'},
    ],
    randomize_order: true,
    repetitions: 1
  }

timeline.push(practice_procedure);


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

timeline.push(cueing_practice_finish)