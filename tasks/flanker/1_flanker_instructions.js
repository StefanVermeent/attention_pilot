//-------------------- Welcome
var flanker_welcome = {
  type: 'instructions',
  pages: [
    "Welcome to the <b>Flanker</b> game!"
  ],
  show_clickable_nav: true,
  allow_backward: true,
  key_forward: -1,
  key_backward: -1,
  button_label_next: "continue",
  data: {variable: 'welcome', task: "flanker"}
};

//-------------------- Instructions
var flanker_instructions = {
  type: 'instructions',
  pages: [
    "<p style = 'text-align: left; padding: 0 20% 0 20%;'>"+ 
      "You will be playing the <strong>Flanker</strong> game.<br><br>" + 
      "In this game you will see groups of five arrows pointing left or right (e.g., &larr;&larr;&larr;&larr;&larr;, or &rarr;&rarr;&rarr;&rarr;&rarr;).<br>" +
      "Your job is to indicate the direction of the <strong>central</strong> arrow, while ignoring the others.<br><br>" +
      "On some rounds, the surrounding arrows will point in the same direction as the central arrow. On other rounds, they will point in the opposite direction.<br><br>" +
      "If the central arrow points left, press the (&larr;) arrow. If the central arrow points right, press the (&rarr;) arrow.<br><br>" +
      "Please try to respond as quickly and accurately as possible.<br><br>"+
      
      "Click the 'continue' button to practice the <strong>Flanker</strong> game.<br></p>"
  ],
  show_clickable_nav: true,
  allow_backward: true,
  key_forward: -1,
  key_backward: -1,
  button_label_next: "continue",
  button_label_previous: "go back",
  data: {variable: "instructions", task: "flanker"}
};



//-------------------- Practice

var flanker_practice_start = {
  type: 'html-keyboard-response',
  stimulus: "You are about to start the practice round.<br><br><strong>Press any key when you are ready!</strong><br><br>",
  choices: jsPsych.ALL_KEYS,
  data: {variable: "practice_start", task: "flanker"}
};


var flanker_practice = {
    type: "html-keyboard-response",
    stimulus: jsPsych.timelineVariable('practice_stim'),
    choices: ['ArrowLeft', 'ArrowRight'],
    data: {
      variable: 'practice',
      task: 'flanker',
      location: jsPsych.timelineVariable('location'),
      stimtype: jsPsych.timelineVariable('stimtype')
    },
    on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    }
};


var flanker_practice_procedure = {
  timeline: [flanker_fixation, flanker_practice, feedback],
  timeline_variables: [
    {location: 'top',    correct_response: 'ArrowLeft',  stimtype: 'congruent_left',    practice_stim: location_stim(up='&larr;&larr;&larr;&larr;&larr;', down=null)},
    {location: 'top',    correct_response: 'ArrowRight', stimtype: 'congruent_right',   practice_stim: location_stim(up='&rarr;&rarr;&rarr;&rarr;&rarr;', down=null)},
    {location: 'top',    correct_response: 'ArrowLeft',  stimtype: 'incongruent_left',  practice_stim: location_stim(up='&rarr;&rarr;&larr;&rarr;&rarr;', down=null)},
    {location: 'top',    correct_response: 'ArrowRight', stimtype: 'incongruent_right', practice_stim: location_stim(up='&larr;&larr;&rarr;&larr;&larr;', down=null)},
    {location: 'bottom', correct_response: 'ArrowLeft',  stimtype: 'congruent_left',    practice_stim: location_stim(up=null, down='&larr;&larr;&larr;&larr;&larr;')},
    {location: 'bottom', correct_response: 'ArrowRight', stimtype: 'congruent_right',   practice_stim: location_stim(up=null, down='&rarr;&rarr;&rarr;&rarr;&rarr;')},
    {location: 'bottom', correct_response: 'ArrowLeft',  stimtype: 'incongruent_left',  practice_stim: location_stim(up=null, down='&rarr;&rarr;&larr;&rarr;&rarr;')},
    {location: 'bottom', correct_response: 'ArrowRight', stimtype: 'incongruent_right', practice_stim: location_stim(up=null, down='&larr;&larr;&rarr;&larr;&larr;')},
  ],
  randomize_order: true,
  repetitions: 1,
};



// Finish Practice trials
var flanker_practice_finish = {
  type: "html-keyboard-response",
  stimulus: "<div style = 'text-align: left;'>" +
  "Great job!<br><br>" +
  "You are now finished practicing the <strong>Flanker</strong> game.<br><br>" +
  "Next, you will play the actual game.<br><br>" +
  "In the real game, you will only have 2 seconds to respond.<br>" +
  "Remember to respond only to the direction of the <strong>central</strong> arrow.<br><br>" +
  "The game will last for about three minutes. From now on you will not receive feedback after each response.<br><br>" +
  "Press any key to begin! <br><br></div>",
  choices: jsPsych.ALL_KEYS,
  data: {variable: "practice_finish", task: "flanker"}
};

var flanker_end = {
  type: "html-button-response",
  stimulus: 
  "Great job!<br><br>" +
  "You are now finished playing the <strong>Flanker</strong> game.<br><br>" +
  "Click 'finish' to continue.<br><br>",
  choices: ['Finish'],
  data: {variable: "end", task: "flanker"}
};