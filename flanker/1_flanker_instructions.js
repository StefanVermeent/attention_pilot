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

timeline.push(flanker_welcome)

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
  data: {variable: "instructions", task: "abs_updating"}
};

timeline.push(flanker_instructions)


//-------------------- Practice

var flanker_practice_start = {
  type: 'html-keyboard-response',
  stimulus: "You are about to start the practice round.<br><br><strong>Press any key when you are ready!</strong><br><br>",
  choices: jsPsych.ALL_KEYS,
  data: {variable: "practice_start", task: "flanker"}
};

timeline.push(flanker_practice_start)


var flanker_practice = {
    type: "html-keyboard-response",
    stimulus: jsPsych.timelineVariable('practice_stim'),
    choices: ['ArrowLeft', 'ArrowRight'],
    data: {
      task: 'response',
      location: jsPsych.timelineVariable('location'),
      stimtype: jsPsych.timelineVariable('stimtype')
    },
    on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('stimtype', true))) {
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
}

var practice_procedure = {
  timeline: [fixation, flanker_practice, feedback],
  timeline_variables: [
    {location: 'top',    stimtype: 'ArrowLeft',    practice_stim: location_stim(up='&larr;&larr;&larr;&larr;&larr;', down=null)},
    {location: 'top',    stimtype: 'ArrowRight',   practice_stim: location_stim(up='&rarr;&rarr;&rarr;&rarr;&rarr;', down=null)},
    {location: 'top',    stimtype: 'ArrowLeft',    practice_stim: location_stim(up='&rarr;&rarr;&larr;&rarr;&rarr;', down=null)},
    {location: 'top',    stimtype: 'ArrowRight',   practice_stim: location_stim(up='&larr;&larr;&rarr;&larr;&larr;', down=null)},
    {location: 'bottom', stimtype: 'ArrowLeft',    practice_stim: location_stim(up=null, down='&larr;&larr;&larr;&larr;&larr;')},
    {location: 'bottom', stimtype: 'ArrowRight',   practice_stim: location_stim(up=null, down='&rarr;&rarr;&rarr;&rarr;&rarr;')},
    {location: 'bottom', stimtype: 'ArrowLeft',    practice_stim: location_stim(up=null, down='&rarr;&rarr;&larr;&rarr;&rarr;')},
    {location: 'bottom', stimtype: 'ArrowRight',   practice_stim: location_stim(up=null, down='&larr;&larr;&rarr;&larr;&larr;')},
  ],
  randomize_order: true,
  repetitions: 1,
};

timeline.push(practice_procedure);


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

timeline.push(flanker_practice_finish)