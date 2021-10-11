var flanker_present_arrows = {
  type: "html-keyboard-response",
  stimulus: jsPsych.timelineVariable('stim'),
  choices: ['ArrowLeft', 'ArrowRight'],
  trial_duration: 2000,
  data: {
    variable: 'flanker_test',
    task: 'flanker',
    location: jsPsych.timelineVariable('location'),
    stimtype: jsPsych.timelineVariable('stimtype'),
    correct_response: jsPsych.timelineVariable('correct_response')
  },
  on_finish: function(data) { {
    if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
      data.correct = true;
    } else {
      data.correct = false;
    }
  }
  }
}


var flanker_test_procedure = {
  timeline: [flanker_fixation, flanker_present_arrows],
  timeline_variables: [
    {location: 'top',    correct_response: 'ArrowLeft', stimtype: 'congruent_left',    stim: location_stim(up='&larr;&larr;&larr;&larr;&larr;', down=null)},
    {location: 'top',    correct_response: 'ArrowRight', stimtype: 'congruent_right',   stim: location_stim(up='&rarr;&rarr;&rarr;&rarr;&rarr;', down=null)},
    {location: 'top',    correct_response: 'ArrowLeft', stimtype: 'incongruent_left',  stim: location_stim(up='&rarr;&rarr;&larr;&rarr;&rarr;', down=null)},
    {location: 'top',    correct_response: 'ArrowRight', stimtype: 'incongruent_right', stim: location_stim(up='&larr;&larr;&rarr;&larr;&larr;', down=null)},
    {location: 'bottom', correct_response: 'ArrowLeft', stimtype: 'congruent_left',    stim: location_stim(up=null, down='&larr;&larr;&larr;&larr;&larr;')},
    {location: 'bottom', correct_response: 'ArrowRight', stimtype: 'congruent_right',   stim: location_stim(up=null, down='&rarr;&rarr;&rarr;&rarr;&rarr;')},
    {location: 'bottom', correct_response: 'ArrowLeft', stimtype: 'incongruent_left',  stim: location_stim(up=null, down='&rarr;&rarr;&larr;&rarr;&rarr;')},
    {location: 'bottom', correct_response: 'ArrowRight', stimtype: 'incongruent_right', stim: location_stim(up=null, down='&larr;&larr;&rarr;&larr;&larr;')},
  ],
  randomize_order: true,
  repetitions: 8,
};