var flanker_present_arrows = {
  type: "html-keyboard-response",
  stimulus: jsPsych.timelineVariable('stim'),
  choices: ['ArrowLeft', 'ArrowRight'],
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
};


var flanker_test_procedure = {
  timeline: [flanker_fixation, flanker_present_arrows],
  timeline_variables: [
    {location: 'top',    correct_response: 'ArrowLeft',  stimtype: 'congruent_left',     stim: set_arrows(angles = [335, 25, 205, 335, 335], up=true, down=false)},
    {location: 'top',    correct_response: 'ArrowRight', stimtype: 'congruent_right',    stim: set_arrows(angles = [335, 25, 205, 335, 335], up=true, down=false)},
    {location: 'top',    correct_response: 'ArrowRight', stimtype: 'incongruent_left',   stim: set_arrows(angles = [335, 25, 205, 335, 335], up=true, down=false)},
    {location: 'top',    correct_response: 'ArrowLeft',  stimtype: 'incongruent_right',  stim: set_arrows(angles = [335, 25, 205, 335, 335], up=true, down=false)},
    
  

    {location: 'bottom', correct_response: 'ArrowRight', stimtype: 'congruent_left',     stim: set_arrows(angles = [335, 25, 205, 335, 335], up=true, down=false)},
    {location: 'bottom', correct_response: 'ArrowLeft',  stimtype: 'congruent_right',    stim: set_arrows(angles = [335, 25, 205, 335, 335], up=true, down=false)},
    {location: 'bottom', correct_response: 'ArrowLeft',  stimtype: 'incongruent_left',   stim: set_arrows(angles = [335, 25, 205, 335, 335], up=true, down=false)},
    {location: 'bottom', correct_response: 'ArrowLeft',  stimtype: 'incongruent_right',  stim: set_arrows(angles = [335, 25, 205, 335, 335], up=true, down=false)},
   
  ],
  randomize_order: true,
  repetitions: 8,
};