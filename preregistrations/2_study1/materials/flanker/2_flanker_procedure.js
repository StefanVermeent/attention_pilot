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
    {location: 'top',    correct_response: 'ArrowLeft',  stimtype: 'congruent_left1',    stim: location_stim(up="&nwarr;&nwarr;&nwarr;&swarr;&swarr;", down=null)},
    {location: 'top',    correct_response: 'ArrowRight', stimtype: 'congruent_left2',    stim: location_stim(up="&swarr;&swarr;&nwarr;&nwarr;&nwarr;", down=null)},
    {location: 'top',    correct_response: 'ArrowLeft',  stimtype: 'congruent_left3',    stim: location_stim(up="&nwarr;&nwarr;&swarr;&swarr;&swarr;", down=null)},
    {location: 'top',    correct_response: 'ArrowRight', stimtype: 'congruent_left4',    stim: location_stim(up="&swarr;&swarr;&swarr;&nwarr;&nwarr;", down=null)},
    {location: 'top',    correct_response: 'ArrowLeft',  stimtype: 'congruent_right1',   stim: location_stim(up="&nwarr;&swarr;&nwarr;&nwarr;&swarr;", down=null)},
    {location: 'top',    correct_response: 'ArrowRight', stimtype: 'congruent_right2',   stim: location_stim(up="&swarr;&nwarr;&nwarr;&swarr;&nwarr;", down=null)},
    {location: 'top',    correct_response: 'ArrowLeft',  stimtype: 'congruent_right3',   stim: location_stim(up="&nwarr;&swarr;&swarr;&nwarr;&swarr;", down=null)},
    {location: 'top',    correct_response: 'ArrowRight', stimtype: 'congruent_right4',   stim: location_stim(up="&swarr;&nwarr;&swarr;&swarr;&nwarr;", down=null)},
    {location: 'top',    correct_response: 'ArrowLeft',  stimtype: 'incongruent_left1',  stim: location_stim(up="&nearr;&nearr;&nearr;&searr;&searr;", down=null)},
    {location: 'top',    correct_response: 'ArrowRight', stimtype: 'incongruent_left2',  stim: location_stim(up="&searr;&searr;&nearr;&nearr;&nearr;", down=null)},
    {location: 'top',    correct_response: 'ArrowLeft',  stimtype: 'incongruent_left3',  stim: location_stim(up="&nearr;&nearr;&searr;&searr;&searr;", down=null)},
    {location: 'top',    correct_response: 'ArrowRight', stimtype: 'incongruent_left4',  stim: location_stim(up="&searr;&searr;&searr;&nearr;&nearr;", down=null)},
    {location: 'top',    correct_response: 'ArrowLeft',  stimtype: 'incongruent_right1',  stim: location_stim(up="&nearr;&searr;&nearr;&nearr;&searr;", down=null)},
    {location: 'top',    correct_response: 'ArrowRight', stimtype: 'incongruent_right2',  stim: location_stim(up="&searr;&nearr;&nearr;&searr;&nearr;", down=null)},
    {location: 'top',    correct_response: 'ArrowLeft',  stimtype: 'incongruent_right3',  stim: location_stim(up="&nearr;&searr;&searr;&nearr;&searr;", down=null)},
    {location: 'top',    correct_response: 'ArrowRight', stimtype: 'incongruent_right4',  stim: location_stim(up="&searr;&nearr;&searr;&searr;&nearr;", down=null)},
    
  
    {location: 'bottom', correct_response: 'ArrowLeft',  stimtype: 'congruent_left1', stim: location_stim(up=null, down="&nwarr;&nwarr;&nwarr;&swarr;&swarr;")},
    {location: 'bottom', correct_response: 'ArrowRight', stimtype: 'congruent_left2', stim: location_stim(up=null, down="&swarr;&swarr;&nwarr;&nwarr;&nwarr;")},
    {location: 'bottom', correct_response: 'ArrowLeft',  stimtype: 'congruent_left3', stim: location_stim(up=null, down="&nwarr;&nwarr;&swarr;&swarr;&swarr;")},
    {location: 'bottom', correct_response: 'ArrowRight', stimtype: 'congruent_left4', stim: location_stim(up=null, down="&swarr;&swarr;&swarr;&nwarr;&nwarr;")},
    {location: 'bottom', correct_response: 'ArrowLeft',  stimtype: 'congruent_right1',  stim: location_stim(up=null, down="&nwarr;&swarr;&nwarr;&nwarr;&swarr;")},
    {location: 'bottom', correct_response: 'ArrowRight', stimtype: 'congruent_right2',  stim: location_stim(up=null, down="&swarr;&nwarr;&nwarr;&swarr;&nwarr;")},
    {location: 'bottom', correct_response: 'ArrowLeft',  stimtype: 'congruent_right3',  stim: location_stim(up=null, down="&nwarr;&swarr;&swarr;&nwarr;&swarr;")},
    {location: 'bottom', correct_response: 'ArrowRight', stimtype: 'congruent_right4',  stim: location_stim(up=null, down="&swarr;&nwarr;&swarr;&swarr;&nwarr;")},
    {location: 'bottom', correct_response: 'ArrowLeft',  stimtype: 'incongruent_left1',  stim: location_stim(up=null, down="&nearr;&nearr;&nearr;&searr;&searr;")},
    {location: 'bottom', correct_response: 'ArrowRight', stimtype: 'incongruent_left2',  stim: location_stim(up=null, down="&searr;&searr;&nearr;&nearr;&nearr;")},
    {location: 'bottom', correct_response: 'ArrowLeft',  stimtype: 'incongruent_left3',  stim: location_stim(up=null, down="&nearr;&nearr;&searr;&searr;&searr;")},
    {location: 'bottom', correct_response: 'ArrowRight', stimtype: 'incongruent_left4',  stim: location_stim(up=null, down="&searr;&searr;&searr;&nearr;&nearr;")},
    {location: 'bottom', correct_response: 'ArrowLeft',  stimtype: 'incongruent_right1', stim: location_stim(up=null, down="&nearr;&searr;&nearr;&nearr;&searr;")},
    {location: 'bottom', correct_response: 'ArrowRight', stimtype: 'incongruent_right2', stim: location_stim(up=null, down="&searr;&nearr;&nearr;&searr;&nearr;")},
    {location: 'bottom', correct_response: 'ArrowLeft',  stimtype: 'incongruent_right3', stim: location_stim(up=null, down="&nearr;&searr;&searr;&nearr;&searr;")},
    {location: 'bottom', correct_response: 'ArrowRight', stimtype: 'incongruent_right4', stim: location_stim(up=null, down="&searr;&nearr;&searr;&searr;&nearr;")},
  ],
  randomize_order: true,
  repetitions: 8,
};