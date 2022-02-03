var flanker_present_arrows = {
  type: "html-keyboard-response",
  stimulus: jsPsych.timelineVariable('stim'),
  choices: ['ArrowLeft', 'ArrowRight'],
  data: {
    variable: 'flanker_test',
    task: 'flanker',
    location: jsPsych.timelineVariable('location'),
    congruency: jsPsych.timelineVariable('congruency'),
    correct_response: jsPsych.timelineVariable('correct_response'),
    condition: jsPsych.timelineVariable('condition')
  },
  on_finish: function(data) { 
    if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
      data.correct = true;
    } else {
      data.correct = false;
    }
  }
};


var flanker_test_procedure = {
  timeline: [flanker_fixation, flanker_present_arrows],
  timeline_variables: [
{congruency: 'congruent', condition: 'standard', location: 'up', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'congruent', condition: 'standard', location: 'down', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'standard', location: 'up', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'standard', location: 'down', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'congruent', condition: 'standard', location: 'up', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&rarr;', size = 40, padding = 0)},
{congruency: 'congruent', condition: 'standard', location: 'down', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&rarr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'standard', location: 'up', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&rarr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'standard', location: 'down', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&rarr;', size = 40, padding = 0)},
{congruency: 'congruent', condition: 'standard', location: 'up', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'congruent', condition: 'standard', location: 'down', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'standard', location: 'up', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'standard', location: 'down', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'congruent', condition: 'standard', location: 'up', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&rarr;', size = 40, padding = 0)},
{congruency: 'congruent', condition: 'standard', location: 'down', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&rarr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'standard', location: 'up', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&rarr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'standard', location: 'down', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&rarr;', size = 40, padding = 0)},


{congruency: 'congruent', condition: 'standard', location: 'up', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&larr;',       size = 60, padding = 20)},
{congruency: 'congruent', condition: 'standard', location: 'down', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&larr;',   size = 60, padding = 20)},
{congruency: 'incongruent', condition: 'standard', location: 'up', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&larr;',     size = 60, padding = 20)},
{congruency: 'incongruent', condition: 'standard', location: 'down', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&larr;', size = 60, padding = 20)},
{congruency: 'congruent', condition: 'standard', location: 'up', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&rarr;',      size = 60, padding = 20)},
{congruency: 'congruent', condition: 'standard', location: 'down', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&rarr;',  size = 60, padding = 20)},
{congruency: 'incongruent', condition: 'standard', location: 'up', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&rarr;',    size = 60, padding = 20)},
{congruency: 'incongruent', condition: 'standard', location: 'down', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&rarr;',size = 60, padding = 20)},
{congruency: 'congruent', condition: 'standard', location: 'up', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&larr;',       size = 60, padding = 20)},
{congruency: 'congruent', condition: 'standard', location: 'down', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&larr;',   size = 60, padding = 20)},
{congruency: 'incongruent', condition: 'standard', location: 'up', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&larr;',     size = 60, padding = 20)},
{congruency: 'incongruent', condition: 'standard', location: 'down', correct_response: 'left', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&larr;', size = 60, padding = 20)},
{congruency: 'congruent', condition: 'standard', location: 'up', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&rarr;',      size = 60, padding = 20)},
{congruency: 'congruent', condition: 'standard', location: 'down', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&rarr;',  size = 60, padding = 20)},
{congruency: 'incongruent', condition: 'standard', location: 'up', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&rarr;',    size = 60, padding = 20)},
{congruency: 'incongruent', condition: 'standard', location: 'down', correct_response: 'right', stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&rarr;',size = 60, padding = 20)},

  ],
  randomize_order: true,
  repetitions: 4
};