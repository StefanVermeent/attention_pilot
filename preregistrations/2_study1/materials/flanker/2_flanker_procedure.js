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
    angle: jsPsych.timelineVariable('angle')
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
    
{congruency: 'congruent',   angle: 'standard', location: 'up',   correct_response: 'left_up',        stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'standard', location: 'down', correct_response: 'left_up',      stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'standard', location: 'up',   correct_response: 'left_up',        stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'standard', location: 'down', correct_response: 'left_up',      stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'standard', location: 'up',   correct_response: 'left_down',      stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'standard', location: 'down', correct_response: 'left_down',    stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'standard', location: 'up',   correct_response: 'left_down',      stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'standard', location: 'down', correct_response: 'left_down',    stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'standard', location: 'up',   correct_response: 'right_up',       stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'congruent',   angle: 'standard', location: 'down', correct_response: 'right_up',     stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'standard', location: 'up',   correct_response: 'right_up',       stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'standard', location: 'down', correct_response: 'right_up',     stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&rarr;')},
{congruency: 'congruent',   angle: 'standard', location: 'up',   correct_response: 'right_down',     stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'congruent',   angle: 'standard', location: 'down', correct_response: 'right_down',   stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'standard', location: 'up',   correct_response: 'right_down',     stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'standard', location: 'down', correct_response: 'right_down',   stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&rarr;')},

{congruency: 'congruent',   angle: 'opposite_45deg', location: 'up', correct_response: 'left_up',        stim: set_arrows(angles = [45, 45, 45, 45, 45], loc = 'up', flankers = '&larr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'opposite_45deg', location: 'down', correct_response: 'left_up',      stim: set_arrows(angles = [45, 45, 45, 45, 45], loc = 'down', flankers = '&larr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'opposite_45deg', location: 'up', correct_response: 'left_up',      stim: set_arrows(angles = [45, 45, 45, 45, 45], loc = 'up', flankers = '&rarr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'opposite_45deg', location: 'down', correct_response: 'left_up',    stim: set_arrows(angles = [45, 45, 45, 45, 45], loc = 'down', flankers = '&rarr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'opposite_45deg', location: 'up', correct_response: 'left_down',      stim: set_arrows(angles = [315, 315, 315, 315, 315], loc = 'up', flankers = '&larr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'opposite_45deg', location: 'down', correct_response: 'left_down',    stim: set_arrows(angles = [315, 315, 315, 315, 315], loc = 'down', flankers = '&larr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'opposite_45deg', location: 'up', correct_response: 'left_down',    stim: set_arrows(angles = [315, 315, 315, 315, 315], loc = 'up', flankers = '&rarr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'opposite_45deg', location: 'down', correct_response: 'left_down',  stim: set_arrows(angles = [315, 315, 315, 315, 315], loc = 'down', flankers = '&rarr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'opposite_45deg', location: 'up', correct_response: 'right_up',       stim: set_arrows(angles = [315, 315, 315, 315, 315], loc = 'up', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'congruent',   angle: 'opposite_45deg', location: 'down', correct_response: 'right_up',     stim: set_arrows(angles = [315, 315, 315, 315, 315], loc = 'down', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'opposite_45deg', location: 'up', correct_response: 'right_up',     stim: set_arrows(angles = [315, 315, 315, 315, 315], loc = 'up', flankers = '&larr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'opposite_45deg', location: 'down', correct_response: 'right_up',   stim: set_arrows(angles = [315, 315, 315, 315, 315], loc = 'down', flankers = '&larr;', target = '&rarr;')},
{congruency: 'congruent',   angle: 'opposite_45deg', location: 'up', correct_response: 'right_down',     stim: set_arrows(angles = [45, 45, 45, 45, 45], loc = 'up', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'congruent',   angle: 'opposite_45deg', location: 'down', correct_response: 'right_down',   stim: set_arrows(angles = [45, 45, 45, 45, 45], loc = 'down', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'opposite_45deg', location: 'up', correct_response: 'right_down',   stim: set_arrows(angles = [45, 45, 45, 45, 45], loc = 'up', flankers = '&larr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'opposite_45deg', location: 'down', correct_response: 'right_down', stim: set_arrows(angles = [45, 45, 45, 45, 45], loc = 'down', flankers = '&larr;', target = '&rarr;')},

{congruency: 'congruent',   angle: 'opposite_60deg', location: 'up', correct_response: 'left_up',        stim: set_arrows(angles = [60, 60, 60, 60, 60], loc = 'up', flankers = '&larr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'opposite_60deg', location: 'down', correct_response: 'left_up',      stim: set_arrows(angles = [60, 60, 60, 60, 60], loc = 'down', flankers = '&larr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'opposite_60deg', location: 'up', correct_response: 'left_up',      stim: set_arrows(angles = [60, 60, 60, 60, 60], loc = 'up', flankers = '&rarr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'opposite_60deg', location: 'down', correct_response: 'left_up',    stim: set_arrows(angles = [60, 60, 60, 60, 60], loc = 'down', flankers = '&rarr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'opposite_60deg', location: 'up', correct_response: 'left_down',      stim: set_arrows(angles = [300, 300, 300, 300, 300], loc = 'up', flankers = '&larr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'opposite_60deg', location: 'down', correct_response: 'left_down',    stim: set_arrows(angles = [300, 300, 300, 300, 300], loc = 'down', flankers = '&larr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'opposite_60deg', location: 'up', correct_response: 'left_down',    stim: set_arrows(angles = [300, 300, 300, 300, 300], loc = 'up', flankers = '&rarr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'opposite_60deg', location: 'down', correct_response: 'left_down',  stim: set_arrows(angles = [300, 300, 300, 300, 300], loc = 'down', flankers = '&rarr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'opposite_60deg', location: 'up', correct_response: 'right_up',       stim: set_arrows(angles = [300, 300, 300, 300, 300], loc = 'up', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'congruent',   angle: 'opposite_60deg', location: 'down', correct_response: 'right_up',     stim: set_arrows(angles = [300, 300, 300, 300, 300], loc = 'down', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'opposite_60deg', location: 'up', correct_response: 'right_up',     stim: set_arrows(angles = [300, 300, 300, 300, 300], loc = 'up', flankers = '&larr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'opposite_60deg', location: 'down', correct_response: 'right_up',   stim: set_arrows(angles = [300, 300, 300, 300, 300], loc = 'down', flankers = '&larr;', target = '&rarr;')},
{congruency: 'congruent',   angle: 'opposite_60deg', location: 'up', correct_response: 'right_down',     stim: set_arrows(angles = [60, 60, 60, 60, 60], loc = 'up', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'congruent',   angle: 'opposite_60deg', location: 'down', correct_response: 'right_down',   stim: set_arrows(angles = [60, 60, 60, 60, 60], loc = 'down', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'opposite_60deg', location: 'up', correct_response: 'right_down',   stim: set_arrows(angles = [60, 60, 60, 60, 60], loc = 'up', flankers = '&larr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'opposite_60deg', location: 'down', correct_response: 'right_down', stim: set_arrows(angles = [60, 60, 60, 60, 60], loc = 'down', flankers = '&larr;', target = '&rarr;')},

{congruency: 'congruent',   angle: 'opposite_70deg', location: 'up', correct_response: 'left_up',        stim: set_arrows(angles = [70, 70, 70, 70, 70], loc = 'up', flankers = '&larr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'opposite_70deg', location: 'down', correct_response: 'left_up',      stim: set_arrows(angles = [70, 70, 70, 70, 70], loc = 'down', flankers = '&larr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'opposite_70deg', location: 'up', correct_response: 'left_up',      stim: set_arrows(angles = [70, 70, 70, 70, 70], loc = 'up', flankers = '&rarr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'opposite_70deg', location: 'down', correct_response: 'left_up',    stim: set_arrows(angles = [70, 70, 70, 70, 70], loc = 'down', flankers = '&rarr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'opposite_70deg', location: 'up', correct_response: 'left_down',      stim: set_arrows(angles = [290, 290, 290, 290, 290], loc = 'up', flankers = '&larr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'opposite_70deg', location: 'down', correct_response: 'left_down',    stim: set_arrows(angles = [290, 290, 290, 290, 290], loc = 'down', flankers = '&larr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'opposite_70deg', location: 'up', correct_response: 'left_down',    stim: set_arrows(angles = [290, 290, 290, 290, 290], loc = 'up', flankers = '&rarr;', target = '&larr;')},
{congruency: 'incongruent', angle: 'opposite_70deg', location: 'down', correct_response: 'left_down',  stim: set_arrows(angles = [290, 290, 290, 290, 290], loc = 'down', flankers = '&rarr;', target = '&larr;')},
{congruency: 'congruent',   angle: 'opposite_70deg', location: 'up', correct_response: 'right_up',       stim: set_arrows(angles = [290, 290, 290, 290, 290], loc = 'up', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'congruent',   angle: 'opposite_70deg', location: 'down', correct_response: 'right_up',     stim: set_arrows(angles = [290, 290, 290, 290, 290], loc = 'down', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'opposite_70deg', location: 'up', correct_response: 'right_up',     stim: set_arrows(angles = [290, 290, 290, 290, 290], loc = 'up', flankers = '&larr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'opposite_70deg', location: 'down', correct_response: 'right_up',   stim: set_arrows(angles = [290, 290, 290, 290, 290], loc = 'down', flankers = '&larr;', target = '&rarr;')},
{congruency: 'congruent',   angle: 'opposite_70deg', location: 'up', correct_response: 'right_down',     stim: set_arrows(angles = [70, 70, 70, 70, 70], loc = 'up', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'congruent',   angle: 'opposite_70deg', location: 'down', correct_response: 'right_down',   stim: set_arrows(angles = [70, 70, 70, 70, 70], loc = 'down', flankers = '&rarr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'opposite_70deg', location: 'up', correct_response: 'right_down',   stim: set_arrows(angles = [70, 70, 70, 70, 70], loc = 'up', flankers = '&larr;', target = '&rarr;')},
{congruency: 'incongruent', angle: 'opposite_70deg', location: 'down', correct_response: 'right_down', stim: set_arrows(angles = [70, 70, 70, 70, 70], loc = 'down', flankers = '&larr;', target = '&rarr;')},
  
  ],
  randomize_order: true,
  repetitions: 4
};