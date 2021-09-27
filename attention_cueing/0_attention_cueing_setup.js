
//------------------------- Variables to hide cursor during trials
var cursor_off = {
    type: 'call-function',
    func: function() {
        document.body.style.cursor= "none";
    }
}

var cursor_on = {
    type: 'call-function',
    func: function() {
        document.body.style.cursor= "auto";
    }
}

//------------------------- Symmetry Images
var fixation_grid    = "<img src='img/empty_grid.png' width='40%'>";
var cue_neutral      = "<img src='img/cue_neutral.png' width='40%'>";
var cue_topleft      = "<img src='img/cue_topleft.png' width='40%'>";
var cue_middleleft   = "<img src='img/cue_middleleft.png' width='40%'>";
var cue_bottomleft   = "<img src='img/cue_bottomleft.png' width='40%'>";
var cue_bottommiddle = "<img src='img/cue_bottommiddle.png' width='40%'>";
var cue_bottomright  = "<img src='img/cue_bottomright.png' width='40%'>";
var cue_middleright  = "<img src='img/cue_middleright.png' width='40%'>";
var cue_topright     = "<img src='img/cue_topright.png' width='40%'>";
var cue_topmiddle    = "<img src='img/cue_topmiddle.png' width='40%'>";
  
var leftarrow_topleft      = "<img src='img/leftarrow_topleft.png' width='40%'>";
var leftarrow_middleleft   = "<img src='img/leftarrow_middleleft.png' width='40%'>";
var leftarrow_bottomleft   = "<img src='img/leftarrow_bottomleft.png' width='40%'>";
var leftarrow_bottommiddle = "<img src='img/leftarrow_bottommiddle.png' width='40%'>";
var leftarrow_bottomright  = "<img src='img/leftarrow_bottomright.png' width='40%'>";
var leftarrow_middleright  = "<img src='img/leftarrow_middleright.png' width='40%'>";
var leftarrow_topright     = "<img src='img/leftarrow_topright.png' width='40%'>";
var leftarrow_topmiddle    = "<img src='img/leftarrow_topmiddle.png' width='40%'>";
  
var rightarrow_topleft      = "<img src='img/rightarrow_topleft.png' width='40%'>";
var rightarrow_middleleft   = "<img src='img/rightarrow_middleleft.png' width='40%'>";
var rightarrow_bottomleft   = "<img src='img/rightarrow_bottomleft.png' width='40%'>";
var rightarrow_bottommiddle = "<img src='img/rightarrow_bottommiddle.png' width='40%'>";
var rightarrow_bottomright  = "<img src='img/rightarrow_bottomright.png' width='40%'>";
var rightarrow_middleright  = "<img src='img/rightarrow_middleright.png' width='40%'>";
var rightarrow_topright     = "<img src='img/rightarrow_topright.png' width='40%'>";
var rightarrow_topmiddle    = "<img src='img/rightarrow_topmiddle.png' width='40%'>";
  

var fixation = {
  type: 'html-keyboard-response',
  stimulus: fixation_grid,
  choices: jsPsych.NO_KEYS,
  trial_duration: 1000,
  data: {variable: 'fixation'}
}
  
var cue_presentation = {
  type: 'html-keyboard-response',
  stimulus: jsPsych.timelineVariable('cue'),
  choices: jsPsych.NO_KEYS,
  trial_duration: 250,
  data: {variable: 'cue'}
}
  
var target_presentation = {
  type: 'html-keyboard-response',
  stimulus: jsPsych.timelineVariable('target'),
  choices: ['ArrowLeft', 'ArrowRight'],
  trial_duration: 3000,
  data: {variable: 'target'}
}


//------------------------- Initiate timeline
var timeline = []

  timeline.push({
  type: 'fullscreen',
  fullscreen_mode: true
});
  
