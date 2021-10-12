//------------------------- Functions

var location_stim = function(up, down){
  
  // Present flanker stimuli either at the top or bottom of the screen
  var html = "<style>" +
              "divbottom{display:flex; justify-content: center; align-items: flex-end; height:600px}" +
              "divtop{display:flex; justify-content: center; align-items: flex-start; height:600px}" +
             ".topflanker{font-size:40px;}" +
             ".bottomflanker{font-size:40px;}" +
             "</style>";
             
  if(up != null) {
    html += "<divtop>";
    html += "<div class = 'topflanker'>";
    html += up;
    html += "</div></div>";
  }
  
  if(down != null) {
    html += "<divbottom>";
    html += "<div class = 'bottomflanker'>";
    html += down;
    html += "</div></div>";

  }
  
  return html;
};
  
  
  
//------------------------- Stimuli
  
  // Flanker stimuli
  var congruent_left = "&larr;&larr;&larr;&larr;&larr;"
  var congruent_right = "&rarr;&rarr;&rarr;&rarr;&rarr;"
  var incongruent_left = "&rarr;&rarr;&larr;&rarr;&rarr;"
  var incongruent_right = "&larr;&larr;&rarr;&larr;&larr;"


  var flanker_stimuli = [
    {stimulus: congruent_left},
    {stimulus: congruent_right},
    {stimulus: incongruent_left},
    {stimulus: incongruent_right},
  ]
  
  
  // Fixation cross
  var flanker_fixation = {
  type: 'html-keyboard-response',
  stimulus: '<div style="font-size:60px;">+</div>',
  choices: jsPsych.NO_KEYS,
  trial_duration: function(){
    return jsPsych.randomization.sampleWithoutReplacement([800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200], 1)[0]},
  data: {
    variable: 'fixation'
  }
}
  

