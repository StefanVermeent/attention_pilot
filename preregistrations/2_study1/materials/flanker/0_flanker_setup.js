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
  var congruent_left1 = "&nwarr;&nwarr;&nwarr;&swarr;&swarr;";
  var congruent_left2 = "&swarr;&swarr;&nwarr;&nwarr;&nwarr;";
  var congruent_left3 = "&nwarr;&nwarr;&swarr;&swarr;&swarr;";
  var congruent_left4 = "&swarr;&swarr;&swarr;&nwarr;&nwarr;";
  
  var congruent_left5 = "&nwarr;&swarr;&nwarr;&nwarr;&swarr;";
  var congruent_left6 = "&swarr;&nwarr;&nwarr;&swarr;&nwarr;";
  var congruent_left7 = "&nwarr;&swarr;&swarr;&nwarr;&swarr;";
  var congruent_left8 = "&swarr;&nwarr;&swarr;&swarr;&nwarr;";
  
  
  var congruent_right1 = "&nearr;&nearr;&nearr;&searr;&searr;";
  var congruent_right2 = "&searr;&searr;&nearr;&nearr;&nearr;";
  var congruent_right3 = "&nearr;&nearr;&searr;&searr;&searr;";
  var congruent_right4 = "&searr;&searr;&searr;&nearr;&nearr;";
  
  var congruent_right5 = "&nearr;&searr;&nearr;&nearr;&searr;";
  var congruent_right6 = "&searr;&nearr;&nearr;&searr;&nearr;";
  var congruent_right7 = "&nearr;&searr;&searr;&nearr;&searr;";
  var congruent_right8 = "&searr;&nearr;&searr;&searr;&nearr;";

 
  var incongruent_left1 = "&nwarr;&nwarr;&nearr;&swarr;&swarr;";
  var incongruent_left2 = "&swarr;&swarr;&nearr;&nwarr;&nwarr;";
  var incongruent_left3 = "&nwarr;&nwarr;&searr;&swarr;&swarr;";
  var incongruent_left4 = "&swarr;&swarr;&searr;&nwarr;&nwarr;";
  
  var incongruent_left5 = "&nwarr;&swarr;&nearr;&nwarr;&swarr;";
  var incongruent_left6 = "&swarr;&nwarr;&nearr;&swarr;&nwarr;";
  var incongruent_left7 = "&nwarr;&swarr;&searr;&nwarr;&swarr;";
  var incongruent_left8 = "&swarr;&nwarr;&searr;&swarr;&nwarr;";
  
  
  var incongruent_right1 = "&nearr;&nearr;&nwarr;&searr;&searr;";
  var incongruent_right2 = "&searr;&searr;&nwarr;&nearr;&nearr;";
  var incongruent_right3 = "&nearr;&nearr;&swarr;&searr;&searr;";
  var incongruent_right4 = "&searr;&searr;&swarr;&nearr;&nearr;";

  var incongruent_right5 = "&nearr;&searr;&nwarr;&nearr;&searr;";
  var incongruent_right6 = "&searr;&nearr;&nwarr;&searr;&nearr;";
  var incongruent_right7 = "&nearr;&searr;&swarr;&nearr;&searr;";
  var incongruent_right8 = "&searr;&nearr;&swarr;&searr;&nearr;";


  var flanker_stimuli = [
    {stimulus: congruent_left2},
    {stimulus: congruent_left3},
    {stimulus: congruent_left4},
    {stimulus: congruent_left5},
    {stimulus: congruent_left6},
    {stimulus: congruent_left7},
    {stimulus: congruent_left8},
    {stimulus: congruent_right1},
    {stimulus: congruent_right2},
    {stimulus: congruent_right3},
    {stimulus: congruent_right4},
    {stimulus: congruent_right5},
    {stimulus: congruent_right6},
    {stimulus: congruent_right7},
    {stimulus: congruent_right8},
    {stimulus: incongruent_left1},
    {stimulus: incongruent_left2},
    {stimulus: incongruent_left3},
    {stimulus: incongruent_left4},
    {stimulus: incongruent_left5},
    {stimulus: incongruent_left6},
    {stimulus: incongruent_left7},
    {stimulus: incongruent_left8},
    {stimulus: incongruent_right1},
    {stimulus: incongruent_right2},
    {stimulus: incongruent_right3},
    {stimulus: incongruent_right4},
    {stimulus: incongruent_right5},
    {stimulus: incongruent_right6},
    {stimulus: incongruent_right7},
    {stimulus: incongruent_right8},
  ]
  
  
  // Fixation cross
  var flanker_fixation = {
  type: 'html-keyboard-response',
  stimulus: '<div style="font-size:60px;">+</div>',
  choices: jsPsych.NO_KEYS,
  trial_duration: 1000,
  data: {
    variable: 'fixation'
  }
}
  

