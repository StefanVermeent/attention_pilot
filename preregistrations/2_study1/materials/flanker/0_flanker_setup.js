//------------------------- Functions

var set_arrows = function(angles, loc, flankers, target, size, padding){
  
  var html = "<style>" +
             "divbottom{display:flex; justify-content: center; align-items: flex-end; height:600px}" +
             "divtop{display:flex; justify-content: center; align-items: flex-start; height:600px}" +
              "</style>";
  
  if(loc == 'up') {
    html += "<divtop>";
  } else {
    html += "<divbottom>";
  }
  
  
    // Global frame
    html += "<div style = 'display: grid; width: 100%; font-size: " + size + "px; grid: 150px / auto auto auto auto auto;'>";
    // Outer left flanker
    html += "<div style = 'margin: auto; padding: 5px " + padding + "px;'>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[0] + "deg);'><span>" + flankers + "</span></div>";
    html += "</div>";
    // Inner left flanker
    html += "<div style = 'margin: auto; padding: 5px " + padding + "px;'>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[1] + "deg);'><span>" + flankers + "</span></div>";
    html += "</div>";
    // Target
    html += "<div style = 'margin: auto; padding: 5px " + padding + "px;'>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[2] + "deg);'><span>" + target + "</span></div>";
    html += "</div>";
    // Inner right target
    html += "<div style = 'margin: auto; padding: 5px " + padding + "px;'>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[3] + "deg);'><span>" + flankers + "</span></div>";
    html += "</div>";
    // Outer right target
    html += "<div style = 'margin: auto; padding: 5px " + padding + "px;'>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[4] + "deg);'><span>" + flankers + "</span></div>";
    html += "</div>";
    html += "</div>";
 
   
   return html;
  
};   
  
  




  
  // Fixation cross
  var flanker_fixation = {
  type: 'html-keyboard-response',
  stimulus: '<div style="font-size:60px;">+</div>',
  choices: jsPsych.NO_KEYS,
  trial_duration: 1000,
  data: {
    variable: 'fixation'
  }
};
  

