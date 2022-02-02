//------------------------- Functions

var set_arrows = function(angles, loc, flankers, target){
  
  var html = "<style>" +
              
             "divbottom{display:flex; justify-content: center; align-items: flex-end; height:600px}" +
             "divtop{display:flex; justify-content: center; align-items: flex-start; height:600px}" +
           //  ".topflanker{font-size:40px;}" +
           //  ".bottomflanker{font-size:40px;}" +
             
              "</style>";
  
  if(loc == 'up') {
    html += "<divtop>";
    html += "<div style = 'display: grid; width: 100%; font-size:40px; grid: 150px / auto auto auto auto auto;'>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[0] + "deg);'><span>" + flankers + "</span></div>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[1] + "deg);'><span>" + flankers + "</span></div>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[2] + "deg);'><span>" + target + "</span></div>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[3] + "deg);'><span>" + flankers + "</span></div>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[4] + "deg);'><span>" + flankers + "</span></div>";
    html += "</div>";
  } else {
    html += "<divbottom>";
    html += "<div style = 'display: grid; width: 100%; font-size:40px; grid: 150px / auto auto auto auto auto;'>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[0] + "deg);'><span>" + flankers + "</span></div>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[1] + "deg);'><span>" + flankers + "</span></div>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[2] + "deg);'><span>" + target + "</span></div>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[3] + "deg);'><span>" + flankers + "</span></div>";
    html += "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(" + angles[4] + "deg);'><span>" + flankers + "</span></div>";
    html += "</div>"
  }   
  
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
  

