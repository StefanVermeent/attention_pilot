//------------------------- Functions

var set_arrows = function(angles, up, down){
  
  var html = "<style>" +
              
             "divbottom{display:flex; justify-content: center; align-items: flex-end; height:600px}" +
             "divtop{display:flex; justify-content: center; align-items: flex-start; height:600px}" +
             ".topflanker{font-size:40px;}" +
             ".bottomflanker{font-size:40px;}" +
  
              ".outer_left {" +
                "transform: rotate(" + angles[0] + ");" +
                "-webkit-transform: rotate(" + angles[0] + ");" +
                "};" +
              ".inner_left {" +
                "transform: rotate(" + angles[1] + ");" +
                "-webkit-transform: rotate(" + angles[1] + ");" +
                "};" +
              ".target {" +
                "transform: rotate(" + angles[2] + ");" +
                "-webkit-transform: rotate(" + angles[2] + ");" +
                "};" +
              ".inner_right {" +
                "transform: rotate(" + angles[3] + ");" +
                "-webkit-transform: rotate(" + angles[3] + ");" +
                "};" +
              ".outer_right {" +
                "transform: rotate(" + angles[4] + ");" +
                "-webkit-transform: rotate(" + angles[4] + ");" +
                "};" +
              "</style>";

  
  if(up === true) {
    html += "<divtop>";
    html += "<div class = 'topflanker'>";
    html += "<p>";
    html += "<i class = 'outer_left'>&larr;</i>"; 
    html += "<i class = 'inner_left'>&larr;</i>";
    html += "<i class = 'target'>&larr;</i>";
    html += "<i class = 'inner_right'>&larr;</i>";
    html += "<i class = 'outer_right'>&larr;</i>";
    html += "</p></div></div>";
  }          
  
  if(down === true) {
    html += "<divbottom>";
    html += "<div class = 'bottomflanker'>";
    html += "<p>";
    html += "<i class = 'outer_left'>&larr;</i>"; 
    html += "<i class = 'inner_left'>&larr;</i>";
    html += "<i class = 'target'>&larr;</i>";
    html += "<i class = 'inner_right'>&larr;</i>";
    html += "<i class = 'outer_right'>&larr;</i>";
    html += "</p></div></div>";
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
  

