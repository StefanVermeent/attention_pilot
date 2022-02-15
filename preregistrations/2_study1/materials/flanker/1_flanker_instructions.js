//-------------------- Welcome
var flanker_welcome = {
  type: 'instructions',
  pages: [
    "Welcome to the <b>Flanker</b> game!"
  ],
  show_clickable_nav: true,
  allow_backward: true,
  key_forward: -1,
  key_backward: -1,
  button_label_next: "continue",
  data: {variable: 'welcome', task: "flanker_practice"}
};

//-------------------- Instructions
var flanker_instructions = {
  type: 'instructions',
  pages: [
  //Page 1
  "<p style = 'text-align: center;'>"+ 
  "In this game, you will see five arrows like the ones below.<br><br><br>" +
  "<div style = 'font-size: 30px'>&larr;&larr;&larr;&larr;&larr;</div><br><br><br>" +
  "Your job is to decide which way the <strong>middle arrow</strong> is pointing.<br><br><br>",
  //Page 2
  "<p style = 'text-align: center;'>"+ 
  "Sometimes, all the arrows point the <strong>same</strong> way.<br><br><br>" +
  "<div style = 'font-size: 30px'>&larr;&larr;&larr;&larr;&larr;</div><br><br><br>",
  //Page 3
  "<p style = 'text-align: center;'>"+ 
  "Other times, the arrows point a <strong>different</strong> way.<br><br><br>" +
  "<div style = 'font-size: 30px'>&larr;&larr;&rarr;&larr;&larr;</div><br><br><br>",  
  //Page 4
  "<p style = 'text-align: center;'>"+ 
  "You should <i>always</i> look at the <strong>middle arrow</strong> and ignore the others.<br><br>" +
  //Page 5 
  "<div style = 'float: left;'>If it points LEFT<br>press the LEFT (&larr;) key.</div>" +
  "<div style = 'float: right;'>If it points RIGHT<br>press the RIGHT (&rarr;) key.</div><br><br><br><br>" + 
  //Page 6
  "In the example below, the middle arrow points right,<br>" +
  "so you would press the right key.<br><br>" +
  "<div style = 'font-size: 30px'>&larr;&larr;&rarr;&larr;&larr;</div><br><br><br>", 
  //Page 7
   "<style>" +
   ".grid-container {" +
      "display: grid;" +
      "grid-template-columns: auto;" +
      "grid-template-rows: auto auto auto auto auto auto;" +
      "padding: 0px;" +
   "}" +
   ".grid-item {" +
      "padding: 0px;" +
      "justify-content: center;" +
      "text-align: center;" +
      "}" +
     "</style>" +
     
     "<p style = 'text-align: center;'>" +
     "You will play three versions of this game.<br>" + 
     "In each version, the arrows look a bit different:</p><br><br>" +
     "<div class='grid-container'>" +

     "<div class='grid-item'><strong>VERSION 1</strong></div>" +
     "<div class='grid-item'>" +
     "<div style = 'display: inline-grid; width: 200px; height: 80px; font-size: 40px; grid: 70px / auto auto auto auto auto;'>" +
     "<div style = 'margin: auto; padding: 0px 0px;'>" +
     "<div style = 'margin: auto; padding: 0px 0px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 0px 0px;'>" +
     "<div style = 'margin: auto; padding: 0px 0px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 0px 0px;'>" +
     "<div style = 'margin: auto; padding: 0px 0px; transform: rotate(0deg);'><span>&rarr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 0px 0px;'>" +
     "<div style = 'margin: auto; padding: 0px 0px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 0px 0px;'>" +
     "<div style = 'margin: auto; padding: 0px 0px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "</div></div> " +
   
     "<div class='grid-item'><br><br><br><strong>VERSION 2</strong></div>  " +
     "<div class='grid-item'>" +
     "<div style = 'display: inline-grid; width: 500px; font-size: 60px; grid: 70px / auto auto auto auto auto;'>" +
     "<div style = 'margin: auto; padding: 5px 0px;'>" +
     "<div style = 'margin: auto; padding: 5px 20px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 20px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 20px; transform: rotate(0deg);'><span>&rarr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 20px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 20px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "</div></div>" +
     "</div>" +
 
     "<div class='grid-item'><br><br><br><strong>VERSION 3</strong></div>" +
     "<div class='grid-item'>" +
  	 "<div style = 'display: inline-grid; content-align: center; width: 200px; font-size: 40px; grid: 70px / auto auto auto auto auto;'>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(45deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(45deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(45deg);'><span>&rarr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(45deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(45deg);'><span>&larr;</span></div>" +
     "</div>" +
     "</div></div> " +
     "</div>  " +
     "</div>",
     //Page 8
   "<style>" +
   ".grid-container {" +
      "display: grid;" +
      "grid-template-columns: auto;" +
      "grid-template-rows: auto auto auto auto auto auto;" +
      "padding: 0px;" +
   "}" +
   ".grid-item {" +
      "padding: 0px;" +
      "justify-content: center;" +
      "text-align: center;" +
      "}" +
     "</style>" +
     
     "<p style = 'text-align: center;'>" +
     "Even though the versions look different, your job is the same each time:<br>" + 
     "Decide which way the <strong>middle arrow</strong> is pointing." +
     "</p><br><br>" +
     "<div class='grid-container'>" +

     "<div class='grid-item'><strong>VERSION 1</strong></div>" +
     "<div class='grid-item'>" +
     "<div style = 'display: inline-grid; width: 200px; height: 80px; font-size: 40px; grid: 70px / auto auto auto auto auto;'>" +
     "<div style = 'margin: auto; padding: 0px 0px;'>" +
     "<div style = 'margin: auto; padding: 0px 0px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 0px 0px;'>" +
     "<div style = 'margin: auto; padding: 0px 0px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 0px 0px;'>" +
     "<div style = 'margin: auto; padding: 0px 0px; transform: rotate(0deg);'><span>&rarr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 0px 0px;'>" +
     "<div style = 'margin: auto; padding: 0px 0px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 0px 0px;'>" +
     "<div style = 'margin: auto; padding: 0px 0px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "</div></div> " +
   
     "<div class='grid-item'><br><br><br><strong>VERSION 2</strong></div>  " +
     "<div class='grid-item'>" +
     "<div style = 'display: inline-grid; width: 500px; font-size: 60px; grid: 70px / auto auto auto auto auto;'>" +
     "<div style = 'margin: auto; padding: 5px 0px;'>" +
     "<div style = 'margin: auto; padding: 5px 20px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 20px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 20px; transform: rotate(0deg);'><span>&rarr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 20px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 20px; transform: rotate(0deg);'><span>&larr;</span></div>" +
     "</div>" +
     "</div></div>" +
     "</div>" +
 
     "<div class='grid-item'><br><br><br><strong>VERSION 3</strong></div>" +
     "<div class='grid-item'>" +
  	 "<div style = 'display: inline-grid; content-align: center; width: 200px; font-size: 40px; grid: 70px / auto auto auto auto auto;'>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(45deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(45deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(45deg);'><span>&rarr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(45deg);'><span>&larr;</span></div>" +
     "</div>" +
     "<div style = 'margin: auto; padding: 5px 0;'>" +
     "<div style = 'margin: auto; padding: 5px 0px; transform: rotate(45deg);'><span>&larr;</span></div>" +
     "</div>" +
     "</div></div> " +
     "</div>  " +
     "</div>",
     
     
     "<p style = 'text-align: center;'>" + 
      "Try to respond as fast and as correctly as possible.<br><br>" +
      "Click 'continue' to practice this game<br><br><br>",
  ],
  show_clickable_nav: true,
  allow_backward: true,
  key_forward: -1,
  key_backward: -1,
  button_label_next: "continue",
  button_label_previous: "go back",
  data: {variable: "instructions", task: "flanker_practice"}
};

//-------------------- Practice

var flanker_practice = {
    type: "html-keyboard-response",
    stimulus: jsPsych.timelineVariable('practice_stim'),
    choices: ['ArrowLeft', 'ArrowRight'],
    data: {
      variable: 'practice',
      task: 'flanker_practice',
      location: jsPsych.timelineVariable('location'),
      condition: jsPsych.timelineVariable('condition'),
      congruency: jsPsych.timelineVariable('congruency'),
      stimulus: jsPsych.timelineVariable('condition'),
    },
    on_finish: function(data) {
      if(jsPsych.pluginAPI.compareKeys(data.response, jsPsych.timelineVariable('correct_response', true))) {
        data.correct = true;
      } else {
        data.correct = false;
      }
    }
};

// Practice Procedures for each version
var flanker_practice_procedure_standard = {
  timeline: [flanker_fixation, flanker_practice, feedback],
  timeline_variables: [
{congruency: 'congruent', condition: 'standard', location: 'up', correct_response: 'ArrowLeft',     practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'congruent', condition: 'standard', location: 'down', correct_response: 'ArrowLeft',   practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'standard', location: 'up', correct_response: 'ArrowLeft',   practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'standard', location: 'down', correct_response: 'ArrowLeft', practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'congruent', condition: 'standard', location: 'up',  correct_response: 'ArrowRight',    practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&rarr;', size = 40, padding = 0)},
{congruency: 'congruent', condition: 'standard', location: 'down', correct_response: 'ArrowRight',  practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&rarr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'standard', location: 'up', correct_response: 'ArrowRight',  practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&rarr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'standard', location: 'down', correct_response: 'ArrowRight',practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&rarr;', size = 40, padding = 0)},
  ],
  randomize_order: true,
  repetitions: 1,
};

var flanker_practice_procedure_enhanced = {
  timeline: [flanker_fixation, flanker_practice, feedback],
  timeline_variables: [
{congruency: 'congruent',   condition: 'enhanced', location: 'up', correct_response: 'ArrowLeft',    practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&larr;', size = 60, padding = 20)},
{congruency: 'congruent',   condition: 'enhanced', location: 'down', correct_response: 'ArrowLeft',  practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&larr;', size = 60, padding = 20)},
{congruency: 'incongruent', condition: 'enhanced', location: 'up', correct_response: 'ArrowLeft',    practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&larr;', size = 60, padding = 20)},
{congruency: 'incongruent', condition: 'enhanced', location: 'down', correct_response: 'ArrowLeft',  practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&larr;', size = 60, padding = 20)},
{congruency: 'congruent',   condition: 'enhanced', location: 'up', correct_response: 'ArrowRight',   practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&rarr;', target = '&rarr;', size = 60, padding = 20)},
{congruency: 'congruent',   condition: 'enhanced', location: 'down', correct_response: 'ArrowRight', practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&rarr;', target = '&rarr;', size = 60, padding = 20)},
{congruency: 'incongruent', condition: 'enhanced', location: 'up', correct_response: 'ArrowRight',   practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'up', flankers = '&larr;', target = '&rarr;', size = 60, padding = 20)},
{congruency: 'incongruent', condition: 'enhanced', location: 'down', correct_response: 'ArrowRight', practice_stim: set_arrows(angles = [0, 0, 0, 0, 0], loc = 'down', flankers = '&larr;', target = '&rarr;', size = 60, padding = 20)},
  ],
  randomize_order: true,
  repetitions: 1,
};

var flanker_practice_procedure_degraded = {
  timeline: [flanker_fixation, flanker_practice, feedback],
  timeline_variables: [
{congruency: 'congruent',   condition: 'degraded', location: 'up', correct_response: 'ArrowLeft',    practice_stim: set_arrows(angles = [45,45,45,45,45], loc = 'up', flankers = '&larr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'congruent',   condition: 'degraded', location: 'down', correct_response: 'ArrowLeft',  practice_stim: set_arrows(angles = [45,45,45,45,45], loc = 'down', flankers = '&larr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'degraded', location: 'up', correct_response: 'ArrowLeft',    practice_stim: set_arrows(angles = [45,45,45,45,45], loc = 'up', flankers = '&rarr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'degraded', location: 'down', correct_response: 'ArrowLeft',  practice_stim: set_arrows(angles = [45,45,45,45,45], loc = 'down', flankers = '&rarr;', target = '&larr;', size = 40, padding = 0)},
{congruency: 'congruent',   condition: 'degraded', location: 'up', correct_response: 'ArrowRight',   practice_stim: set_arrows(angles = [45,45,45,45,45], loc = 'up', flankers = '&rarr;', target = '&rarr;', size = 40, padding = 0)},
{congruency: 'congruent',   condition: 'degraded', location: 'down', correct_response: 'ArrowRight', practice_stim: set_arrows(angles = [45,45,45,45,45], loc = 'down', flankers = '&rarr;', target = '&rarr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'degraded', location: 'up', correct_response: 'ArrowRight',   practice_stim: set_arrows(angles = [45,45,45,45,45], loc = 'up', flankers = '&larr;', target = '&rarr;', size = 40, padding = 0)},
{congruency: 'incongruent', condition: 'degraded', location: 'down', correct_response: 'ArrowRight', practice_stim: set_arrows(angles = [45,45,45,45,45], loc = 'down', flankers = '&larr;', target = '&rarr;', size = 40, padding = 0)},
  ],
  randomize_order: true,
  repetitions: 1,
};



// Start different versions of the Flanker Task
// VERSION 1
var flanker_practice_start_standard = {
  type: "html-keyboard-response",
  stimulus: "<p style = 'text-align: center;'>" +
  "You will now play the version shown below:<br><br>" +
  "<div style = 'display: inline-grid; content-align: center; width: 200px; grid: 70px / auto;'>" +
  set_arrows(angles = [0,0,0,0,0], loc = "center", flankers = "&larr;", target = "&rarr;", size = 40, padding = 0) +
  "</div>" +
  "<br><br><br><br><br>" +
  "We will start with <strong>8 practice rounds</strong>." +
  "<br><br>" +
  "Place your fingers on the left (&larr;) and right (&rarr;) arrow keys.<br><br>" +
  "When you are ready to practice, press any key to start.",
  data: {variable: "test_practice_start_standard", task: "flanker_start_standard"}
};

var flanker_start_standard = {
  type: "html-keyboard-response",
  stimulus: "<p style = 'text-align: center;'>" +
  "Now it's time for the real game.<br><br>" +
  "<div style = 'display: inline-grid; content-align: center; width: 200px; grid: 70px / auto;'>" +
  set_arrows(angles = [0,0,0,0,0], loc = "center", flankers = "&larr;", target = "&rarr;", size = 40, padding = 0) +
  "</div>" +
  "<br><br><br><br><br>" +
  "From now on you will not receive feedback after each response.<br><br>" +
  "Place your fingers on the left (&larr;) and right (&rarr;) arrow keys.<br><br>" +
  "When you are ready, press any key to start.",
  data: {variable: "test_start_standard", task: "flanker_start_standard"}
};

// VERSION 2
var flanker_practice_start_enhanced = {
  type: "html-keyboard-response",
  stimulus: "<p style = 'text-align: center;'>" +
  "You will now play the version shown below:<br><br>" +
  "<div style = 'display: inline-grid; content-align: center; width: 500px; grid: 70px / auto;'>" +
  set_arrows(angles = [0,0,0,0,0], loc = "center", flankers = "&larr;", target = "&rarr;", size = 60, padding = 20) +
  "</div>" + 
  "<br><br><br><br><br>" +
  "We will start with <strong>8 practice rounds</strong>." +
  "<br><br>" +
  "Place your fingers on the left (&larr;) and right (&rarr;) arrow keys.<br><br>" +
  "When you are ready to practice, press any key to start.",
  data: {variable: "test_practice_start_enhanced", task: "flanker_start_degraded"}
};

var flanker_start_enhanced = {
  type: "html-keyboard-response",
  stimulus: "<p style = 'text-align: center;'>" +
  "Now it's time for the real game.<br><br>" +
  "<div style = 'display: inline-grid; content-align: center; width: 500px; grid: 70px / auto;'>" +
  set_arrows(angles = [0,0,0,0,0], loc = "center", flankers = "&larr;", target = "&rarr;", size = 60, padding = 20) +
  "</div>" +
  "<br><br><br><br><br>" +
  "From now on you will not receive feedback after each response.<br><br>" +
  "Place your fingers on the left (&larr;) and right (&rarr;) arrow keys.<br><br>" +
  "When you are ready, press any key to start.",
  data: {variable: "test_start_enhanced", task: "flanker_start_enhanced"}
};

// VERSION 3
var flanker_practice_start_degraded = {
  type: "html-keyboard-response",
  stimulus: "<p style = 'text-align: center;'>" +
   "You will now play the version shown below:<br><br>" +
  "<div style = 'display: inline-grid; content-align: center; width: 200px; grid: 70px / auto;'>" +
  set_arrows(angles = [45,45,45,45,45], loc = "center", flankers = "&larr;", target = "&rarr;", size = 40, padding = 0) +
  "</div>" + 
  "<br><br><br><br><br>" +
  "We will start with <strong>8 practice rounds</strong>." +
  "<br><br>" +
  "Place your fingers on the left (&larr;) and right (&rarr;) arrow keys.<br><br>" +
  "When you are ready to practice, press any key to start.",
  data: {variable: "test_practice_start_degraded", task: "flanker_start_degraded"}
};

var flanker_start_degraded = {
  type: "html-keyboard-response",
  stimulus: "<p style = 'text-align: center;'>" +
  "Now it's time for the real game.<br><br>" +
  "<div style = 'display: inline-grid; content-align: center; width: 200px; grid: 70px / auto;'>" +
  set_arrows(angles = [45,45,45,45,45], loc = "center", flankers = "&larr;", target = "&rarr;", size = 40, padding = 0) +
  "</div>" + 
  "<br><br><br><br><br>" +
  "From now on you will not receive feedback after each response.<br><br>" +
  "Place your fingers on the left (&larr;) and right (&rarr;) arrow keys.<br><br>" +
  "When you are ready, press any key to start.",
  data: {variable: "test_start_degraded", task: "flanker_start_degraded"}
};



var flanker_break = {
  type: "html-button-response",
  stimulus: "<p style = 'text-align: center;'>" +
  "Well done!<br><br>" +
  "You can now take a break before starting the next round.<br><br>" +
  "Take all the time you need!<br><br>" +
  "Press 'continue' when you are ready.<br><br>",
  choices: ['Continue'],
  data: {variable: "test_break", task: "flanker_break"}
};


// Finish Practice trials
var flanker_practice_finish = {
  type: "html-button-response",
  stimulus: "<p style = 'text-align: center;'>" +
  "Great job!<br><br>" +
  "You will now play the actual game.<br><br>" +
  "This round will take about two minutes.<br>" +
  "From now on you will not receive feedback after each response.<br><br>",
  choices: ['Continue'],
  data: {variable: "practice_finish", task: "flanker_practice"}
};



var flanker_end = {
  type: "html-button-response",
  stimulus: 
  "Great job!<br><br>" +
  "You are now finished playing the <strong>Flanker</strong> game.<br><br>" +
  "Click 'finish' to continue.<br><br>",
  choices: ['Finish'],
  data: {variable: "end", task: "flanker_practice"}
};