
// These are variables that are shared across all attention tasks.


// At start of experiment: Make sure the size of stimulus presentation is the same for all participants.
var resize_screen = {
  type: 'resize',
  item_width: 3 + 3/8,
  item_height: 2 + 1/8,
  prompt: "<p>Before we start, it is important that you follow the steps below carefully.<br>" +
  "If you skip these steps, the experiment might not work correctly on your computer.<br><br>" +
  "1. Pick up a creditcard (or another card of similar size) and hold it against your computer screen.<br>" +
  "2. Next, click on the lower right corner of the blue box. You can now make it bigger or smaller.<br>" +
  "3. Change the size of the box until it is <strong> just as big as your creditcard</strong>.<br><br><br>" +
  "Click 'continue' when you are done" +
  "</p>",
  button_label: ['Continue'],
  pixels_per_unit: 100,
  starting_size: 300,
  data: {
    task: 'resize',
}};

var fullscreenmode = {
  type: 'fullscreen',
  fullscreen_mode: true
};

// Remove cursor during cognitive tasks
var cursor_off = {
    type: 'call-function',
    func: function() {
        document.body.style.cursor= "none";
    }
};


// Make cursor visible again in between cognitive tasks or when a button response is required.
var cursor_on = {
    type: 'call-function',
    func: function() {
        document.body.style.cursor= "auto";
    }
};


// Task feedback during the practice trials.
var feedback = {
  type: 'html-keyboard-response',
  stimulus: function(){
    
    var last_trial_rt = jsPsych.data.getLastTrialData().values()[0].rt;
    var last_trial_correct = jsPsych.data.get().last(1).values()[0].correct;
    
    if(last_trial_rt > 2000){
      return "<p style = 'color:red;font-size:40px'>Too slow!</p>"; 
    } else {
      if(last_trial_correct){
        return "<p style = 'color:green;font-size:40px'>Correct!</p>"; 
      } else {
        return "<p style = 'color:red;font-size:40px'>Incorrect!</p>"; 
      }}},
  trial_duration: 2000,
  choices: jsPsych.NO_KEYS,
  data: {
    variable: 'feedback'
  }
};


// End of Cognitive task part of the experiment.
var cognitive_tasks_end = {
  type: "html-button-response",
  stimulus: 
  "Great job!<br><br>" +
  "You have finished the first part of the experiment.<br><br>" +
  "Click the button below to start the second part of the experiment.<br><br>",
  choices: ['Continue to part 2'],
  data: {variable: "end", task: "end_global"}
}