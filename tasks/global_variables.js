var resize_screen = {
  type: 'resize',
  item_width: 3 + 3/8,
  item_height: 2 + 1/8,
  prompt: "<p>For this game to work properly, it is important that we know the resolution of your screen.<br><br>" +
  "Please pick up a creditcard (or another card of similar size) and hold it up to the screen.<br>" +
  "Then, click and drag the lower right corner of the box until the box is the same size as the credit card when held up to the screen." +
  "</p>",
  button_label: ['Continue'],
  pixels_per_unit: 100
};

var fullscreenmode = {
  type: 'fullscreen',
  fullscreen_mode: true
};

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

var feedback = {
  type: 'html-keyboard-response',
  stimulus: function(){
    var last_trial_correct = jsPsych.data.get().last(1).values()[0].correct;
    if(last_trial_correct){
      return "<p style = 'color:green;font-size:40px'>Correct!</p>"; // the parameter value has to be returned from the function
    } else {
      return "<p style = 'color:red;font-size:40px'>Incorrect!</p>"; // the parameter value has to be returned from the function
    }
  },
  trial_duration: 2000,
  choices: jsPsych.NO_KEYS,
  data: {
    variable: 'feedback'
  }
}

var cognitive_tasks_end = {
  type: "html-button-response",
  stimulus: 
  "Great job!<br><br>" +
  "You have finished the first part of the experiment.<br><br>" +
  "Click 'Continue' to start the second part of the experiment.<br><br>",
  choices: ['Continue'],
  data: {variable: "end", task: "end_global"}
};
}