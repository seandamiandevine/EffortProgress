// FREE-CHOICE PRACTICE FOR EFFORT-PROGRESS TASK

// variables
var blueside = ''; // the side the blue portal shows up each trial
var purpleside = ''; // the side the purple portal shows up each trial
var bluechoice = 0; // number of times a blue portal has been chosen in practice
var purplechoice = 0; // number of times a purple portal has been chosen in practice
// elements
var pracfreechoice = {
  type: 'html-keyboard-response',
  stimulus: function() {
    blueside=_.sample(['right', 'left'])
    if(blueside=='right'){purpside='left'}else{purpside='right'}
    h = '<img src="/static/images/blue.png" align='+blueside+' width="400" height="400" hspace="50"/><img src="/static/images/purple.png" align='+purpside+' width="400" height="400" hspace="50"/>'

   return h
  },
  choices: choicekeys,
  prompt: function(){
    if(bluechoice < numpracblocks/2 && purplechoice < numpracblocks/2){
      h=_.sample(['<p>Choose the <b>blue</b> portal ("l" key chooses left; "r" key chooses right).</p>', '<p>Choose the <b>purple</b> portal ("l" key chooses left; "r" key chooses right).</p>'])
  } else if(bluechoice < numpracblocks/2) {
    h = '<p>Choose the <b>blue</b> portal ("l" key chooses left; "r" key chooses right).</p>'
  } else if (purplechoice < numpracblocks/2){
    h = '<p>Choose the <b>purple</b> portal ("l" key chooses left; "r" key chooses right).</p>'
  }
    return h
  },
  post_trial_gap: iti,
  data: {'hard_choice': hard},
  on_start: function() {
    document.body.style.background = "grey";  // set bg back to grey for choice
    document.querySelector('#jspsych-progressbar-container').style.display = 'none'; // hide progress bar for choice
  },
  on_finish: function(data) {
    if(data.key_press == jsPsych.pluginAPI.convertKeyCharacterToKeyCode(choicekeys[0])) {
      if (blueside=='left'){
        bluechoice ++;
        document.body.style.background = "#0000ff";
        if(easy == 'blue') {
          colList = maketrials(easy_rate, numDSTpractrials, numcolors[0], numcolors[1]);
      } else {
        colList = maketrials(hard_rate, numDSTpractrials, numcolors[0], numcolors[1]);
      }
    }  else {
        purplechoice ++;
        document.body.style.background = "#98009b";
        if(easy == 'purple') {
          colList = maketrials(easy_rate, numDSTpractrials, numcolors[0], numcolors[1]);
      } else {
        colList = maketrials(hard_rate, numDSTpractrials, numcolors[0], numcolors[1]);
      }
    }
} else if(data.key_press == jsPsych.pluginAPI.convertKeyCharacterToKeyCode(choicekeys[1])){
  if (blueside=='right') {
    bluechoice ++;
    document.body.style.background = "#0000ff";
    if(easy == 'blue') {
      colList = maketrials(easy_rate, numDSTpractrials, numcolors[0], numcolors[1]);
  } else if (hard == 'blue') {
    colList = maketrials(hard_rate, numDSTpractrials, numcolors[0], numcolors[1]);
  }
} else {
      purplechoice ++;
      document.body.style.background = "#98009b";
      if(easy == 'purple'){
        colList = maketrials(easy_rate, numDSTpractrials, numcolors[0], numcolors[1]);
    } else if(hard == 'purple'){
      colList = maketrials(hard_rate, numDSTpractrials, numcolors[0], numcolors[1]);
    }
    }
  }
  }
};

var freechoice_practice = {
  timeline: [pracfreechoice, ptrials],
  loop_function: function() {
    if(bluechoice >= numpracblocks/2 && purplechoice >= numpracblocks/2) {
      return false
    } else {
      return true
    }
  }
};
