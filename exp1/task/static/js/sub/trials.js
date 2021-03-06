// WITHIN-BLOCK TRIAL ELEMENTS FOR EFFORT PROG TASK

//variables
trialCount = 0; // keep track of trials
blockCount = 0; // keep track of blocks

//elements
var probe = {
  type: "html-keyboard-response",
  stimulus: function(){
    stim = _.sample(nums2choose) // random number probe
    h = '<p style="color:'+colList[trialCount]+'; font-size: '+fontsize+'px;">'+stim+'</p>'
    return h
  },
  choices: trialkeys,
  trial_duration: resptime, // timeout
  post_trial_gap: iti,
  on_finish: function() {
    jsPsych.setProgressBar((trialCount+1)/colList.length); // shows up here for the momentary full progress bar view
    if(trialCount != colList.length-1) {
      trialCount ++;
    } else {
      trialCount = 0; // reset trialCount for next block
    }
  }
};

var blockprog = {
  type: "html-keyboard-response",
  stimulus: function() {
    blockCount ++; // update blockCount
    h = '';
    if(condition==1) {
      for(i in _.range(blockCount)) {
        h = h+"<span style='color:orange; font-size: 40pt' class='fa fa-star'></span>" // checked stars
      }
      for(i in _.range(numblocks-blockCount)){
        h = h+"<span style='color:black; font-size: 40pt' class='fa fa-star'></span>" // unchecked stars
      }
    }
    return h
  },
  choices:jsPsych.NO_KEYS,
  trial_duration: function(){if(condition==1){return progtime} else{return 0}}, // don't show if stars when they shouldn't be there
  post_trial_gap: function(){if(condition==1){return iti} else{return 0}},
  on_start: function() {
    document.querySelector('#jspsych-progressbar-container').style.display = 'none'; // hide in-trial progress bar
    document.body.style.background = "grey" ; // show progress against grey background
    jsPsych.setProgressBar(0); // reset progress bar for next block
  }
};

var ptrials = {
  timeline: [probe],
  repetitions: numDSTpractrials
};


var trials = {
  timeline: [probe],
  repetitions: numtrials
};
