// WITHIN-BLOCK TRIAL ELEMENTS FOR EFFORT PROG TASK

subtrial = 0; // keep track of trials
var probe = '' // store probe this trial

var switchtrial = {
  type: "html-keyboard-response",
  stimulus: function() {

    probe = _.sample(nums2choose) // random number probe

    h = '<div style="color:'+colList[subtrial]+'; font-size: '+fontsize+'px; background-color:grey">'+probe+'</div>'
    return h

  },
  choices: trialkeys,
  trial_duration: resptime, // timeout
  post_trial_gap: iti,
  on_start: function() {

    if(isprogress == true) {
      document.querySelector('#jspsych-progressbar-container').style.display = 'inline-block'; // show progress bar
    } else {
      if(nonprogbar == true ){
        document.querySelector('#jspsych-progressbar-container').style.display = 'inline-block'; // show progress bar
      } else {
        document.querySelector('#jspsych-progressbar-container').style.display = 'none'; // show progress bar
      }
    }

    document.body.style.backgroundImage = "url('/static/images/"+background_pattern+"')"
    document.body.style.backgroundRepeat = "space"

  },
  on_finish: function(data) {
    if(isprogress == false && nonprogbar == true) {
      trialArray = Array.from(Array(colList.length).keys());
      ranVal = _.sample(trialArray);
      jsPsych.setProgressBar(ranVal/colList.length);       // just noise
    } else {
      jsPsych.setProgressBar((subtrial+1)/colList.length); // shows up here for the momentary full progress bar view
    }

    // compute and store data
    if(colList[subtrial] == numcolors[0]) {

      task = 'magnitude';

    } else {

      task = 'parity';

    }

    data.run = r;
    data.decisionnum = p;
    data.subtrial = subtrial+1;
    data.deckl = deckl.substring(0, 5);
    data.deckr = deckr.substring(0, 5);
    data.decision_type = decision_type;
    data.deckchoice = choice;
    data.deckrt = deckrt;
    data.probe = probe;
    data.task = task;
    data.switchresp = data.key_press;
    data.switchrt = data.rt;

  }
};

var switchtrials = {
  timeline: [switchtrial],
  loop_function: function() {

    if(subtrial == colList.length-1) {

      bl ++ ; // iterate block length list index
      subtrial = 0; // reset subtrials for next block
      return false

    } else {

      subtrial ++;
      return true

    }

  }
};
