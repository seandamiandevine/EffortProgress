const learning_order = Array(learnrep).fill([d1, d2, d3, d4, d5, d6]).flat();
const learning_duration = learning_order.length; // total duration of learning phase

// data to be saved -- matches testphase, but NA for anything that doesn't apply
var r = 'learning' // run number
var p = 0 // pair
var lCount = 0; // trial counter for learning phase
var deckl = ''; // deck being learnt
var deckr = 'NA'; // deck on the right
var decision_type = 'NA'; // type of decision (progress-progress, no progress-progress, no progress-no progress)
var choice = 'NA'; // chosen deck (key code)
var deckrt = 'NA'; // time it took to choose deck (in ms.)


var displaydeck = {
  type: 'html-keyboard-response',
  stimulus: function() {

    deckl = learning_order[lCount]['file']
    h = '<img src="/static/images/'+ deckl +'" width="480" height="270" />'

   return h

  },
  choices: ['space'],
  //trial_duration: learntime,
  post_trial_gap: iti,
  prompt: function() {

    if(lCount <= learning_duration/learnrep) {

      return "<p>Now we'll show you what the game would look like if you chose <b>this</b> deck! Press SPACE to start</p>"

    } else {

      return "<p>For another time now, this is what the game will look like if you chose <b>this</b> deck! Press SPACE to start</p>"

    }

  },
  on_start: function() {

    document.body.style.background = "grey";  // set bg back to grey for choice
    document.querySelector('#jspsych-progressbar-container').style.display = 'none'; // don't show progres bar

  },
  on_finish: function() {

    background_pattern = learning_order[lCount]['background']
    colList = maketrials(learning_order[lCount]['switch'], numtrials[bl], numcolors[0], numcolors[1])


    if(learning_order[lCount]['progress']==true) {

      isprogress = true;

    } else {

      isprogress = false;

    }

    jsPsych.setProgressBar(0)
    lCount ++

  }

};

var learningphase = {
  timeline: [displaydeck, switchtrials],
  repetitions: learning_duration
};
