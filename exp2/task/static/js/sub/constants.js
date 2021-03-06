// CONSTANTS FOR EFFORT PROGRESS TASK

const subject_id = uniqueId; //unique id number generated by psiTurk

jsPsych.data.addProperties({

  subject: subject_id

});

/* set constants */
const d1 = {switch: 0.1, progress: false, file: 'deck1.png', background: 'symbol1.png'};
const d2 = {switch: 0.5, progress: false, file: 'deck2.png', background: 'symbol2.png'};
const d3 = {switch: 0.9, progress: false, file: 'deck3.png', background: 'symbol3.png'};
const d4 = {switch: 0.1, progress: true, file: 'deck4.png', background: 'symbol4.png'};
const d5 = {switch: 0.5, progress: true, file: 'deck5.png', background: 'symbol5.png'};
const d6 = {switch: 0.9, progress: true, file: 'deck6.png', background: 'symbol6.png'};

const pairs = [
  [d1, d2], [d1, d3], [d1, d4], [d1, d5], [d1, d6],
  [d2, d3], [d2, d4], [d2, d5], [d2, d6],
  [d3, d4], [d3, d5], [d3, d6],
  [d4, d5], [d4, d6],
  [d5, d6]
];

const numpractrials = 20;              // number of practice trials for task-switching (7 magnitude, 7 parity; 13 magnitude/parity)
const numruns = 2;                     // number of runs
const pairsperrun = 2;                 // number of time pairs are presented per run
const iti = 500;                       // time after response until next cue (in ms)
const confirmtime = 500;               // time that deck choice confirmation appears for (in ms.)
const lengthfixed = true;              // whether or not block lengths vary
const nonprogbar = true;               // whether a "progress" bar shows up in the no progress decks
//const choicetime = 3000              // time to choose deck before timeout (in ms.)
//const learntime = 2000               // time for which a deck is displayed during the learning phase (in ms)
const learnrep = 3;                    // number of times the same deck is shown during the learning phase
const resptime = 2500;                 // time to answer switch task before timeout (in ms)
const fontsize = 100;                  // font size for cue and probe text (in px)
const choicekeys = ['l','r'];          // keys that subjects can use to make deck choices
const trialkeys = ['e', 'i'];          // keys that subjects can use to make responses with during trials
var nums2choose = [1,2,3,4,6,7,8,9];   // numbers to judge
var numcolors = ['green', 'orange'];   // colors for rules
var background_pattern = '';           // background patten to reflect deck choice
var colList = [];                      // stores block-to-block color list
var isprogress = false;                // boolean for whether or not to show progress bar

// function to make switch trials list
maketrials = function(switch_rate, n, col1, col2) {
  nt = n-1;
  nswitch = Math.round(switch_rate*nt);
  nrep = nt-nswitch;
  x = Array(nswitch).fill('switch');
  x = x.concat(Array(nrep).fill('repeat'));
  x = _.sample(x, nt); // shuffle everything up
  x = ['NA'].concat(x); // first trial is neither switch nor repeat
  colors = [];
  for (i in _.range(x.length)){
    if(x[i]=='NA'){
      colors= colors.concat( _.sample([col1, col2]));// first trial
    } else if(x[i]=='repeat') {
      colors= colors.concat(colors[i-1]);
    } else if(x[i] == 'switch') {
      if(colors[i-1]==col1){
        y = col2;
      } else {y = col1}
      colors = colors.concat(y);
    }
  }
  return(colors);
};

// randomized deck order for one subject
var deckList = [];
for(i in _.range(numruns)) {
    var onerun = _.shuffle(pairs);
    var counterrun = []; // same decklist, but pairings on opposite sides
    if(pairsperrun % 2 == 0) {
     for(p in onerun) {
     reversepair = [onerun[p][1], onerun[p][0]];
     counterrun.push(reversepair);
     }
 }
 counterrun = _.shuffle(counterrun);
 deckList.push(onerun, counterrun);
};

deckList = _.shuffle(deckList.flat());

var bl = 0; // block length counter
if(lengthfixed) {
  var numtrials  = Array(deckList.length + pairs.length * learnrep).fill(16); // fixed block lengths for switch task
} else {
  var numtrials  = _.sample(blockLengths);          // variable block lengths for switch task
};

var fs = {
	type: 'fullscreen',        // setup fullscreen mode
  fullscreen_mode: true,
  on_start: function(){
    document.body.style.background = "grey"; // set background to grey
    document.querySelector('#jspsych-progressbar-container').style.display = 'none'; // hide progress bar right away
   }
};
