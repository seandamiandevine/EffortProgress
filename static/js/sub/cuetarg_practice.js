// CUE-TARGET PRACTICE TRIALS FOR EFFORT PROGRESS TASK

// practice constants
pcolors = Array(numpractrials/2).fill(numcolors[0]).concat(Array(numpractrials/2).fill(numcolors[1])); // single practice
pcolors=pcolors.concat(_.shuffle(pcolors)); // mixed practice

pprobes =  []; // actual probes (numbers)
for(i in _.range(pcolors.length)){
  pprobes.push(_.sample(nums2choose));
};

pCount = 0;
acc = ''; // accuracy during practice

// elements
var pracprobe = {
  type: "html-keyboard-response",
  stimulus: function() {
    return '<p style="color: '+pcolors[pCount]+'; font-size: '+fontsize+'px;"><b>'+pprobes[pCount]+'</b></p>'
  },
  choices: trialkeys,
  post_trial_gap: iti,
  on_finish: function(data){
      if(pcolors[pCount]==numcolors[0] && pprobes[pCount]<5 && data.key_press==jsPsych.pluginAPI.convertKeyCharacterToKeyCode(trialkeys[0])) {
        acc=1
      } else if (pcolors[pCount]==numcolors[0] && pprobes[pCount]>5 && data.key_press==jsPsych.pluginAPI.convertKeyCharacterToKeyCode(trialkeys[1])){
        acc=1
      } else if (pcolors[pCount]==numcolors[1] && pprobes[pCount]%2==0 && data.key_press==jsPsych.pluginAPI.convertKeyCharacterToKeyCode(trialkeys[0])){
        acc=1
      } else if (pcolors[pCount]==numcolors[1] && pprobes[pCount]%2!=0 && data.key_press==jsPsych.pluginAPI.convertKeyCharacterToKeyCode(trialkeys[1])){
        acc=1
      } else {
        acc = 0
      }

      pCount++
    }
  };

var feedback = {
  type: "html-keyboard-response",
  stimulus: function(){
    if(acc==1){return '<p style="color: black; font-size: '+fontsize+'px;">Correct!</p>'
  } else {return '<p style= "color: red; font-size: '+fontsize+'px;">Incorrect!</p>'}
  },
  choices: jsPsych.NO_KEYS,
  trial_duration: 750,
  post_trial_gap: iti
};

var singlepractice = {
  timeline: [pracprobe, feedback],
  repetitions: numpractrials/2
};

var combinedpractice = {
  timeline: [pracprobe, feedback],
  repetitions: numpractrials
};
