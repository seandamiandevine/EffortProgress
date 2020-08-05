// FREE-CHOICE PRACTICE FOR EFFORT-PROGRESS TASK

var freechoice = {
  type: 'html-keyboard-response',
  stimulus: function() {
    blueside=_.sample(['right', 'left'])
    if(blueside=='right'){purpside='left'}else{purpside='right'}
    h = '<img src="/static/images/blue.png" align='+blueside+' width="400" height="400" hspace="50"/><img src="/static/images/purple.png" align='+purpside+' width="400" height="400" hspace="50"/>'

    // add stars in progress condition
    if(condition==1) {
      pos = 0;
      for(i in _.range(numblocks-blockCount)){
        h = h+"<span style='color:black; font-size:20pt; position:absolute; bottom:0; right:"+pos+"pt;' class='fa fa-star'></span>" // unchecked stars
        pos = pos+20 // increment position with star size
      }
      for(i in _.range(blockCount)) {
        h = h+"<span style='color:orange; font-size: 20pt; position:absolute; bottom:0; right:"+pos+"pt;' class='fa fa-star'></span>" // checked stars
        pos = pos+20 // increment position with star size
      }
    }

   return h
  },
  choices: choicekeys,
  prompt: function(){
    h = '<p>Choose a portal! The "l" key chooses the portal on the left and the "r" key chooses the one on the right.</p>'+
        '<p><b>REMEMBER:</b> The blue and purple portal may have switched sides since last time!</p>';
   return h
  },
  post_trial_gap: iti,
  data: {'hard_choice': hard},
  on_start: function() {
    document.body.style.background = "grey"; // set bg back to grey for choice
    document.querySelector('#jspsych-progressbar-container').style.display = 'none'; // hide progress bar for choice
  },
  on_finish: function(data) {
    if(data.key_press == jsPsych.pluginAPI.convertKeyCharacterToKeyCode(choicekeys[0])) { // chose portal on the left
      if (blueside=='left'){ // chose blue portal
        document.body.style.background = "#0000ff"; // make background blue
        if(easy == 'blue') { // blue portal is low demand
          colList = maketrials(easy_rate, numtrials, numcolors[0], numcolors[1]); // next trials are low switch rate
      } else if (hard == 'blue'){
        colList = maketrials(hard_rate, numtrials, numcolors[0], numcolors[1]);// if blue portal is high demand, next trials are high switch rate
      }

    }  else if (purpside == 'left'){ // chose purple portal
        document.body.style.background = "#98009b"; // make background purple
        if(easy == 'purple') { // purple portal is low demand
          colList = maketrials(easy_rate, numtrials, numcolors[0], numcolors[1]); // next trials are low switch rate
      } else if (hard == 'purple'){
        colList = maketrials(hard_rate, numtrials, numcolors[0], numcolors[1]); // if purple portal is high demand, next trials are high switch rate
       }
    }

} else if(data.key_press == jsPsych.pluginAPI.convertKeyCharacterToKeyCode(choicekeys[1])){ // chose portal on the right
  if (blueside=='right'){ // chose blue
    document.body.style.background = "#0000ff";
    if(easy == 'blue') {
      colList = maketrials(easy_rate, numtrials, numcolors[0], numcolors[1]);
  } else if (hard == 'blue') {
    colList = maketrials(hard_rate, numtrials, numcolors[0], numcolors[1]);
   }
 }  else if (purpside=='right'){ // chose purple 
      document.body.style.background = "#98009b";
      if(easy == 'purple'){
        colList = maketrials(easy_rate, numtrials, numcolors[0], numcolors[1]);
    } else if(hard == 'purple'){
    colList = maketrials(hard_rate, numtrials, numcolors[0], numcolors[1]);
    }
      }
    }
    if(condition == 1) {
      document.querySelector('#jspsych-progressbar-container').style.display = 'inline-block'; // show progress bar for trials
    }
  }
};
