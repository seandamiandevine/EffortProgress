// DECK CHOICE FOR EFFORT PROGRESS TASK 2

var freechoice = {
  type: 'html-keyboard-response',
  stimulus: function() {

   deckl = deckList[p][0]['file']
   deckr = deckList[p][1]['file']

    h = '<img src="/static/images/'+ deckl +'"  width="480" height="270" style = "position:absolute; left:12.5%; top: 30%;" />' +
        '<img src="/static/images/'+ deckr +'"  width="480" height="270" style = "position:absolute; left:52.5%; top: 30%;" />'

   return h
  },
  choices: choicekeys,
  prompt: function() {

    h = '<div style = "position:relative; top: 200px;">Choose a deck! The "l" key chooses the deck on the left and the "r" key chooses the one on the right.</div>'

   return h
  },
  //trial_duration: choicetime,
  on_start: function() {

    document.querySelector('#jspsych-progressbar-container').style.display = 'none'; // don't show progress bar
    document.body.style.background = "grey"; // set background to grey

    isprogress = false; // assume is false unless chosen otherwise

  },
  on_finish: function(data) {

    choice = data.key_press;
    deckrt = data.rt;

    if(data.key_press == jsPsych.pluginAPI.convertKeyCharacterToKeyCode(choicekeys[0])) { // chose deck on the left

      background_pattern = deckList[p][0]['background']
      colList = maketrials(deckList[p][0]['switch'], numtrials[bl], numcolors[0], numcolors[1])

      if(deckList[p][0]['progress']==true) {

        isprogress = true;

      };


    } else if(data.key_press == jsPsych.pluginAPI.convertKeyCharacterToKeyCode(choicekeys[1])) { // chose deck on the right

      background_pattern = deckList[p][1]['background']
      colList = maketrials(deckList[p][1]['switch'], numtrials[bl], numcolors[0], numcolors[1])

      if(deckList[p][1]['progress']==true) {

        isprogress = true;

    }

  };

  jsPsych.setProgressBar(0); // set progress bar to zero

  // compute and store data
  if(deckList[p][0]['progress'] == true && deckList[p][1]['progress'] == true) {

    decision_type = 'P-P'

  } else if(deckList[p][0]['progress'] == false && deckList[p][1]['progress'] == false) {

    decision_type = 'NP-NP'

  } else {

    decision_type = 'P-NP'

  }

  p ++;  // iterate pair number

  if(p == (pairs.length*pairsperrun)-1) {

    r ++

  };

 }

};

var choice_confirmation = {

  type: 'html-keyboard-response',
  stimulus: function() {

    if(jsPsych.pluginAPI.convertKeyCodeToKeyCharacter(choice) == choicekeys[0]) {

      h = '<img src="/static/images/'+ deckl +'"  width="480" height="270" style = "position:absolute; left:12.5%; top: 30%;" />' +
          '<img src="/static/images/'+ deckr +'"  width="480" height="270" style = "position:absolute; left:52.5%; top: 30%;" />'  +
           '<img src="/static/images/selectsquare.png" width="480" height="270" style = "position:absolute; left:12.5%; top: 30%;" />'

    } else {

      h = '<img src="/static/images/'+ deckl +'"  width="480" height="270" style = "position:absolute; left:12.5%; top: 30%;" />' +
          '<img src="/static/images/'+ deckr +'"  width="480" height="270" style = "position:absolute; left:52.5%; top: 30%;" />' +
          '<img src="/static/images/selectsquare.png" width="480" height="270" style = "position:absolute; left:52.5%; top: 30%;" />'

    }

   return h

  },
  prompt: function() {

    h = '<div style = "position:relative; top: 200px;">Choose a deck! The "l" key chooses the deck on the left and the "r" key chooses the one on the right.</div>'

   return h
  },
  choices: jsPsych.NO_KEYS,
  trial_duration: confirmtime,
  post_trial_gap: iti,

}
