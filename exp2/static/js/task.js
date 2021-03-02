
var psiturk = new PsiTurk(uniqueId, adServerLoc, mode);

var timeline = [fs, i1, singlepractice, i2, singlepractice, i3, combinedpractice, i4, learningphase, i5];
for(t in _.range(numruns*pairsperrun*pairs.length)) {
  timeline.push(freechoice);
  timeline.push(choice_confirmation);
  timeline.push(switchtrials);
};
timeline.push(finish2q, demographics, exit_questions, tlx, uncertainty, nfc, debrief, end);

// run and preload images
jsPsych.init({
    timeline: timeline,
    preload_images: ['/static/images/deck1.png',
    '/static/images/deck2.png',
    '/static/images/deck3.png',
    '/static/images/deck4.png',
    '/static/images/deck5.png',
    '/static/images/deck6.png',
    '/static/images/symbol1.png',
    '/static/images/symbol2.png',
    '/static/images/symbol3.png',
    '/static/images/symbol4.png',
    '/static/images/symbol5.png',
    '/static/images/symbol6.png',
    '/static/images/progressinst.png',
    '/static/images/selectsquare.png'
    ],
    show_preload_progress_bar: true,
    show_progress_bar: true,
    auto_update_progress_bar: false,
    on_data_update: function(data) {
      psiturk.recordTrialData(data)
    },
    on_finish: function() {
      psiturk.saveData({

            success: function(){

            	psiturk.completeHIT();

            },

            error: prompt_resubmit});
      //setTimeout(function(){psiturk.completeHIT();}, 2000); // add short delay so data saves right
    }
});

prompt_resubmit = function() {
  document.body.innerHTML = error_message;
  $("#resubmit").click(resubmit);
};

resubmit = function() {
  document.body.innerHTML = "<h1>Trying to resubmit...</h1>";
  reprompt = setTimeout(prompt_resubmit, 10000);

  psiturk.saveData({
    success: function() {
        clearInterval(reprompt);
        psiturk.completeHIT();
    },
    error: prompt_resubmit
  });
};
