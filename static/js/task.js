
var psiturk = new PsiTurk(uniqueId, adServerLoc, mode);

var timeline = [fs,i1, singlepractice, i2, singlepractice, i3, combinedpractice, i4, freechoice_practice, i5];

// add main trials
for (b in _.range(numblocks)){
  timeline.push(freechoice);
  timeline.push(trials);
  timeline.push(blockprog)
};

// add end screen and questionnaire
timeline.push(end);
timeline.push(demographics);
timeline.push(exit_questions);
timeline.push(nasatlx_purple);
timeline.push(nasatlx_blue);
timeline.push(nfc);

// add debrief form
timeline.push(debrief);

// run and preload images
jsPsych.init({
    timeline: timeline,
    preload_images: ['/static/images/blue.png', '/static/images/purple.png'],
    show_preload_progress_bar: true,
    show_progress_bar: true,
    auto_update_progress_bar: false,
    on_data_update: function(data) {
      psiturk.recordTrialData(data)
    },
    on_finish: function() {
      psiturk.saveData();
      setTimeout(function(){psiturk.completeHIT();}, 2000); // add short delay so data saves right
    }
});
