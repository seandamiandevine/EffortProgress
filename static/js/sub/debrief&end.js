// LOAD DEBRIEF EXTERNAL DEBRIEF FORM AT THE END OF THE TASK

//specify main task ending screen 
var end = {
  type: "html-keyboard-response",
  stimulus: "<p>You have finished the main task!</p>"+
            "<p>You are almost done the entire experiment. There are only a few short things left to do.</p>"+
            "<p>On the next screen, there will be a short questionnaire to fill out.</p>"+
            "<p>When you are ready, press SPACE to proceed to this questionnaire.</p>",
  choices: ['space']
  };

var debriefchoice = '';
var debrief = {
    type:'external-html',
    url: "/static/js/sub/debriefing.html",
    cont_btn: "submit",
    check_fn: function(elem){
      if (document.getElementById('affirmative').checked) {
        debriefchoice = 'agree';
        return true;
    } else if (document.getElementById('negative').checked) {
      debriefchoice = 'decline';
      return true
    } else {
        alert("Please choose one of the options");
        return false;
      }
      return false;
    },
   on_start: function(){document.body.style.background = "white"},
   on_finish: function(data){data.debrief = debriefchoice}
  };
