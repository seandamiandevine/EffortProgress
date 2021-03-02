// QUESTIONNAIRE VARIABLES FOR EFFORT PROGRESS TASK //

var demographics = {
  type: 'survey-text',
  questions: [
    {prompt: "For statistical purposes, how old are you? (please type out in years)"},
    {prompt: "For statistical purposes, What is your gender? (please type out your answer below)"},
    {prompt: "Were the instructions clear to you? Did you notice anything particular about the task you'd like to comment on? We would appreciate any feedback so we can improve the task in the future. (you can type as much as you want in the box below)"}
    ],
  preamble: 'Please answer these questions.',
  on_start: function(){
    document.body.style.background = "white"; // white bg for contrast
  }
};

var exit_questions = {
    type: 'survey-text',
    questions: [
      {prompt: "When choosing between the portals, did you develop a preference for choosing one over the other?", name:'preference'},
      {prompt: "Did you notice a difference between the tasks in each portals? If so, what was it?",name:"notice"},
      {prompt: "Did you find the task more difficult in one portal than the other?", name:'difficult'}
      //{prompt: "Some participants had a progress bar update each trial and between each block during task. If you were one of this participants, did the progress bar affect your performance? How so?", name:"progress"},
    ],
    preamble: 'Please answer these short questions about your approach to the experiment.'
  };

var nasatlx_choices = ['Low','','','','','','','','','High'];
var nasatlx_purple = {
    type: 'survey-likert',
    questions: [
      {prompt: "How much mental and perceptual activity was required when doing the task? Was the task easy or demanding, simple of complex, exacting or forgiving?",
      name: 'mental_purple',
      labels: nasatlx_choices, required:true},
      {prompt: "How much time pressure did you feel due to the rate or pace at which task elements occured? Was the pace slow and leisurely or rapid and frantic?",
      name: 'temporal_purple',
      labels: nasatlx_choices, required:true},
      {prompt: "How hard did you have to work mentally to accomplish your level of performance?",
      name: 'effort_purple',
      labels: nasatlx_choices, required:true},
      {prompt: "How successful do you think you were in accomplishing the goals of the task set by the experimenter? How satisfied were you with your performance in accomplishing these goals",
      name: 'performance_purple',
      labels: nasatlx_choices, required:true},
      {prompt: "How insecure, discouraged, irritated, stressed, and annoyed versus secure, gratified, content, relaxed, and complacent did you feel during the task.",
      name: 'frustration_purple',
      labels: nasatlx_choices, required:true},
    ],
    preamble: 'Please answer the following questions for how you felt the task IN THE PURPLE WORLD was.',
    scale_width: 400
  };

var nasatlx_blue = {
        type: 'survey-likert',
        questions: [
          {prompt: "How much mental and perceptual activity was required when doing the task? Was the task easy or demanding, simple of complex, exacting or forgiving?",
          name: 'mental_blue',
          labels: nasatlx_choices, required:true},
          {prompt: "How much time pressure did you feel due to the rate or pace at which task elements occured? Was the pace slow and leisurely or rapid and frantic?",
          name: 'temporal_blue',
          labels: nasatlx_choices, required:true},
          {prompt: "How hard did you have to work mentally to accomplish your level of performance?",
          name: 'effort_blue',
          labels: nasatlx_choices, required:true},
          {prompt: "How successful do you think you were in accomplishing the goals of the task set by the experimenter? How satisfied were you with your performance in accomplishing these goals",
          name: 'performance_blue',
          labels: nasatlx_choices, required:true},
          {prompt: "How insecure, discouraged, irritated, stressed, and annoyed versus secure, gratified, content, relaxed, and complacent did you feel during the task.",
          name: 'frustration_blue',
          labels: nasatlx_choices, required:true},
        ],
        preamble: 'Please answer the following questions for how you felt the game IN THE BLUE WORLD was.',
        scale_width: 400
      };

var nfc_options = ['Very Strong Disagreement','Strong Disagreement','Moderate Disagreement','Slight Disagreement',
                'Neither Agreement nor Disagreement','Slight Agreement','Moderate Agreement','Strong Agreement',
                'Very Strong Agreement'];
var nfc = {
        type: 'survey-multi-choice',
        questions: [
          {prompt: "I would prefer complex to simple problems.",
          name: 'nfc1',
          options: nfc_options, required:true},
          {prompt: "I like to have the responsibility of handling a situation that requires a lot of thinking.",
          name: 'nfc2',
          options: nfc_options, required:true},
          {prompt: "Thinking is not my idea of fun.",
          name: 'nfc3',
          options: nfc_options, required:true},
          {prompt: "I would rather do something that requires little thought than something that is sure to challenge my thinking abilities.",
          name: 'nfc4',
          options: nfc_options, required:true},
          {prompt: "I try to anticipate and avoid situations where there is likely a chance I will have to think in depth about something.",
          name: 'nfc5',
          options: nfc_options, required:true},
          {prompt: "I find satisfaction in deliberating hard and for long hours.",
          name: 'nfc6',
          options: nfc_options, required:true},
          {prompt: "I only think as hard as I have to.",
          name: 'nfc7',
          options: nfc_options, required:true},
          {prompt: "I prefer to think about small, daily projects to long-term ones.",
          name: 'nfc8',
          options: nfc_options, required:true},
          {prompt: "I like tasks that require little thought once I’ve learned them.",
          name: 'nfc9',
          options: nfc_options, required:true},
          {prompt: "The idea of relying on thought to make my way to the top appeals to me.",
          name: 'nfc10',
          options: nfc_options, required:true},
          {prompt: "I really enjoy a task that involves coming up with new solutions to problems.",
          name: 'nfc11',
          options: nfc_options, required:true},
          {prompt: "Learning new ways to think doesn’t excite me very much.",
          name: 'nfc12',
          options: nfc_options, required:true},
          {prompt: "I prefer my life to be filled with puzzles that I must solve.",
          name: 'nfc13',
          options: nfc_options, required:true},
          {prompt: "The notion of thinking abstractly is appealing to me.",
          name: 'nfc14',
          options: nfc_options, required:true},
          {prompt: "I would prefer a task that is intellectual, difficult, and important to one that is somewhat important but does not require much thought.",
          name: 'nfc15',
          options: nfc_options, required:true},
          {prompt: "I feel relief rather than satisfaction after completing a task that required a lot of mental effort.",
          name: 'nfc16',
          options: nfc_options, required:true},
          {prompt: "It’s enough for me that something gets the job done; I don’t care how or why it works.",
          name: 'nfc17',
          options: nfc_options, required:true},
          {prompt: "I usually end up deliberating about issues even when they do not affect me personally.",
          name: 'nfc18',
          options: nfc_options, required:true}
        ],
        preamble: 'For the following questions, please respond as honestly as possible and choose the option that best describes you.'
      };
