// QUESTIONNAIRE VARIABLES FOR EFFORT PROGRESS TASK //

var demographics = {
  type: 'survey-text',
  questions: [
    {prompt: "For statistical purposes, how old are you? (please type out in years)", name:'age'},
    {prompt: "For statistical purposes, What is your gender? (please type out your answer below)", name:'gender'}
      ],
  preamble: 'Please answer these questions.',
  on_start: function() {
    document.body.style.background = "white"; // white bg for contrast
  },
  on_finish: function(data) {
    jsPsych.data.addProperties({
      age: JSON.parse(data.responses)['age'],
      gender: JSON.parse(data.responses)['gender']
    });
  }
};

var exit_questions = {
    type: 'survey-text',
    questions: [
      {prompt: "Do you feel like you understood the study well?", name:'understood'},
      {prompt: "Do you feel that you were able to remember the how each deck affected the color/number game?", name:'mapping'},
      {prompt: "When choosing between the decks, how did you decide which ones you preferred? Did you use any tricks or show any systematic preference for one kind of deck over the other?", name:'preference'},
      {prompt: "Did you notice a difference between the game after choosing either deck? If so, what was it?",name:"notice"},
      {prompt: "Did you find the task more difficult in some of the decks than others?", name:'difficult'},
    ],
    preamble: 'Please answer these short questions about your approach to the experiment.',
    on_finish: function(data) {
      jsPsych.data.addProperties({
        understood: JSON.parse(data.responses)['understood'],
        mapping: JSON.parse(data.responses)['mapping'],
        preference: JSON.parse(data.responses)['preference'],
        notice: JSON.parse(data.responses)['notice'],
        difficulty: JSON.parse(data.responses)['difficult']
      });
    }
  };

var nasatlx_choices = ['Low','','','','','','','','','High'];
var tlx = {
    type: 'survey-likert',
    questions: [
      {prompt: '<img src="static/images/deck1.png" width="160" height="90"/>', name: 'deck1_demand', labels: nasatlx_choices, required:true},
      {prompt: '<img src="static/images/deck2.png" width="160" height="90"/>', name: 'deck2_demand', labels: nasatlx_choices, required:true},
      {prompt: '<img src="static/images/deck3.png" width="160" height="90"/>', name: 'deck3_demand', labels: nasatlx_choices, required:true},
      {prompt: '<img src="static/images/deck4.png" width="160" height="90"/>', name: 'deck4_demand', labels: nasatlx_choices, required:true},
      {prompt: '<img src="static/images/deck5.png" width="160" height="90"/>', name: 'deck5_demand', labels: nasatlx_choices, required:true},
      {prompt: '<img src="static/images/deck6.png" width="160" height="90"/>', name: 'deck6_demand', labels: nasatlx_choices, required:true}

    ],
    preamble: 'For the number/color game in each of the following decks, how much mental and perceptual activity did you find was required (e.g., thinking, deciding, calculating, remembering, looking, searching, etc.)? Was the task easy or demanding, simple of complex, exacting or forgiving?',
    scale_width: 500,
    on_finish: function(data) {
      jsPsych.data.addProperties({
        tlx_deck1: JSON.parse(data.responses)['deck1_demand'],
        tlx_deck2: JSON.parse(data.responses)['deck2_demand'],
        tlx_deck3: JSON.parse(data.responses)['deck3_demand'],
        tlx_deck4: JSON.parse(data.responses)['deck4_demand'],
        tlx_deck5: JSON.parse(data.responses)['deck5_demand'],
        tlx_deck6: JSON.parse(data.responses)['deck6_demand']
      });
    }
 };

var nfc_options = ['Extremely uncharacteristic', 'Somewhat uncharacteristic', 'Uncertain', 'Somewhat characteristic', 'Extremely characteristic'];
var nfc = {
        type: 'survey-likert',
        questions: [
          {prompt: "I would prefer complex to simple problems.",
          name: 'nfc1',
          labels: nfc_options, required:true},
          {prompt: "I like to have the responsibility of handling a situation that requires a lot of thinking.",
          name: 'nfc2',
          labels: nfc_options, required:true},
          {prompt: "Thinking is not my idea of fun.",
          name: 'nfc3',
          labels: nfc_options, required:true},
          {prompt: "I would rather do something that requires little thought than something that is sure to challenge my thinking abilities.",
          name: 'nfc4',
          labels: nfc_options, required:true},
          {prompt: "I try to anticipate and avoid situations where there is likely a chance I will have to think in depth about something.",
          name: 'nfc5',
          labels: nfc_options, required:true},
          {prompt: "I find satisfaction in deliberating hard and for long hours.",
          name: 'nfc6',
          labels: nfc_options, required:true},
          {prompt: "I only think as hard as I have to.",
          name: 'nfc7',
          labels: nfc_options, required:true},
          {prompt: "I prefer to think about small, daily projects to long-term ones.",
          name: 'nfc8',
          labels: nfc_options, required:true},
          {prompt: "I like tasks that require little thought once I have learned them.",
          name: 'nfc9',
          labels: nfc_options, required:true},
          {prompt: "The idea of relying on thought to make my way to the top appeals to me.",
          name: 'nfc10',
          labels: nfc_options, required:true},
          {prompt: "I really enjoy a task that involves coming up with new solutions to problems.",
          name: 'nfc11',
          labels: nfc_options, required:true},
          {prompt: "Learning new ways to think does not excite me very much.",
          name: 'nfc12',
          labels: nfc_options, required:true},
          {prompt: "I prefer my life to be filled with puzzles that I must solve.",
          name: 'nfc13',
          labels: nfc_options, required:true},
          {prompt: "The notion of thinking abstractly is appealing to me.",
          name: 'nfc14',
          labels: nfc_options, required:true},
          {prompt: "I would prefer a task that is intellectual, difficult, and important to one that is somewhat important but does not require much thought.",
          name: 'nfc15',
          labels: nfc_options, required:true},
          {prompt: "I feel relief rather than satisfaction after completing a task that required a lot of mental effort.",
          name: 'nfc16',
          labels: nfc_options, required:true},
          {prompt: "It is enough for me that something gets the job done; I don’t care how or why it works.",
          name: 'nfc17',
          labels: nfc_options, required:true},
          {prompt: "I usually end up deliberating about issues even when they do not affect me personally.",
          name: 'nfc18',
          labels: nfc_options, required:true}
        ],
        preamble: 'For each of the statements below, please indicate to what extent the statement is characteristic of you. If the statement is extremely uncharacteristic of you (not at all like you) please choose that option beside the question; if the statement is extremely characteristic of you (very much like you) please indicate that option next to the question. Of course, a statement may be neither extremely uncharacteristic nor extremely characteristic of you; if so, please choose the option in the middle of the scale that describes the best fit.',
        scale_width: 900,
        on_finish: function(data) {
          jsPsych.data.addProperties({
            nfc1: JSON.parse(data.responses)['nfc1'],
            nfc2: JSON.parse(data.responses)['nfc2'],
            nfc3: JSON.parse(data.responses)['nfc3'],
            nfc4: JSON.parse(data.responses)['nfc4'],
            nfc5: JSON.parse(data.responses)['nfc5'],
            nfc6: JSON.parse(data.responses)['nfc6'],
            nfc7: JSON.parse(data.responses)['nfc7'],
            nfc8: JSON.parse(data.responses)['nfc8'],
            nfc9: JSON.parse(data.responses)['nfc9'],
            nfc10: JSON.parse(data.responses)['nfc10'],
            nfc11: JSON.parse(data.responses)['nfc11'],
            nfc12: JSON.parse(data.responses)['nfc12'],
            nfc13: JSON.parse(data.responses)['nfc13'],
            nfc14: JSON.parse(data.responses)['nfc14'],
            nfc15: JSON.parse(data.responses)['nfc15'],
            nfc16: JSON.parse(data.responses)['nfc16'],
            nfc17: JSON.parse(data.responses)['nfc17'],
            nfc18: JSON.parse(data.responses)['nfc18']
          });
        }
      };

var uncertainty_scale = ['1', '2', '3', '4', '5'];
var uncertainty = {
        type: 'survey-likert',
        questions: [
          {prompt: "Uncertainty stops me from having a strong opinion.",
          name: 'uncertainty1',
          labels: uncertainty_scale, required:true},
          {prompt: "Being uncertain means that a person is disorganized.",
          name: 'uncertainty2',
          labels: uncertainty_scale, required:true},
          {prompt: "Uncertainty makes life intolerable.",
          name: 'uncertainty3',
          labels: uncertainty_scale, required:true},
          {prompt: "It's unfair having no guarantees in life.",
          name: 'uncertainty4',
          labels: uncertainty_scale, required:true},
          {prompt: "My mind can't be relaxed if I don't know what will happen tomorrow.",
          name: 'uncertainty5',
          labels: uncertainty_scale, required:true},
          {prompt: "Uncertainty makes me uneasy, anxious, or stressed.",
          name: 'uncertainty6',
          labels: uncertainty_scale, required:true},
          {prompt: "Unforeseen events upset me greatly.",
          name: 'uncertainty7',
          labels: uncertainty_scale, required:true},
          {prompt: "It frustrates me not having all the information I need.",
          name: 'uncertainty8',
          labels: uncertainty_scale, required:true},
          {prompt: "Uncertainty keeps me from living a full life.",
          name: 'uncertainty9',
          labels: uncertainty_scale, required:true},
          {prompt: "One should always look ahead so as to avoid surprises.",
          name: 'uncertainty10',
          labels: uncertainty_scale, required:true},
          {prompt: "A small unforeseen event can spoil everything, even with the best planning.",
          name: 'uncertainty11',
          labels: uncertainty_scale, required:true},
          {prompt: "When it's time to act, uncertainty paralyses me.",
          name: 'uncertainty12',
          labels: uncertainty_scale, required:true},
          {prompt: "Being uncertain means that I am not first rate.",
          name: 'uncertainty13',
          labels: uncertainty_scale, required:true},
          {prompt: "When I am uncertain, I can't go forward.",
          name: 'uncertainty14',
          labels: uncertainty_scale, required:true},
          {prompt: "When I am uncertain, I can't function very well.",
          name: 'uncertainty15',
          labels: uncertainty_scale, required:true},
          {prompt: "Unlike me, others seem to know where they are going with their lives.",
          name: 'uncertainty16',
          labels: uncertainty_scale, required:true},
          {prompt: "Uncertainty makes me vulnerable, unhappy, or sad.",
          name: 'uncertainty17',
          labels: uncertainty_scale, required:true},
          {prompt: "I always want to know what the future has in store for me.",
          name: 'uncertainty18',
          labels: uncertainty_scale, required:true},
          {prompt: "I can't stand being taken by surprise.",
          name: 'uncertainty19',
          labels: uncertainty_scale, required:true},
          {prompt: "The smallest doubt can stop me from acting.",
          name: 'uncertainty20',
          labels: uncertainty_scale, required:true},
          {prompt: "I should be able to organize everything in advance.",
          name: 'uncertainty21',
          labels: uncertainty_scale, required:true},
          {prompt: "Being uncertain means that I lack confidence.",
          name: 'uncertainty22',
          labels: uncertainty_scale, required:true},
          {prompt: "I think it's unfair that other people seem to be sure about their future.",
          name: 'uncertainty23',
          labels: uncertainty_scale, required:true},
          {prompt: "Uncertainty keeps me from sleeping soundly.",
          name: 'uncertainty24',
          labels: uncertainty_scale, required:true},
          {prompt: "I must get away from all uncertain situations.",
          name: 'uncertainty25',
          labels: uncertainty_scale, required:true},
          {prompt: "The ambiguities in life stress me.",
          name: 'uncertainty26',
          labels: uncertainty_scale, required:true},
          {prompt: "I can't stand being undecided about my future.",
          name: 'uncertainty27',
          labels: uncertainty_scale, required:true}
        ],
        preamble: 'For the following items, please indicate how characteristic the statement is of you. Select a number to do so, where 1 = not at all characteristic of me and 5 = entirely characteristic of me.',
        scale_width: 350,
        on_finish: function(data) {
          jsPsych.data.addProperties({
            uncertainty1: JSON.parse(data.responses)['uncertainty1'],
            uncertainty2: JSON.parse(data.responses)['uncertainty2'],
            uncertainty3: JSON.parse(data.responses)['uncertainty3'],
            uncertainty4: JSON.parse(data.responses)['uncertainty4'],
            uncertainty5: JSON.parse(data.responses)['uncertainty5'],
            uncertainty6: JSON.parse(data.responses)['uncertainty6'],
            uncertainty7: JSON.parse(data.responses)['uncertainty7'],
            uncertainty8: JSON.parse(data.responses)['uncertainty8'],
            uncertainty9: JSON.parse(data.responses)['uncertainty9'],
            uncertainty10: JSON.parse(data.responses)['uncertainty10'],
            uncertainty11: JSON.parse(data.responses)['uncertainty11'],
            uncertainty12: JSON.parse(data.responses)['uncertainty12'],
            uncertainty13: JSON.parse(data.responses)['uncertainty13'],
            uncertainty14: JSON.parse(data.responses)['uncertainty14'],
            uncertainty15: JSON.parse(data.responses)['uncertainty15'],
            uncertainty16: JSON.parse(data.responses)['uncertainty16'],
            uncertainty17: JSON.parse(data.responses)['uncertainty17'],
            uncertainty18: JSON.parse(data.responses)['uncertainty18'],
            uncertainty19: JSON.parse(data.responses)['uncertainty19'],
            uncertainty20: JSON.parse(data.responses)['uncertainty20'],
            uncertainty21: JSON.parse(data.responses)['uncertainty21'],
            uncertainty22: JSON.parse(data.responses)['uncertainty22'],
            uncertainty23: JSON.parse(data.responses)['uncertainty23'],
            uncertainty24: JSON.parse(data.responses)['uncertainty24'],
            uncertainty25: JSON.parse(data.responses)['uncertainty25'],
            uncertainty26: JSON.parse(data.responses)['uncertainty26'],
            uncertainty27: JSON.parse(data.responses)['uncertainty27']
          });
        }
      };
