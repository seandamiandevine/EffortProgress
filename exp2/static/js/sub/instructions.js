// TASK INSTRUCTIONS FOR EFFORT PROGRESS TASK
var i1 = {
    type: 'instructions',
    pages: [
  '<p>Welcome to this study!</p><p>Use the navigation pane below to navigate through the instructions.</p>',

  '<p>In this study, we are going to ask you to play a game. Before getting started, we will spend some time explaining the rules to the game.</p>'+
  '<p>The rules are simple. Throughout the game, you will see a series of numbers.<p>'+
  '<p>Your job will be to press the <b>correct keyboard key</b> depending on the colour of the number.</p>'+
  "<p>The numbers will either be <b>"+numcolors[0]+"</b> or <b>"+numcolors[1]+"</b></p>."+
  "<p>Each color has its own rules. We will go over some these rules on the next screen.</p>",

  "<p>Let's start with what the rules are when the numbers are "+numcolors[0].toUpperCase()+":</p>"+
  '<ul><li>If the number is <b>less than 5</b>, press the '+trialkeys[0].toUpperCase()+' KEY on your keyboard.</li>'+
  '<li>If the number is <b>greater than 5</b>, press the '+ trialkeys[1].toUpperCase()+' KEY on your keyboard.</li>'+
  '</ul>',

  '<p>To be sure you understand the rules of the game so far, you will now have a chance to practice. Once you press NEXT below, the practice will begin.</p>'+
  '<p>We will start by only having green numbers. The rules are the same as we just explained:</p>'+
  "<p>Press the "+trialkeys[0].toUpperCase()+" key when the number is less than 5 and the "+trialkeys[1].toUpperCase()+" key when the number is greater than 5.</p>"+
  "<p>If you still don't fully understand the rules, you can go back to read them again using the PREVIOUS button. Otherwise, press NEXT below to start practicing!</p>"
    ],
    show_clickable_nav: true
};

var i2 = {
    type: 'instructions',
    pages: [
  "<p>Good job! Now we'll look at the rules for when the numbers are "+numcolors[1].toUpperCase()+":</p>"+
  '<ul><li>If the number is <b>even</b>, press the '+trialkeys[0].toUpperCase()+' KEY on your keyboard.</li>'+
  '<li>If the number is <b>odd</b>, press the '+trialkeys[1].toUpperCase()+' KEY on your keyboard.</li>'+
  '</ul>'+
  "<p><br>We'll practice playing the game now when all the numbers are only "+numcolors[1]+". Press NEXT to start!</p>"
    ],
    show_clickable_nav: true
};

var i3 = {
    type: 'instructions',
    pages: [
  "<p>Good job! Now that you've got the hang of the rules for each color, you'll practice playing the game when the numbers change colors!</p>"+
  "<p>This means that the numbers will sometimes be "+numcolors[0]+" and other times be "+numcolors[1]+". Your job  is to keep in mind what the rules for each colour are and give the correct answer.</p>"+
  '<b>REMEMBER: </b>When the number is <b>'+numcolors[0]+'</b>: '+trialkeys[0].toUpperCase()+' = Less than 5; '+trialkeys[1].toUpperCase()+' = Greater than 5.</p>'+
  '<b>BUT</b>, when the number is <b>'+numcolors[1]+'</b>: '+trialkeys[0].toUpperCase()+' = Even; '+trialkeys[1].toUpperCase()+' = Odd.</p>'
    ],
    show_clickable_nav: true
};

if(lengthfixed) {
  var lengthinst = "<p>The game will always be the same length, regardless of which deck you pick.</p>"
} else {
  var lengthinst = "<p>Sometimes the game will be longer and sometimes it will be shorter. This has <b>nothing to do with the decks</b> and happens randomly.</p>"
};

var i4 = {
    type: 'instructions',
    pages: [
  "<p>Great! You should now be familiar with how the game works. You'll play this game many times today, but moving forward, we won't tell you whether your answers are right or wrong.</p>"+
  "<p>To make sure you are paying attention, you will also be under time pressure to answer. You will have "+resptime/1000+" seconds to respond to each number on the screen.</p>" +
  "<p>There's one more thing you need to know. Before playing the game you just played, you'll have to choose a <b>deck</b>.</p>"+
  "<p>On the next page, we'll go over what the decks look like and what they do.</p>",

  "<p>Below, you can see all the different decks you can choose from.</p>"+
  "<p>Before each time you play the color/number game you've learnt so far, you will choose one of these decks.</p>" +
  "<p>Depending on which one you choose, the game may differ slightly, but the rules of the game will always be the <b>same</b>.</p>" +
  "<p>In the next part of the study, you will practice playing the game as if you chose each deck.</p>"+
  "<p>Press NEXT to learn more.</p>"+
  '<br><br>'+
  '<div>'+
    '<img src="static/images/deck1.png" style="float:center;" width="160" height="90"/>'+
    '<img src="static/images/deck2.png" style="float:center;" width="160" height="90"/>'+
    '<img src="static/images/deck3.png" style="float:center;" width="160" height="90"/>'+
    '<img src="static/images/deck4.png" style="float:center;" width="160" height="90"/>'+
    '<img src="static/images/deck5.png" style="float:center;" width="160" height="90"/>'+
    '<img src="static/images/deck6.png" style="float:center;" width="160" height="90"/>'+
  '</div>' +
  "<br><br>",

  "<p>In this next part of the study, you will see one of the decks appear on the screen and then practice playing the game as if you had chosen that deck.</p>"+
  "<p>Your job for now is to try and remember which deck leads to which version of the game. </p>"+
  "<p>You'll know which deck you've chosen because the symbol for that deck will show up in the background of the game.</p>"+
  lengthinst +
  "<p>Again, your job for now is just to learn how the game changes depending on the deck.</p>" +
  "<p>Click NEXT to continue.</p>",

  "<p>One last thing, some of the decks will make a <b>progress bar</b> appear during the color/number game. You can see an example of this below.</p>"+
  '<img src="static/images/progressinst.png" style="float:center;" width="20%" height="20%"/>' +
  '<br><br>' +
  "<p>When this happens, the rules to the game are the same, but everytime you make a response, the progress bar will fill up with green. When it is all filled up, you'll be finished the game in that deck.</p>",

  "<p>This is enough explaining for now. If you do not understand the instructions, use the PREVIOUS button below to go back.</p>"+
  "<p>When you click NEXT below, you will start playing the games as if you chose each of the decks.</p>" +
  "<p>Remember: Respond as quickly AND accurately as possible and try your best to learn how each deck affects the game!</p>"

    ],
    show_clickable_nav: true,
  };

if(lengthfixed) {
  var lengthinst2 = "<p>Again, the duration of the game will always be the same whichever deck you pick.</p>"
} else {
  var lengthinst2 = "<p>Again, the duration of the game won't always be the same: sometimes it will be longer and sometimes it will be shorter. This has nothing to do with your deck choice.</p>"
};
var i5 = {
    type: 'instructions',
    pages: [
      "<p>Great job! Now that you know how the decks affect the game, you will now have to pick between them.</p>"+
      "<p>In this part of the study, you will have to pick between two of the decks you've seen at a time. You will make many of these decisions throughout the study, one after the other.</p>"+
      "<p>After picking a deck, a red rectangle will confirm your choice, and you will play the version of the game you learnt comes from that deck.</p>"+
      lengthinst2 +
      "<p>Which deck you choose is <b>entirely up to you</b>. We are interested in your preferences, so please pick whichever deck leads to the version of the game you'd like to play!</p>"+
      "<p>When you are ready to start, press NEXT!</p>"
    ],
    show_clickable_nav: true,
    on_start: function() {

      document.body.style.background = "grey";
      document.querySelector('#jspsych-progressbar-container').style.display = 'none';
    },
    on_finish: function() {

      // update variables for testphase
      r = 1 // run number
      p = 0 // pair

    }
};

var finish2q = {
    type: 'instructions',
    pages: [
      "<p>Thank you so much for participating in our study!</p>" +
      "<p>You're done the main part of the study and you're almost done the whole thing! We just have a few short questions for you. Press NEXT to view them.</p>"
    ],
    show_clickable_nav: true,
    on_start: function() {

      document.body.style.background = "grey";
      document.querySelector('#jspsych-progressbar-container').style.display = 'none';

    }
};

var end = {
    type: 'instructions',
    pages: ["You are done the experiment! Press NEXT to finish and receive your payment. Thank you so much!"],
    show_clickable_nav: true
};
