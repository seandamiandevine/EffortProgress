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

var i4 = {
    type: 'instructions',
    pages: [
  "<p>Great! You should now be familiar with how the game works. Moving forward, we won't tell you whether your answers are right or wrong.</p>"+
  "<p>Also, to make sure you're always paying attention, you'll now have a time limit to make each choice (press "+trialkeys[0].toUpperCase()+" or "+trialkeys[1].toUpperCase()+").</p>" +
  "<p>So, it is important for you to respond accurately AND quickly.</p>",

  '<img src = "/static/images/blue.png" align="left" height="300" width="300">'+
  '<img src = "/static/images/purple.png" align="right" height="300" width="300">'+
  '<p>Before each series of numbers, you will have the choice to enter two portals: the <b>blue</b> portal or the <b>purple</b> portal.</p>'+
  '<p>When you choose a portal, the background will change colors to reflect your choice. You can think of this as being in the <b>blue world</b> or the <b>purple world</b></p>'+
  "<p>The game is a bit different depending on which world you're in, but the rules are always the same as the ones you've already learned.",

  "<p>To show you how the game is different depending on the world you choose, you're going to practice choosing each portal.</p>"+
  "<p>To make sure you visit both worlds for enough time, we're going to tell you which portals to choose during the practice. Just follow the instructions on the bottom of the screen. </p>"+
  '<p>To choose the portal on the <b>left</b> press the '+choicekeys[0].toUpperCase()+' key. To choose the portal on the <b>right</b>, press the '+choicekeys[1].toUpperCase()+' key.</p>'+
  '<p><b>KEEP IN MIND</b>: The blue and purple portals may not always be on the same side! Make sure to consider your choice before making it.</p>' +
  '<br><br><p>WHATEVER PORTAL YOU CHOOSE, REMEMBER THE RULES!<p>'+
  '<p style = "color:'+numcolors[0]+';"><b>When the number is '+numcolors[0].toUpperCase()+': '+trialkeys[0].toUpperCase()+' = Less than 5; '+trialkeys[1].toUpperCase()+' = More than 5</b></p>'+
  '<p style = "color:'+numcolors[1]+';"><b>When the number is '+numcolors[1].toUpperCase()+': '+trialkeys[0].toUpperCase()+' = Even; '+trialkeys[1].toUpperCase()+' = Odd</b></p>'+
  "<br><p>Finally, <b>don't forget you have a time limit to response!</b> So, make sure you pay attention!"+
  '<p>Press NEXT to practice choosing the portals!</p>'
    ],
    show_clickable_nav: true,
};
var i5 = {
    type: 'instructions',
    pages: function(){
    if (condition==1) {
      h =  [  "<p>OK! Now you should have a good grasp on how the game works and how to choose which world you want to play the game in. There's just one last thing to learn about.</p>"+
              "<p>If you look at the top of the screen, you will see a progress bar that tells you how many numbers you have responded to in that world.</p>"+
              "<p>This means that every time you make a choice in the game (press the "+trialkeys[0].toUpperCase()+" or "+trialkeys[1].toUpperCase()+" keyboard key), the progress bar will fill up with green. When the bar is filled up all the way, you'll be done in that world.</p>"+
              "<p>The progress bar will reset every time you enter a new portal. In other words, every time you respond, you get closer to finishing that world.</p>",

              "<p>Once you're done a world, you will also see a progress screen showing how many more worlds you have left to play the game in and how many you've already finished.</p>"+
              "<p>This will be represented by stars, with yellow stars indicating how many worlds you've completed and greyed out ones showing how many you have left to complete.</p>"+
              "<p>You will see the stars after each world and whenever you're choosing a portal." +

              "<br><br><p>For example, here the stars would show that you were halfway through the experiment:</p>"+
              "<br><span style='color:orange; font-size: 30pt' class='fa fa-star checked'></span>"+
              "<span style='color:orange; font-size: 30pt' class='fa fa-star'></span>"+
              "<span style='color:orange; font-size: 30pt' class='fa fa-star'></span>"+
              "<span style='color:black; font-size: 30pt' class='fa fa-star'></span>"+
              "<span style='color:black; font-size: 30pt' class='fa fa-star'></span>"+
              "<span style='color:black; font-size: 30pt' class='fa fa-star'></span><br>"+
              "<p>This is telling you that you would have finished 3 worlds (3 yellow stars), but have 3 left to go.</p>"+
              "<p>Every time you complete a world, a new star will be added to show your pogress.</p>",

              "<p>Throughout the game, you may notice differences between the worlds. If you develop a preference, you can feel free to choose one world more than the other. </p>"+
              "<p>Please avoid using simple rules such as alternating back and forth between the worlds. Instead, try to make a decision every time.</p> ",

              "<p>OK! You're ready to start the real game. From now on, you will be free to choose any portal you like as many times as you like.</p>"+
              "<p>When you are ready to start, press NEXT!</p>"
            ]
  } else {
      h = [   "<p>Throughout the game, you may notice differences between the worlds. If you develop a preference, you can feel free to choose one world more than the other. </p>"+
              "<p>Please avoid using simple rules such as alternating back and forth between the worlds. Instead, try to make a decision every time.</p>" +
              "<p>OK! You're ready to start the real game. From now on, you will be free to choose any portal you like as many times as you like.</p>"+
              "<p>When you are ready to start, press NEXT!</p>"
         ]
    }
    return h
  },
    show_clickable_nav: true,
    on_start: function(){ // show progress if condition == 1
      document.body.style.background = "grey" // reset background from last world
      jsPsych.setProgressBar(0.5); // set progress bar for salience
      if (condition == 1){
        document.querySelector('#jspsych-progressbar-container').style.display = 'inline-block';
      }
    },
    on_finish: function(){
      jsPsych.setProgressBar(0); // reset progress bar for first block
    }
  };
