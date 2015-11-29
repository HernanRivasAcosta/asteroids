//==============================================================================
// Screens
//==============================================================================
var menu = new Menu(onMenuDone);
menu.init();

var game;
var currentScreen = menu;

//==============================================================================
// Navigation functions
//==============================================================================
function onMenuDone(name, ip)
{
  game = new Game(name, ip);
  game.init();
  currentScreen = game;
}

//==============================================================================
// Game loop
//==============================================================================
function update()
{
  if(currentScreen)
  {
    currentScreen.update();
  }

  // Has to be done at the end if we want to use keyWasJustPressed
  refreshInput();
}

function render()
{
  if(currentScreen)
  {
    ctx.clearRect(0, 0, w, h);
    currentScreen.render();
  }
  requestAnimationFrame(render);
};

//==============================================================================
// Start point
//==============================================================================
render();
window.setInterval(update, 1000 / 30); // 30fps