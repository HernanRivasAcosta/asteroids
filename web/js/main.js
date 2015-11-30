//==============================================================================
// Screens
//==============================================================================
var menu = new Menu(onMenuDone);
menu.init.apply(menu);

var game;
var currentScreen = menu;

//==============================================================================
// Navigation functions
//==============================================================================
function onMenuDone(name, ip)
{
  game = new Game(name);
  game.init.apply(game);
  currentScreen = game;
}

//==============================================================================
// Game loop
//==============================================================================
function update()
{
  if(currentScreen)
  {
    currentScreen.update.apply(currentScreen);
  }

  // Has to be done at the end if we want to use keyWasJustPressed
  refreshInput();
}

function render()
{
  if(currentScreen)
  {
    ctx.clearRect(0, 0, w, h);
    currentScreen.render.apply(currentScreen);
  }
  requestAnimationFrame(render);
};

//==============================================================================
// Start point
//==============================================================================
render();
window.setInterval(update, 1000 / 40); // 40fps