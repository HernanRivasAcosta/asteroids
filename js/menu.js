var nameString = "";
var hostString = "";
var underscore = "_";

var frame = 0;

function Menu(callback)
{
  this.callback = callback;

  this.init = function() {};
  this.update = function() { nameInputScreenUpdate(this); };
  this.render = function() { nameInputScreenRender(this); };
}

function nameInputScreenUpdate(screen)
{
  var keys = allNewKeys();

  // Makes the cursor blink
  if(++frame % 30 == 0)
  {
    underscore = underscore == '' ? '_' : '';
  }

  // TODO: The text input system needs work
  if (nameString.length < 3)
  {
    for(var i = 0; i < keys.length; i++)
    {
      if(keys[i] >= 65 && keys[i] <= 90)
      {
        nameString += String.fromCharCode(keys[i]);
      }
    }
  }
  if(keyWasJustPressed(KEY_BACKSPACE))
  {
    nameString = nameString.substring(0, nameString.length - 1);
  }

  // Go to the host pick screen
  if(keyWasJustPressed(KEY_ENTER))
  {
    //this.update = hostInputScreenUpdate;
    //this.render = hostInputScreenRender;
    screen.callback(nameString);
  }
}
function nameInputScreenRender(screen)
{
  renderText('name: ' + nameString + underscore, new Point(20, hh), 20);
}