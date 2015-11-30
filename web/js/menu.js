function Menu(callback)
{
  this.callback = callback;
  this.nameString = "";
  this.underscore = "_";
  this.frame = 0;

  this.init = function() {};
  this.update = menu_nameInputScreenUpdate;
  this.render = menu_nameInputScreenRender;
}

function menu_nameInputScreenUpdate()
{
  var keys = allNewKeys();

  // Makes the cursor blink
  if(++this.frame % 30 == 0)
  {
    this.underscore = this.underscore == '' ? '_' : '';
  }

  // TODO: The text input system needs work
  if (this.nameString.length < 3)
  {
    for(var i = 0; i < keys.length; i++)
    {
      if(keys[i] >= 65 && keys[i] <= 90)
      {
        this.nameString += String.fromCharCode(keys[i]);
      }
    }
  }
  if(keyWasJustPressed(KEY_BACKSPACE))
  {
    this.nameString = this.nameString.substring(0, this.nameString.length - 1);
  }

  if(keyWasJustPressed(KEY_ENTER))
  {
    this.callback(this.nameString);
  }
}
function menu_nameInputScreenRender()
{
  renderText('name: ' + this.nameString + this.underscore, new Point(20, hh), 20);
}