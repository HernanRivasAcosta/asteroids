function Game(name, ip)
{
  this.name = name;
  this.ip = ip;

  this.children = [];

  this.init   = function() { gameInit(this); };
  this.update = function() { gameUpdate(this); };
  this.render = function() { gameRender(this); };

  this.addChild = function(child)
                  {
                    if(child.parent != this)
                    {
                      this.children[this.children.length] = child;
                      child.parent = this;  
                    }
                  };
  this.removeChild = function(child)
                     {
                       if(child.parent == this)
                       {
                         this.children[this.children.length] = child;
                         child.parent = null;
                       }
                     };
}

function gameInit(screen)
{
  console.log("dskfabhn");
  screen.addChild(new PlayerShip(screen));
}

function gameUpdate(screen)
{
  var l = screen.children.length;
  console.log("upd: " + l);
  for(var i = 0; i < l; i++)
  {
    screen.children[i].update();
  }
}

function gameRender(screen)
{
  var l = screen.children.length;
  for(var i = 0; i < l; i++)
  {
    screen.children[i].render(ctx);
  }
}