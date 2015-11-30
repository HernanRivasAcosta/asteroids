function Game(name)
{
  this.name = name;

  this.children = [];

  // Network
  this.net = null;
  this.onJoin = game_onJoin;
  this.onTick = game_onTick;
  this.onPlayerJoined = game_onPlayerJoined;
  this.onPlayerLeft = game_onPlayerLeft;

  // GameActor functions
  this.init   = game_init;
  this.update = game_update;
  this.render = game_render;

  this.remotePlayers = new Object();

  this.onShoot = function(x, y, r)
                 {
                   msg = {action: "shoot",
                          position: {x: x, y: y, r: r}};
                   this.net.send(msg);
                 };
  this.onInputChange = function(up, left, right)
                       {
                         msg = {action: "input",
                                position: {x: x, y: y, r: r}};
                         this.net.send(msg);
                       };

  // GameActorContainer functions
  this.addChild = function(child)
                  {
                    if(child.parent != this)
                    {
                      this.children.push(child);
                      child.parent = this;  
                    }
                  };
  this.removeChild = function(child)
                     {
                       if(child.parent == this)
                       {
                         child.flaggedForRemotion = true;
                         child.parent = null;
                       }
                     };
}

function game_init()
{
  this.net = new Net("ws://" + window.location.host + "/ws", this.name);
  this.net.connect(this);
}

//==============================================================================
// Network functions
//==============================================================================
function game_onJoin(x, y, r, players)
{
  this.addChild(new PlayerShip(this, this.name, true, x, y));

  var l = players.length;
  for(var i = 0; i < l; i++)
  {
    this.onPlayerJoined(players[i]);
  }
}

function game_onTick(data)
{
  //var players = data.players;
  var events = data.events;

  var l = events.length;
  for(var i = 0; i < l; i++)
  {
    game_handleEvent.apply(this, [events[i]]);
  }
}

function game_onPlayerJoined(player)
{
  var name = player.name;
  var x = player.position.x;
  var y = player.position.y;
  var newShip = new PlayerShip(this, name, false, x, y);
  newShip.model.r = player.position.r;
  this.addChild.apply(this, [newShip]);
  this.remotePlayers[name] = newShip;
}

function game_onPlayerLeft(name)
{
  var ship = this.remotePlayers[name];
  delete this.remotePlayers[name];
  this.removeChild.apply(this, [ship]);
}

//==============================================================================
// Utils
//==============================================================================
function game_handleEvent(e)
{
  switch(e.type)
  {
    case "bullet":
      var tickDiff = this.net.tick - e.tick;
      var b = new Bullet(e.position.x, e.position.y, e.position.r, tickDiff);
      b.init();
      this.addChild(b);
      break;
    default:
      console.log("unexpected event of type " + e.type);
      break;
  }
}

//==============================================================================
// Update and render
//==============================================================================
function game_update()
{
  // Notify the network module
  this.net.onLocalTick();

  // Update all the children (and remove the ones that were flagged)
  var l = this.children.length;
  for(var i = 0; i < l; i++)
  {
    var c = this.children[i];
    if(c.flaggedForRemotion)
    {
      this.children.splice(i, 1);
      l--;
      i--;
    }
    else
    {
      c.update.apply(c);
    }
  }
}

function game_render()
{
  // Render all the children (and remove the ones that were flagged)
  var l = this.children.length;
  for(var i = 0; i < l; i++)
  {
    var c = this.children[i];
    if(c.flaggedForRemotion)
    {
      this.children.splice(i, 1);
      l--;
      i--;
    }
    else
    {
      c.render.apply(c, [ctx]);
    }
  }
}