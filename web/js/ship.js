function PlayerShip(game, name, userControlled, x, y)
{
  this.game = game;
  this.name = name;
  this.model = new Model(shipModelA, x, y, 0, 'white');
  this.speed = new Point();
  this.maxSpeed = 8;
  this.acceleration = .3;
  this.deceleration = .05;
  this.rotationSpeed = .1;

  if(userControlled)
    this.update = ship_localUpdate;
  else
    this.update = ship_remoteUpdate;

  this.render = function(ctx)
                {
                  renderText(this.name, this.model.position.clone().add(new Point(25, 0)), 10, '#999');
                  renderModel(ctx, this.model);
                };
}

//==============================================================================
// Update
//==============================================================================
function ship_localUpdate()
{
  if(keyIsDown(KEY_UP))
  {
    ship_accelerate.apply(this);
  }
  else
  {
    ship_decelerate.apply(this);
  }

  if(keyIsDown(KEY_LEFT))
  {
    this.model.r -= this.rotationSpeed;
  }
  if(keyIsDown(KEY_RIGHT))
  {
    this.model.r += this.rotationSpeed;
  }

  this.model.position.add(this.speed);

  ship_constraint.apply(this);

  if(keyWasJustPressed(KEY_SPACEBAR))
  {
    ship_shoot.apply(this);
  }
}

function ship_remoteUpdate()
{
}

//==============================================================================
// Utils
//==============================================================================
function ship_constraint()
{
  if(this.model.position.x > w)
    this.model.position.x -= w;
  if(this.model.position.x < 0)
    this.model.position.x += w;
  if(this.model.position.y > h)
    this.model.position.y -= h;
  if(this.model.position.y < 0)
    this.model.position.y += h;
}

function ship_accelerate()
{
  // Accelerate in the direction the ship is pointing
  this.speed.x += this.acceleration * Math.sin(this.model.r);
  this.speed.y -= this.acceleration * Math.cos(this.model.r);

  // Limit the top speed
  if (this.speed.modulus() > this.maxSpeed)
  {
    this.speed.normalize(this.maxSpeed);
  }

  // Set the model with the engine on
  this.model.vertices = shipModelB;
}

function ship_decelerate()
{
  // Decelerate
  var newSpeed = this.speed.modulus() - this.deceleration;
  if(newSpeed <= 0)
    this.speed.set(0, 0);
  else
    this.speed.normalize(newSpeed);

  // Set the model with the engine off
  this.model.vertices = shipModelA;
}

function ship_shoot()
{
  this.game.onShoot(this.model.position.x, this.model.position.y, this.model.r);
}