function Bullet(ship)
{
  this.model = new Model(bulletModel, 0, 0, ship.model.r, 'white');
  this.speed = new Point(Math.sin(ship.model.r), -Math.cos(ship.model.r)).normalize(12);

  this.init = function()
              {
                // Spawn at the ship position plus
                console.log("dsabfusapjfau");
                this.model.position = ship.model.position.clone().add(this.speed.clone().normalize(15));
              };
  this.update = function()
                {
                  this.model.position.add(this.speed);
                  if(this.parent &&
                     (this.model.position.x > w ||
                      this.model.position.x < 0 ||
                      this.model.position.y > h ||
                      this.model.position.y < 0))
                  {
                    this.parent.removeChild(this);
                  }
                };
  this.render = function(ctx)
                {
                  drawShape(ctx, this.model);
                };
}

function PlayerShip(game)
{
  this.name = game.name;
  this.model = new Model(shipModelA, hw, hh, 0, 'white');
  this.speed = new Point();
  this.maxSpeed = 8;
  this.acceleration = .3;
  this.deceleration = .05;
  this.rotationSpeed = .1;

  this.update = function()
                {
                  if(keyIsDown(KEY_UP))
                  {
                    shipAccelerate(this);
                  }
                  else
                  {
                    shipDecelerate(this);
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

                  if(this.model.position.x > w)
                    this.model.position.x -= w;
                  if(this.model.position.x < 0)
                    this.model.position.x += w;
                  if(this.model.position.y > h)
                    this.model.position.y -= h;
                  if(this.model.position.y < 0)
                    this.model.position.y += h;

                  if(keyWasJustPressed(KEY_SPACEBAR))
                  {
                    shoot(this);
                  }
                };
  this.render = function(ctx)
                {
                  console.log("xxxx");
                  renderText(this.name, this.model.position.clone().add(new Point(25, 0)), 10, '#999');
                  drawShape(ctx, this.model);
                };
}

function shipAccelerate(ship)
{
  // Accelerate in the direction the ship is pointing
  ship.speed.x += ship.acceleration * Math.sin(ship.model.r);
  ship.speed.y -= ship.acceleration * Math.cos(ship.model.r);

  // Limit the top speed
  if (ship.speed.modulus() > ship.maxSpeed)
  {
    ship.speed.normalize(ship.maxSpeed);
  }

  // Set the model with the engine on
  ship.model.vertices = shipModelB;
}

function shipDecelerate(ship)
{
  // Decelerate
  var newSpeed = ship.speed.modulus() - ship.deceleration;
  if(newSpeed <= 0)
    ship.speed.set(0, 0);
  else
    ship.speed.normalize(newSpeed);

  // Set the model with the engine off
  ship.model.vertices = shipModelA;
}

function shoot(ship)
{
  var b = new Bullet(ship);
  b.init();
  ship.parent.addChild(b);
}