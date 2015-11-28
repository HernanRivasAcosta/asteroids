ship = new Model(shipModelA, hw, hh, 0, 'white');
shipSpeed = new Point();
shipMaxSpeed = 12;
shipAcceleration = .3;
shipDeceleration = .05;
shipRotationSpeed = .1;

function Game(name, ip)
{
  this.name = name;
  this.ip = ip;

  this.update = gameUpdate;
  this.render = gameRender;
}

function gameUpdate(screen)
{
  if(keyIsDown(KEY_UP))
  {
    accelerate();
  }
  else
  {
    decelerate();
  }

  if(keyIsDown(KEY_LEFT))
  {
    ship.r -= shipRotationSpeed;
  }
  if(keyIsDown(KEY_RIGHT))
  {
    ship.r += shipRotationSpeed;
  }

  ship.position.add(shipSpeed);

  if(ship.position.x > w)
    ship.position.x -= w;
  if(ship.position.x < 0)
    ship.position.x += w;
  if(ship.position.y > h)
    ship.position.y -= h;
  if(ship.position.y < 0)
    ship.position.y += h;

  if(keyWasJustPressed(KEY_SPACEBAR))
  {

  }
}

function gameRender(screen)
{
  renderText(screen.name, ship.position.clone().add(new Point(25, 0)), 10, '#999');
  drawShape(ctx, ship);
}