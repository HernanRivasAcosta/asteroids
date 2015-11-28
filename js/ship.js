function accelerate()
{
  // Accelerate in the direction the ship is pointing
  shipSpeed.x += shipAcceleration * Math.sin(ship.r);
  shipSpeed.y -= shipAcceleration * Math.cos(ship.r);

  // Limit the top speed
  if (shipSpeed.modulus() > shipMaxSpeed)
  {
    shipSpeed.normalize(shipMaxSpeed);
  }

  // Set the model with the engine on
  ship.vertices = shipModelB;
}

function decelerate()
{
  // Decelerate
  var newSpeed = shipSpeed.modulus() - shipDeceleration;
  if(newSpeed <= 0)
    shipSpeed.set(0, 0);
  else
    shipSpeed.normalize(newSpeed);

  // Set the model with the engine off
  ship.vertices = shipModelA;
}