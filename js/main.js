// Initialisation
var canvas = document.getElementById('canvas');
var ctx = canvas.getContext('2d');

// Variables
var w = canvas.width;
var h = canvas.height;

var ship = new Model(shipModelA, w / 2, h / 2, 0, 'white');

var shipSpeed = new Point();
var shipMaxSpeed = 8;
var shipAcceleration = .3;
var shipDeceleration = .1;
var shipRotationSpeed = .1;

function handleInput()
{
  if(ks['38']) // Up
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
  else
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

  if(ks['37']) // Left
  {
    ship.r -= shipRotationSpeed;
  }
  if(ks['39']) // Right
  {
    ship.r += shipRotationSpeed;
  }
}

// Logic update
function update()
{
  handleInput();
  ship.position.add(shipSpeed);

  if(ship.position.x > w)
    ship.position.x -= w;
  if(ship.position.x < 0)
    ship.position.x += w;
  if(ship.position.y > h)
    ship.position.y -= h;
  if(ship.position.y < 0)
    ship.position.y += h;
}

// Rendering
function render() {
  ctx.clearRect(0, 0, w, h);

  drawShape(ctx, ship);

  requestAnimationFrame(render);
}

// Start game
render();
window.setInterval(update, 1000 / 30); // 30fps