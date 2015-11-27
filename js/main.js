// Assets
var ship = ['white', [0, -15], [6, 5], [-6, 5]];

// Initialization
var canvas = document.getElementById('canvas');
var ctx = canvas.getContext('2d');

// Variables
var shipX = canvas.width / 2;
var shipY = canvas.height / 2;
var shipSpeedX = 0;
var shipSpeedY = 0;
var shipMaxSpeed = 2;
var shipAcceleration = .2;
var shipDeceleration = .1;
var shipRotation = 0;
var shipRotationSpeed = .1;

var ks = new Object();

// Input handling
document.onkeydown = onkeydown;
function onkeydown(e) { ks[e.keyCode] = true; }

document.onkeyup = onkeyup;
function onkeyup(e) { delete ks[e.keyCode]; }

function handleInput()
{
    if(ks['38']) // Up
    {
        shipSpeedX += shipAcceleration * Math.sin(shipRotation);
        shipSpeedY -= shipAcceleration * Math.cos(shipRotation);;
    }
    else
    {
        //shipSpeed -= shipDeceleration;
        //if (shipSpeed < 0)
        //    shipSpeed = 0;
    }

    if(ks['37']) // Left
    {
        shipRotation -= shipRotationSpeed;
    }
    if(ks['39']) // Right
    {
        shipRotation += shipRotationSpeed;
    }
}

// Logic update
function update()
{
    handleInput();

    shipX += shipSpeedX;
    shipY += shipSpeedY;
}

// Rendering
function draw() {
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    drawShape(ctx, ship, shipX, shipY, shipRotation);

    requestAnimationFrame(draw);
}

function drawShape(ctx, shape, x, y, r)
{
    ctx.strokeStyle = shape[0];
    ctx.beginPath();

    var l = shape.length;
    for (var i = 1; i < l; i++)
    {
        line(shape[i], x, y, r, i == 1);
    }
    ctx.closePath();
    ctx.stroke();
}

function line(p, x, y, r, move)
{
    // Translate and rotate the point
    p = translate(rotate(p, r), x, y);
    if (move)
        ctx.moveTo(p[0], p[1]);
    else
        ctx.lineTo(p[0], p[1]);
}

// Math utils
function rotate(p, r)
{
    sin = Math.sin(r);
    cos = Math.cos(r);
    return [p[0] * cos - p[1] * sin,
            p[0] * sin + p[1] * cos];
}
function translate(p, x, y) { return [p[0] + x, p[1] + y]; }

// Start game
draw();
window.setInterval(update, 1000 / 30); // 30fps