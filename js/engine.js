// Main game objects
function Model(vertices, x, y, r, color)
{
  this.vertices = vertices;
  this.position = new Point(x, y);
  this.r = r || 0;
  this.visible = true;
  this.color = color || 'white';
}

//==============================================================================
// Input handling
//==============================================================================
var keys = new Object();

// Key listerners
document.onkeydown = onkeydown;
function onkeydown(e) { keys[e.keyCode] = keys[e.keyCode] || 1; }

document.onkeyup = onkeyup;
function onkeyup(e) { delete keys[e.keyCode]; }

// Helper functions
function keyWasJustPressed(keyCode)
{
  return keys[keyCode] == 1;
}

function allNewKeys()
{
  var r = [];
  
  for(var k in keys)
  {
    if(keys[k] == 1)
    {
      r[r.length] = k;
    }
  }
  
  return r;
}

function keyIsDown(keyCode)
{
  return keys[keyCode] == 2 || keys[keyCode] == 1;
}

function refreshInput()
{
  for(var k in keys)
  {
    keys[k] = 2;
  }
}

//==============================================================================
// Initialisation
//==============================================================================
var canvas = document.getElementById('canvas');
var ctx = canvas.getContext('2d');

var w = canvas.width;
var hw = w / 2;
var h = canvas.height;
var hh = h / 2;