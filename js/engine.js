function Model(vertices, x, y, r, color)
{
  this.vertices = vertices;
  this.position = new Point(x, y);
  this.r = r || 0;
  this.color = color || 'white';
}

// Input handling
var ks = new Object();

document.onkeydown = onkeydown;
function onkeydown(e) { ks[e.keyCode] = true; }

document.onkeyup = onkeyup;
function onkeyup(e) { delete ks[e.keyCode]; }