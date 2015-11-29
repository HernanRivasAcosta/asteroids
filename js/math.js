function Point(x, y)
{
  this.x = x || 0;
  this.y = y || 0;

  this.clone = function() { return new Point(this.x, this.y); };
  this.set = function(x, y) { this.x = x || 0; this.y = y || 0; return this; };
  this.add = function(p) { this.x += p.x; this.y += p.y; return this; };
  this.multiply = function(s) { this.x *= s; this.y *= s; return this; };
  this.modulus = function() { return Math.sqrt(this.x * this.x + this.y * this.y); };
  this.normalize = function(s)
                   {
                     var mod = this.modulus();
                     if(mod != 0)
                     {
                       this.multiply((s || 1) / mod);
                     }
                     return this;
                   };
  this.rotate = function(r)
                {
                  var sin = Math.sin(r);
                  var cos = Math.cos(r);
                  var x = this.x;
                  var y = this.y;
                  this.x = x * cos - y * sin;
                  this.y = x * sin + y * cos;
                  return this;
                };
  this.toString = function() { return "(" + this.x + ", " + this.y + ")"; };
}