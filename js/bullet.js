function Bullet(x, y, r, ticks)
{
  this.model = new Model(bulletModel, x, y, r, 'white');
  this.speed = new Point(Math.sin(r), -Math.cos(r)).normalize(12);

  this.init = function()
              {
                this.model.position.add(this.speed.clone().multiply(ticks + 1));
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
                  renderModel(ctx, this.model);
                };
}