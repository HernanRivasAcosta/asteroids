function renderModel(ctx, model)
{
  if(model.visible)
  {
    ctx.strokeStyle = model.color;
    ctx.beginPath();

    var vertices = model.vertices;
    for (var i = 0; i < vertices.length; i++)
    {
      for (var j = 0; j < vertices[i].length; j++)
      {
        render_line(ctx, vertices[i][j], model.position, model.r, j == 0);
      }
    }
    ctx.closePath();
    ctx.stroke();
  }
}

function renderText(string, position, size, color)
{
  ctx.font = size + 'px hyperspace';
  ctx.fillStyle = color || 'white';
  ctx.textBaseline = 'middle';
  ctx.textAlign = 'left';
  ctx.fillText(string, position.x, position.y);
}

function render_line(ctx, vertex, p, r, move)
{
  // Translate and rotate the point
  vertex = vertex.clone().rotate(r);
  vertex.add(p);
  if (move)
    ctx.moveTo(vertex.x, vertex.y);
  else
    ctx.lineTo(vertex.x, vertex.y);
}