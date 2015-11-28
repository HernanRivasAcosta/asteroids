function drawShape(ctx, model)
{
  ctx.strokeStyle = model.color;
  ctx.beginPath();

  var vertices = model.vertices;
  for (var i = 0; i < vertices.length; i++)
  {
    for (var j = 0; j < vertices[i].length; j++)
    {
      line(ctx, vertices[i][j], model.position, model.r, j == 0);
    }
  }
  ctx.closePath();
  ctx.stroke();
}

function line(ctx, vertex, p, r, move)
{
  // Translate and rotate the point
  vertex = rotate(vertex, r);
  vertex.add(p);
  if (move)
    ctx.moveTo(vertex.x, vertex.y);
  else
    ctx.lineTo(vertex.x, vertex.y);
}