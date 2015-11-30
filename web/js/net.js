function Net(host, name)
{
  this.name = name;
  this.host = host;
  this.socket = null;

  this.tick = 0;
  this.currentMessage = 0;
  
  this.connect = function() { net_connect.apply(this, arguments);};
  this.send = function() { net_send.apply(this, arguments); };

  this.onLocalTick = function() { this.tick++; };
}

function net_send(data)
{
  // Get the current time
  var t = new Date().getMilliseconds();
  // Prepare the time and the id, the server will return them on the reply
  data.time = t;
  // Prepare the current tick
  data.tick = this.tick;
  // Send the data
  var dataStr = JSON.stringify(data);
  this.socket.send(dataStr);
}

function net_connect(game)
{
  console.log("INFO: establishing connection");
  this.socket = new WebSocket(this.host);
  var theName = this.name;
  var thisObj = this;
  this.socket.onopen = function(event)
                       {
                         console.log("INFO: connection established");
                         this.send("connect:" + theName);
                       };
  this.socket.onmessage = function(event)
                          {
                            //console.log("DEBUG: received " + event.data);

                            var msg = JSON.parse(event.data);
                            switch(msg.action)
                            {
                              case "joined":
                                game.onJoin.apply(game, [msg.position.x,
                                                         msg.position.y,
                                                         msg.position.r,
                                                         msg.players])
                                break;
                              case "tick":
                                game.onTick.apply(game, [msg]);
                                thisObj.tick = msg.tick;
                                break;
                              case "playerJoined":
                                game.onPlayerJoined.apply(game, [msg]);
                                break;
                              case "playerLeft":
                                game.onPlayerLeft.apply(game, [msg.name]);
                                break;
                              case "reply":
                                // Calculate the current ping
                                var diff = new Date().getMilliseconds() - msg.time;
                                console.log("DEBUG: current ping is " + diff);
                                break;
                              default:
                                console.log("WARN: unexpected event " + event.data);
                            }
                          }
}