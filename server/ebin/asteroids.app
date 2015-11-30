{application, asteroids, [
  {description, "Asteroids online"},
  {vsn, "1"},
  {modules, ['asteroids_app','asteroids_sup','game','ws_handler']},
  {registered, [asteroids_sup]},
  {applications, [
    lager,
    kernel,
    stdlib,
    cowboy,
    jiffy
  ]},
  {mod, {asteroids_app, []}},
  {env, []}
]}.
