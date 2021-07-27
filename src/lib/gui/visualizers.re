open! Core;

let get_observer_view = (~debug=true, ~delay=1.0, ()) => {
  let debug_printf = (s: string): unit =>
    if (debug) {
      print_string(s);
      Core.Out_channel.flush(stdout);
    };

  let last_update = ref(0.0);
  Referee.Game_observer.(
    event => {
      let delay_remain = delay -. (Unix.time() -. last_update^);
      if (Float.(delay_remain > 0.0)) {
        Thread.delay(delay_remain);
      };
      switch (event) {
      | Register(state) =>
        debug_printf @@ Printf.sprintf("registered\n");
        Render.render(state);
      | [@implicit_arity] PenguinPlacement(state, _, _) =>
        Render.render(state)
      | [@implicit_arity] TurnAction(state, color, act) =>
        debug_printf @@
        Printf.sprintf(
          "action from %s: %s\n",
          Common.Player_state.Player_color.show(color),
          Common.Action.show(act),
        );
        Render.render(state);
      | [@implicit_arity] Disqualify(opt_state, _) =>
        Option.iter(~f=Render.render, opt_state)
      | EndOfGame(result) =>
        let winners_str =
          List.map(~f=p => p#get_name(), result.winners)
          |> List.to_string(~f=Fun.id);
        debug_printf @@
        Printf.sprintf("Game has ended. Winners are: %s", winners_str);
      };
      last_update := Unix.time();
    }
  );
};
