module Cfg = Cfg
module Auth = Auth
module Prediction_markets = Prediction_markets
module Fluxum_adapter = Fluxum_adapter

let command : Command.t =
  Command.group
    ~summary:"Kalshi Command System"
    [ Prediction_markets.command ]
