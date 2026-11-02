module Cfg = Cfg
module Auth = Auth
module Prediction_markets = Prediction_markets
module Trade_v2 = Trade_v2
module Fluxum_adapter = Fluxum_adapter

let command : Command.t =
  Command.group
    ~summary:"Kalshi Command System"
    [ Prediction_markets.command ]
