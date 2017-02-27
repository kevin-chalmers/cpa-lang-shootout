-module(blackscholes).
-export([start/0]).

cummulative_normal_distribution(Input) ->
  if
    Input < 0.0 ->
      Input = -Input,
      Sign = true;
    Input >= 0.0 ->
      Sign = false
  end,
  Prime_of_x = (math:exp(-0.5 * Input * Input) * 0.39894228040143270286),

  Input.

start() ->
  cummulative_normal_distribution(5).
