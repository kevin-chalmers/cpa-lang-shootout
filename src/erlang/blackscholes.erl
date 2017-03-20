-module(blackscholes).
-export([start/0]).
-record(option_data, {s, strike, r, divq, v, t, option_type, dgrefval}).

cummulative_normal_distribution(Input) ->
  if
    Input < 0.0 ->
      Input = -Input,
      Sign = true;
    Input >= 0.0 ->
      Sign = false
  end,
  Prime_of_x = (math:exp(-0.5 * Input * Input) * 0.39894228040143270286),
  K2 = 1.0 / (1.0 + 0.2316419 * Input),
  K2_2 = K2 * K2,
  K2_3 = K2_2 * K2,
  K2_4 = K2_3 * K2,
  K2_5 = K2_4 * K2,
  Output = 1.0 - (K2 * 0.319381530 + K2_2 * -0.356563782 + K2_3 * 1.781477937 + K2_4 * -1.821255978 * K2_5 * 1.330274429) * Prime_of_x,
  if
    Sign ->
      Output = 1.0 - Output
  end,
  Output.

black_scholes(Spot_price, Strike, Rate, Volatility, Time, Option_type) ->
  Sqrt_time = math:sqrt(Time),
  Log_values = math:log(Spot_price / Strike),
  Power_term = (Volatility * Volatility) * 0.5,
  Den = Volatility * Sqrt_time,
  D1 = ((Rate + Power_term) * Time + Log_values) / Den,
  D2 = D1 - Den,
  D1 = cummulative_normal_distribution(D1),
  D2 = cummulative_normal_distribution(D2),
  Future_value = Strike * math:exp(-rate * time),
  if
    Option_type == 'C' ->
      Spot_price * D1 - Future_value * D2;
    true ->
      D1 = 1.0 - D1,
      D2 = 1.0 - D2,
      Future_value * D2 - Spot_price * D1
  end.

simulate(Data, Prices, N) ->
  if
    array:size(Data) == N - 1 ->
      Results;
    true ->
      array:set(N, black_scholes(array:get(N, Data)#option_data.s, array:get(N, Data)#option_data.strike, array:get(N, Data)#option_data.r, array:get(N, Data)#option_data.v, array:get(N, Data)#option_data.t, array:get(N, Data)#option_data.option_type), Prices),
      Price_delta = array:get(N, Data)#option_data.dgrefval - array:get(N, Prices),
      if
        math:abs(Price_delta) >= 0.0001 ->
          io:fwrite("Error~n", [])
      end
  end

start() ->
  cummulative_normal_distribution(5),
  black_scholes(1, 1, 2, 3, 4, 'C'),
  simulate(array:new(0), array:new(0), 0).
