-record(inp_item, {
  nid :: integer(),
  weight = 1 :: float()
}).
-record(out_item, {
  nid :: integer(),
  pid :: pid()
}).
-record(state, {
  nid :: integer(),
  component_type = neuron :: neuron | sensor | actuator,
  cortes_pid :: pid(),
  input = [] ::list(#inp_item{}),
  output = [] :: list(#out_item{}),
  bias = 0.0 :: float(),
  accum = 0.0 :: float(),
  signals = [] :: list(#inp_item{})
}).
-record(inp_config, {
  nid :: integer(),
  type = neuron :: sensor | neuron | actuator,
  input = [] :: list(#inp_item{}),
  bias = 0.0 :: float()
}).
-record(out_config, {
  nid :: integer(),
  output = [] :: list(#out_item{}),
  pid :: pid()
}).

-record(cortex_state, {
  genotype = [] :: list(#inp_config{}),
  id_pids = [] :: list(),
  result_callback :: fun(),
  neuron_supervisor :: pid()
}).