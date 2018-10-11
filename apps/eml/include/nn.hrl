-record(inp_item, {nid :: integer(), weight :: float()}).
-record(out_item, {nid :: integer(), pid :: pid()}).
-record(state, {nid :: integer(), component_type :: neuron | sensor | actuator, cortes_pid :: pid(), input ::list(#inp_item{}), output :: list(#out_item{}), bias :: float(), accum :: float(), signals :: list()}).
-record(inp_config, {type :: sensor | neuron | actuator, nid :: integer(), input :: list(#inp_item{}), bias :: float()}).
-record(out_config, {nid :: integer(), output :: list(#out_item{}), pid}).