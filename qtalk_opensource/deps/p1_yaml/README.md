# P1 YAML

P1 YAML is an Erlang wrapper for [libyaml](http://pyyaml.org/wiki/LibYAML) "C" library.

## Installation

> $ ./configure
> $ make

## Example usage

```erlang
1> application:start(p1_yaml).
ok

2> p1_yaml:decode(<<"a: 1\nb: -3.0">>).
{ok,[[{<<"a">>,1},{<<"b">>,-3.0}]]}

3> p1_yaml:decode(<<"a: 1\nb: -3.0">>, [{plain_as_atom, true}]).
{ok,[[{a,1},{b,-3.0}]]}

4> p1_yaml:decode(<<"a: b\nc">>).  
{error,{scanner_error,<<"could not find expected ':'">>,2,
                      0}}.

5> p1_yaml:decode_from_file("test/test2.yml", [plain_as_atom]).
{ok,[[[{step,[{instrument,<<"Lasik 2000">>},
              {pulseEnergy,5.4},
              {pulseDuration,12},
              {repetition,1000},
              {spotSize,<<"1mm">>}]}],
      [{step,[{instrument,<<"Lasik 2000">>},
              {pulseEnergy,5.0},
              {pulseDuration,10},
              {repetition,500},
              {spotSize,<<"2mm">>}]}],
      [{step,<<"id001">>}],
      [{step,<<"id002">>}],
      [{step,<<"id001">>}],
      [{step,<<"id002">>}]]]}
```
