-module(addr).

-export([from_ip_port/2, to_ip_port/1, to_s/1]).

from_ip_port({A, B, C, D}, Port) ->
    <<A:8, B:8, C:8, D:8, Port:16/big>>.

to_ip_port(<<A, B, C, D, Port:16/big>>) ->
    {{A, B, C, D}, Port}.

to_s(<<A, B, C, D, Port:16/big>>) ->
    lists:flatten(io_lib:format("~B.~B.~B.~B:~B", [A, B, C, D, Port])).
