-module(util).

-export([mk_timestamp/0, mk_timestamp_ms/0, pmap/2, timeout/2]).

mk_timestamp() ->
    {MS, S, _} = erlang:now(),
    MS * 1000000 + S.

mk_timestamp_ms() ->
    {MS, S, SS} = erlang:now(),
    (MS * 1000000 + S) * 1000 + (SS div 1000).

%% http://yarivsblog.com/articles/2008/02/08/the-erlang-challenge/
pmap(Fun, List) ->
    Parent = self(),
    Pids = [spawn_link(fun() ->
                               Parent ! {self(), Fun(Elem)}
                       end)
            || Elem <- List],
    [receive
         {'EXIT', _From, Reason} -> exit(Reason);
         {Pid, Val} -> Val
     end
     || Pid <- Pids].


timeout(Fun, Timeout) ->
    I = self(),
    Ref = make_ref(),
    Pid = spawn_link(fun() ->
                             Result = Fun(),
                             I ! {Ref, Result}
                     end),
    receive
        {Ref, Result} ->
            Result
    after Timeout ->
            exit(Pid, timeout),
            exit(timeout)
    end.
