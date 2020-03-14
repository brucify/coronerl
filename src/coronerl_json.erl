-module(coronerl_json).

%% API
-export([ encode/1
        , encode/2
        , decode/1
        ]).

encode(Data) ->
  try
    jiffy:encode(Data)
  catch
    _Error:Reason ->
      {error, Reason}
  end.

encode(Data, Options) ->
  try
    jiffy:encode(Data, Options)
  catch
    _Error:Reason ->
      {error, Reason}
  end.

decode(Data) ->
  try
    {ok, jiffy:decode(Data, [return_maps])}
  catch
    _Error:Reason ->
      {error, Reason}
  end.