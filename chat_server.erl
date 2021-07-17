%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 7月 2021 21:06
%%%-------------------------------------------------------------------
-module(chat_server).
-author("Administrator").
-behaviour(gen_server).
%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(Port, 2345).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [2345], []).

init([Port]) ->
  %%创建aony和nkss两个房间
  %%创建login、register和room三个表记录user数据
  ets:new(room, [public, set, named_table]),
  ets:new(aony, [public, set, named_table]),
  ets:new(nkss, [public, set, named_table]),
  ets:new(register, [public, set, named_table]),
  ets:insert(room, [{nkss, 0}, {aony, 0}]),
  main_start(Port),
  {ok, ets:new(login, [public, set, named_table])}.

%%创建并行服务器
main_start(Port) ->
  {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 4}]),
  spawn(fun() -> main_connect(Listen) end).

main_connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  io:format("connect succeed  Socket =  ~p~n", [Socket]),
  spawn(fun() -> main_connect(Listen) end),
  main(Socket).

main(Socket) ->
  receive
    {tcp, Socket, Bin} ->
      Message = binary_to_term(Bin),
      case Message of
        {ID, Password, login} ->
          login_handle(ID, Password, Socket);
        {ID, Password, register} ->
          register_handle(ID, Password, Socket);
        {Msg, Roomname, msg} ->
          send_handle(Msg, Socket, Roomname);
        {Msg, Who, one} ->
          one_for_one_handle(Msg, Who, Socket);
        {Roomname, create} ->
          create_handle(Roomname, Socket);
        {Room_name, join} ->
          join_handle(Room_name, Socket);
        {Room_name, exit_room} ->
          exit_room_handle(Room_name, Socket);
        {Roomname, ID, closed} ->
          closed_handle(Roomname, ID, Socket)
      end
  end,
  main(Socket).

%%调用handle_call来处理客户端的需求
login_handle(ID, Password, Socket) ->
  case gen_server:call(?MODULE, {ID, Password, Socket, login}) of
    ok ->
      gen_tcp:send(Socket, term_to_binary(login_succeed));
    no ->
      gen_tcp:send(Socket, term_to_binary(no_false))
  end.

register_handle(ID, Password, Socket) ->
  case gen_server:call(?MODULE, {ID, Password, register}) of
    ok ->
      gen_tcp:send(Socket, term_to_binary({ok, register_succeed}));
    no ->
      gen_tcp:send(Socket, term_to_binary({no, exist}))
  end.

send_handle(Msg, Socket, Roomname) ->
  List = ets:tab2list(Roomname),
  AonyList = [X || {X, _Y} <- List],
  gen_server:call(?MODULE, {Msg, Socket, AonyList, Roomname, msg}).

one_for_one_handle(Msg, Who, Socket) ->
  gen_server:call(?MODULE, {Msg, Who, Socket, one_for_one}).

create_handle(Roomname, Socket) ->
  gen_server:call(?MODULE, {Roomname, Socket, create}).

join_handle(Room_name, Socket) ->
  case gen_server:call(?MODULE, {Room_name, Socket, join}) of
    {ok, NewNumber} ->
      gen_tcp:send(Socket, term_to_binary({join_succeed, Room_name, NewNumber}));
    no ->
      gen_tcp:send(Socket, term_to_binary(the_room_non_exitsent));
    {no, you_are_in_room} ->
      gen_tcp:send(Socket, term_to_binary({no, you_are_in_room}))
  end.

exit_room_handle(Room_name, Socket) ->
  gen_server:call(?MODULE, {Room_name, Socket, exit_room}).

closed_handle(Roomname, ID, Socket) ->
  gen_server:call(?MODULE, {Roomname, ID, Socket, closed}).


%%% callbacks
%%% 处理客户端的需求
%%% 登录
handle_call({ID, Password, Socket, login}, _From, Tab) ->
  Reply = case ets:lookup(register, ID) of
            [{ID, Password}] ->
              ets:insert(login, {ID, Socket}),
              ok;
            _ -> no
          end,
  {reply, Reply, Tab};

%%  注册
handle_call({ID, Password, register}, _From, Tab) ->
  Reply = case ets:lookup(register, ID) of
            [] ->
              ets:insert(register, {ID, Password}),
              ok;
            _ -> no
          end,
  {reply, Reply, Tab};

%%  创建房间
handle_call({Roomname, Socket, create}, _From, Tab) ->
  Reply = case ets:lookup(room, Roomname) of
            [{Roomname, _Number}] ->
              gen_tcp:send(Socket, term_to_binary({room_is_exist}));
            _ ->
              ets:new(Roomname, [public, set, named_table]),
              ets:insert(room, {Roomname, 0}),
              gen_tcp:send(Socket, term_to_binary({create_succeed}))
          end,
  {reply, Reply, Tab};

%%  加入房间
handle_call({Room_name, Socket, join}, _From, Tab) ->
  Reply = case ets:lookup(room, Room_name) of
            [{Room_name, RoomNumber}] ->
              case ets:lookup(Room_name, Socket) of
                [] ->
                  ets:insert(Room_name, {Socket, online}),
                  ets:insert(room, {Room_name, RoomNumber + 1}),
                  {ok, RoomNumber + 1};
                [{Socket, online}] ->
                  {no, you_are_in_room}
              end;
            [] ->
              no
          end,
  {reply, Reply, Tab};

%%  退出房间
handle_call({Room_name, Socket, exit_room}, _From, Tab) ->
  Reply = case ets:lookup(room, Room_name) of
            [{Room_name, RoomNumber}] ->
              case ets:lookup(Room_name, Socket) of
                [{Socket, online}] ->
                  ets:delete(Room_name, Socket),
                  ets:insert(room, {Room_name, RoomNumber - 1}),
                  gen_tcp:send(Socket, term_to_binary(exit_succeed));
                [] ->
                  gen_tcp:send(Socket, term_to_binary(join_room_first))
              end;
            [] ->
              gen_tcp:send(Socket, term_to_binary(no_this_room))
          end,
  {reply, Reply, Tab};

%%  退出账号
handle_call({Roomname, ID, Socket, closed}, _From, Tab) ->
  Reply = case ets:lookup(login, ID) of
            [{ID, Socket}] ->
              ets:delete(login, ID),
              [{Roomname, Number}] = ets:lookup(room, Roomname),
              ets:insert(room, {Roomname, Number - 1}),
              ets:delete(Roomname, Socket),
              gen_tcp:send(Socket, term_to_binary(succeedexit));
            _ ->
              gen_tcp:send(Socket, term_to_binary(noexit))
          end,
  {reply, Reply, Tab};

%%  房间公屏聊天
handle_call({Msg, Socket, AonyList, Roomname, msg}, _From, Tab) ->
  Reply = case ets:lookup(Roomname, Socket) of
            [{Socket, online}] ->
              lists:map(fun(E) -> gen_tcp:send(E, term_to_binary(Msg)) end, AonyList);
            [] ->
              gen_tcp:send(Socket, term_to_binary({no_in_room, Roomname}))
          end,
  {reply, Reply, Tab};

%%  私聊
handle_call({Msg, Who, Socket, one_for_one}, _From, Tab) ->
  Reply = case ets:lookup(login, Who) of
            [{Who, WhoSocket}] ->
              gen_tcp:send(WhoSocket, term_to_binary(Msg)),
              gen_tcp:send(Socket, term_to_binary(Msg));
            [] ->
              gen_tcp:send(Socket, term_to_binary(no_player_or_unonline))
          end,
  {reply, Reply, Tab}.


handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

