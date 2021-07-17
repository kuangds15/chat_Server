%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 7月 2021 17:21
%%%-------------------------------------------------------------------
-module(chat_client).
-author("Administrator").
%% API
-export([start/0, login/2, reg/2,showuser/0, showroom/0,create_room/1,exit_room/0,join_room/1, send_msg/1, send_for/2, exit/0]).
-define(Port, 2345).

%%功能：登录/注册，创建/加入/退出房间，房间内公屏聊天,私聊，显示用户名/房间名/房间人数，退出账号

%%建立TCP连接
start() ->
  register(client, spawn(fun() -> {ok, Socket} = gen_tcp:connect("localhost", ?Port, [binary, {packet, 4}]),
    loop(Socket) end)),
  ets:new(room, [public, set, named_table]),
  ets:new(user, [public, set, named_table]),
  {ok,please_login_or_register}.

%%%API

%%登录
login(ID, Password) ->
  client ! {ID, Password, login}.

%%注册
reg(ID, Password) ->
  client ! {ID, Password, register}.

%%显示房间
showroom()->
  [{Roomname,Roomnumber}] = ets:tab2list(room),
  io:format("Room is  ~p~n",[Roomname]),
  io:format("Numbers of room is ~p~n",[Roomnumber]).

%%显示用户名
showuser()->
  [{ID,Socket}] = ets:tab2list(user),
  io:format("ID is：~p~n",[ID]),
  io:format("Socket is：~p~n",[Socket]).

%%创建房间
create_room(Roomname) ->
  case ets:first(user) of
    '$end_of_table' ->
      io:format("you must login first");
    _ ->
      client ! {Roomname, create}
  end.

%%加入房间
join_room(Room_name) ->
  case ets:first(user) of
    '$end_of_table' ->
      io:format("you must login first");
    _ ->
      case ets:first(room) of
        '$end_of_table' ->
          client ! {Room_name, join};
        _ ->
          io:format("you are in another room")
      end
  end.

%%退出房间
exit_room() ->
  case ets:first(room) of
    '$end_of_table' ->
      io:format("you must join room first");
    _ ->
      Room_name = ets:first(room),
      client ! {Room_name, exit_room}
  end.

%%房间内公屏发送消息
send_msg(Msg) ->
  case ets:first(user) of
    '$end_of_table' ->
      io:format("you must login first");
    _ ->
      case ets:first(room) of
        '$end_of_table' ->
          io:format("you must join room first");
        _ ->
          client ! {Msg, msg}
      end
  end.

%%私聊
send_for(Msg, Who) ->
  case ets:first(user) of
    '$end_of_table' ->
      io:format("you must login first");
    _ ->
      client ! {Msg, Who, one}
  end.

%退出账号
exit() ->
  case ets:first(user) of
    '$end_of_table' ->
      io:format("you must login first");
    _ ->
      Room_name = ets:first(room),
      ID = ets:first(user),
      client ! {Room_name, ID, closed}
  end.

loop(Socket) ->
  receive
    {ID, Password, login} ->
      gen_tcp:send(Socket, term_to_binary({ID, Password, login})),
      receive
        {tcp, _Socket, Bin} ->
          CC = binary_to_term(Bin),
          case CC of
            login_succeed ->
              ets:insert(user, {ID, Socket}),
              io:format("Receive : ~p~n", [CC]);
            _ ->
              io:format("Receive : ~p~n", [CC])
          end
      end;
    {ID, Password, register} ->
      gen_tcp:send(Socket, term_to_binary({ID, Password, register})),
      receive_loop();
    {Msg, Roomname, msg} ->
      gen_tcp:send(Socket, term_to_binary({Msg, Roomname, msg})),
      receive_loop();
    {Msg, Who, one} ->
      gen_tcp:send(Socket, term_to_binary({Msg, Who, one})),
      receive_loop();
    {Roomname, create} ->
      gen_tcp:send(Socket, term_to_binary({Roomname, create})),
      receive_loop();
    {Room_name, join} ->
      gen_tcp:send(Socket, term_to_binary({Room_name, join})),
      receive
        {tcp, _Socket, Bin} ->
          CC = binary_to_term(Bin),
          case CC of
            {join_succeed, Room_name, NewNumber} ->
              ets:insert(room, {Room_name, NewNumber}),
              io:format("Receive : ~p~n", [CC]);
            _ ->
              io:format("Receive : ~p~n", [CC])
          end
      end;
    {Room_name, exit_room} ->
      gen_tcp:send(Socket, term_to_binary({Room_name, exit_room})),
      receive
        {tcp, _Socket, Bin} ->
          CC = binary_to_term(Bin),
          case CC of
            exit_succeed ->
              ets:delete(room, Room_name),
              io:format("Receive : ~p~n", [CC]);
            _ ->
              io:format("Receive : ~p~n", [CC])
          end
      end;
    {Room_name, ID, closed} ->
      gen_tcp:send(Socket, term_to_binary({Room_name, ID, closed})),
      receive_loop();
    {tcp, Socket, Bin} ->
      AA = binary_to_term(Bin),
      io:format("Receive : ~p~n", [AA]);
    {tcp_closed, Socket} ->
      io:format("closed now")
  end,
  loop(Socket).

%%接收函数
receive_loop() ->
  receive
    {tcp, _Socket, Bin} ->
      CC = binary_to_term(Bin),
      io:format("Receive : ~p~n", [CC])
  end.

