-module(server).
-import(crypto,[hash/2]).
-import(string,[equal/2]).
-import(lists,[append/2, member/2]).
-compile(export_all).

% It creates ETS tables to store data and spawns a main server process to make initial connection with the clients.
start() ->
	ets:new(userdatabase, [bag, named_table, public]),
	ets:new(tweetdatabase,[bag, named_table, public]),
	ets:new(subscribedatabase,[bag, named_table, public]),
	ets:new(clientSocketdatabase,[bag, named_table, public]),
	{ok, ListenSocket} = gen_tcp:listen(1204,[binary, {keepalive,true}, {reuseaddr, true}, {active, false}]),
	awate_connections(ListenSocket).

awate_connections(Listen) ->
		{ok, Socket} = gen_tcp:accept(Listen),
		spawn(server, awate_connections, [Listen]),
		io:format("client connected.\n"),
		listen_client(Socket).

listen_client(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Content} ->
			Data = re:split(Content, ","),
			Type = binary_to_list(lists:nth(1, Data)),
			if 
				Type == "signup" ->
					UserName = binary_to_list(lists:nth(2,Data)),
					Name = binary_to_list(lists:nth(3,Data)),
					Password = binary_to_list(lists:nth(4,Data)),
					Conpwd = binary_to_list(lists:nth(5,Data)),
					registeraccount(Socket,UserName,Name,Password,Conpwd);
				Type == "login" ->
					UserName = binary_to_list(lists:nth(2,Data)),
					Password = binary_to_list(lists:nth(3,Data)),
					loginaccount(Socket,UserName,Password);
				Type == "feeddisplay" ->
					UserName = binary_to_list(lists:nth(2,Data)),
					feeddisplay1(Socket,UserName);
				Type == "addtweet" ->
					UserName = binary_to_list(lists:nth(2,Data)),
					Tweet = binary_to_list(lists:nth(3,Data)),
					User = binary_to_list(lists:nth(4,Data)),
					addtweet(Socket,UserName,Tweet,User);
				Type == "searchTweets" ->
					UserName = binary_to_list(lists:nth(2,Data)),
					Word = binary_to_list(lists:nth(3,Data)),
					searchTweets(Socket,UserName,Word);
				Type == "getuserslist" ->
					UserName = binary_to_list(lists:nth(2,Data)),
					getUserList(Socket,UserName);
				Type == "subscribeto" ->
					UserName1 = binary_to_list(lists:nth(2,Data)),
					UserName2 = binary_to_list(lists:nth(4,Data)),
					subscribeto(Socket,UserName1,UserName2);
				true -> 
					true
			end
	end,
	listen_client(Socket).

% It checks if the username already exits and also checks if the confirmed password and password matches.
% If the above checks passed, the username and the SHA256 Hashed Password is stored in the userdatabase table.
% Replies back to the client with a message.
registeraccount(Socket,UserName,Name,Password,Conpwd) ->
	Status1 = string:equal(Password,Conpwd),
	Value = ets:lookup(userdatabase,UserName),
	Status2 = length(Value),
	if
		(Status1 == true andalso Status2 == 0) ->
			HashPwd=io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256,Password))]),
			ets:insert(userdatabase,{UserName,HashPwd}),
			ok = gen_tcp:send(Socket,"Registered\n");
		(Status1 == false) ->
			ok = gen_tcp:send(Socket,"Password didn't match\n");
		(Status2 > 0) ->
			ok = gen_tcp:send(Socket,"UserName already exists\n");
		true ->
			true
	end.

% It checks if the username exits and also checks if SHA256 Hashed Password matches with the user input.
% If the above checks passed, then the server spawns a process to further handle the user requests for this client.
% If logged in, it replies back with the server Socket through which the clients communicates further.
loginaccount(Socket,UserName,Password) ->
	Value = ets:lookup(userdatabase,UserName),
	[{_,Pass}] = ets:lookup(userdatabase,UserName),
	Status = length(Value),
	if
		(Status == 1) ->
			HashPwd=io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256,Password))]),
			if
				Pass == HashPwd ->
					ets:insert(clientSocketdatabase,{UserName,Socket}),
					ok = gen_tcp:send(Socket,"Login Successfull");
				true ->
					ok = gen_tcp:send(Socket,"Wrong Password")
			end;
		true ->
			ok = gen_tcp:send(Socket,"Invalid UserName")
	end.


% The below functions loops over the tweets of the user and also its subscribed users.
% Result message is sent to the client
feeddisplay1(Socket,UserName) ->
	UserList = ets:lookup(subscribedatabase,UserName),
	UserTweets = feeddisplay(UserName,1),
	Index = length(ets:lookup(tweetdatabase,UserName)) + 1,
	Tweets = loopUsers(UserList,Index,UserTweets),
	ok = gen_tcp:send(Socket,Tweets).

feeddisplay(UserName,Index) ->
	List = ets:lookup(tweetdatabase,UserName),
	FinalTweets = loopTweets(List,Index,""),
	FinalTweets.

loopTweets(List,Index,Final) when length(List) ==0->
	Final;

loopTweets(List,Index,Final) when length(List) > 0->
	[Head| Tail] = List,
	{UserName, Tweet, User} = Head,
	if
		length(User) == 0 ->
			NewFinal = Final ++ integer_to_list(Index) ++ ". UserName: " ++ UserName ++ " Tweeted: " ++ Tweet ++ "\n";
		true ->
			NewFinal = Final ++ integer_to_list(Index) ++ ". UserName: " ++ UserName ++ " ReTweeted User " ++ User ++ " Tweet: " ++ Tweet ++ "\n"
	end,
	loopTweets(Tail,Index + 1,NewFinal).

loopUsers(List,Index,Final) when length(List) == 0 ->
	Final;

loopUsers(List,Index,Final) when length(List) > 0->
	[Head | Tail] = List,
	{_,UserName} = Head,
	NewFinal = Final ++ feeddisplay(UserName,Index),
	loopUsers(Tail,Index + length(ets:lookup(tweetdatabase,UserName)),NewFinal).

% It adds the data to the tweet table.
addtweet(Socket,UserName,Tweet,User) ->
	ets:insert(tweetdatabase,{UserName,Tweet,User}),
	ok = gen_tcp:send(Socket,"Tweet Posted Successfull").

% It adds the client username and the username of subscribed to the subscribed table.
subscribeto(Socket,UserName1,UserName2) ->
	ets:insert(subscribedatabase,{UserName1,UserName2}),
	ok = gen_tcp:send(Socket,"Subscribed Successfull").
	
% The below functions loops over the tweetdatabase for each user to search for the word.
% Result message is sent to the client
searchTweets(Socket,UserName,Word) ->
	List = ets:tab2list(userdatabase),
	Tweets = loopUsers1(List,Word,""),
	ok = gen_tcp:send(Socket,Tweets).

loopUsers1(List,Word,Final) when length(List) == 0 ->
	Final;

loopUsers1(List,Word,Final) when length(List) > 0 ->
	[Head | Tail] = List,
	{UserName,_} = Head,
	NewFinal = Final ++ searchInUserTweet(UserName,Word),
	loopUsers1(Tail,Word,NewFinal).

searchInUserTweet(UserName,Word) ->
	TweetList = ets:lookup(tweetdatabase,UserName),
	Tweets = wordSearch(TweetList,UserName,Word,""),
	Tweets.

wordSearch(List,UserName,Word,Final) when length(List) == 0 ->
	Final;

wordSearch(List,UserName,Word,Final) when length(List) > 0 ->
	[Head | Tail] = List,
	{_,Tweet,User} = Head,
	Status = length(string:split(Tweet,Word)),
	if
		Status > 1 ->
			if
				length(User) == 0 ->
					NewFinal = Final ++ "UserName: " ++ UserName ++ " Tweet: " ++ Tweet ++ "\n";
				true ->
					NewFinal = Final ++ "UserName: " ++ UserName ++ " ReTweeted User " ++ User ++ " Tweet: " ++ Tweet ++ "\n"
			end;
		Status == 1 ->
			NewFinal = Final
	end,
	wordSearch(Tail,UserName,Word,NewFinal).

% The below functions loops over the userdatabase to get the list of users using twitter.
% Result message is sent to the client
getUserList(Socket,UserName) ->
	List = ets:tab2list(userdatabase),
	Users = getUsers(List,UserName,""),
	ok = gen_tcp:send(Socket,Users).

getUsers(List,UserName,Final) when length(List) == 0 ->
	Final;

getUsers(List,UserName,Final) when length(List) > 0 ->
	[Head | Tail] = List,
	{User,_} = Head,
	if
		UserName == User ->
			NewFinal = Final;
		true ->
			NewFinal = Final ++ "UserName: " ++ User ++ "\n"
	end,
	getUsers(Tail,UserName,NewFinal). 
