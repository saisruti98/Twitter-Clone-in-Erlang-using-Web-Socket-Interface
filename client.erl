-module(client).
-compile(export_all).
-import(server,[registeraccount/4,loginaccount/2,addtweet/2,subscribeto/2]).

% This func will connect to the server with serveraddress
start() ->
	PortNumber = 1204,
	IPAddress = "localhost",
	{ok, Socket} = gen_tcp:connect(IPAddress, PortNumber, [binary, {packet, 0}]),
	io:fwrite("Sent request to server.\n"),
	userinput(Socket).

% Takes user input for login or signup
userinput(Socket) ->
	io:format("Select an option: \n"),
	{ok, Option} = io:read("1. Signup \n2. Login \n"),
	case Option of
		1 -> 
			signup(Socket);
		2->
			login(Socket)
	end.

% Takes input from the user and sends the data to serverThread and receives message from server.
% If registered it goes back to userinput func for option to select either login or signup.
signup(Socket) ->
	{ok, Name} = io:read("Enter Name: "),
	{ok, UserName} = io:read("Enter UserName: "),
	{ok, Password} = io:read("Enter Password: "),
	{ok, Conpwd} = io:read("Confirm Password: "),
	ok = gen_tcp:send(Socket, ["signup",",",UserName,",",Name,",",Password,",",Conpwd]),
	receive
		{tcp,Socket,Msg}->
			io:format("~s",[Msg])
	end,
	userinput(Socket).

% Takes input from the user and sends the data to serverThread and receives message from server.
% If logged in, it gets the Server Pid from the server through which it communicates for further user inputs.
login(Socket) ->
	{ok, UserName} = io:read("Enter UserName: "),
	{ok, Password} = io:read("Enter Password: "),
	ok = gen_tcp:send(Socket, ["login",",",UserName,",",Password]),
	receive
		{tcp,Socket,Msg}->
			io:format("~s\n",[Msg]),
			if
				Msg == "Login Successfull" ->
					homepage(Socket,UserName);
				true ->
					homepage(Socket,UserName)
					% userinput(Socket)
			end
	end.

% It contains all the functionalities of the client and asks the user to select an option.
homepage(Socket,UserName) ->
	io:format("Select an option: \n"),
	{ok, Option} = io:read("1. My Feed \n2. Tweet \n3. Retweet \n4. Search Tweets \n5. Display Users \n6. Subscribe \n7. Logout \n"),
	
	case Option of
		1 -> 
			Tweets = feeddisplay(Socket,UserName,""),
			homepage(Socket,UserName);		
		2->
			sendtweet(Socket,UserName),
			homepage(Socket,UserName);
		3->
			retweet(Socket,UserName),
			homepage(Socket,UserName);
		4->
			searchtweets(Socket,UserName),
			homepage(Socket,UserName);
		5 ->
			getuserslist(Socket,UserName),
			homepage(Socket,UserName);
		6 ->
			subscribe(Socket,UserName),
			homepage(Socket,UserName);
		7->
			logout(Socket),
			userinput(Socket)
	end.

% It requests the Server Pid for its own tweets and also the tweets of its subscribers. Received Message from the server is displayed.
feeddisplay(Socket,UserName,Tweets) ->
	ok = gen_tcp:send(Socket, ["feeddisplay",",",UserName]),
	receive
		{tcp,Socket,Msg}->
			io:format("~s",[Msg]),
			FinalTweets = Tweets ++ Msg
	end,
	FinalTweets.

% It takes input from the client and sends the data to Server Pid.
sendtweet(Socket,UserName) ->
	{ok, Tweet} = io:read("Enter Tweet: "),
	ok = gen_tcp:send(Socket, ["addtweet",",",UserName,",",Tweet,",",""]),
	receive
		{tcp,Socket,Msg}->
			io:format("~s\n",[Msg])
	end.

% It displays the user feed and asks the user to select a Tweet to retweet. 
% The selected option is parsed for its UserName and TweetContent, which is sent to the Socket.
retweet(Socket,UserName) ->
	io:format("Select an option to Retweet: \n"),
	Tweets = feeddisplay(Socket,UserName,""),
	{ok,Option} = io:read("Enter an option: \n"),
	List1 = string:split(Tweets,integer_to_list(Option + 1)),
	[Head1 | Tail1] = List1,
	Searchstring = integer_to_list(Option) ++ ". UserName: ",
	List2 = string:split(Head1,Searchstring),
	[Head2 | Tail2] = List2,
	List3 = string:split(Tail2," Tweeted: "),
	[User | Tweetwithendline] = List3,
	List4 = string:split(Tweetwithendline,"\n"),
	[Tweet | Tail3] = List4,
	ok = gen_tcp:send(Socket, ["addtweet",",",UserName,",",Tweet,",",User]),
	receive
		{tcp,Socket,Msg} ->
			io:format("~s\n",[Msg])
	end.

% It provides user with 3 options mentioned below and takes input. 
% It requests the Server Pid for relevant tweets acc. to the input and received message is displayed.
searchtweets(Socket,UserName) ->
	io:format("Select an option: \n"),
	{ok, Option} = io:read("1. Search for HashTags \n2. My Mentions \n3. Search in Feed\n"),

	case Option of
		1 ->
			{ok, Tag} = io:read("Enter HashTag: "),
			HashTag = "#" ++ Tag,
			ok = gen_tcp:send(Socket, ["searchTweets",",",UserName,",",HashTag]);
		2 ->
			Mention = "@" ++ UserName,
			ok = gen_tcp:send(Socket, ["searchTweets",",",UserName,",",Mention]);
		3->
			{ok, Word} = io:read("Enter Word to Search: "),
			ok = gen_tcp:send(Socket, ["searchTweets",",",UserName,",",Word])
	end,
	receive
		{tcp,Socket,Msg} ->
			io:format("~s",[Msg])
	end.

% It requests Server Pid for all the UserName of the users using the Twitter.
getuserslist(Socket,UserName) ->
	ok = gen_tcp:send(Socket, ["getuserslist",",",UserName]),
	receive
		{tcp,Socket,Msg} ->
			io:format("~s",[Msg])
	end.

% It takes input from the client and sends the data to Server Pid.
subscribe(Socket,UserName) ->
	{ok, UserName2} = io:read("Enter UserName you want to subscribe: "),
	ok = gen_tcp:send(Socket, ["subscribeto",",",UserName, ",", UserName2]),
	receive
		{tcp,Socket,Msg} ->
			io:format("~s\n",[Msg])
	end.

% It sends the kill message to Server Pid which will kill the spawned process for handling this client and the connection is closed.
logout(Socket) ->
	io:format("Logout Successfull\n").

