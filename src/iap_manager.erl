-module (iap_manager).
-export ([verify_iap/2]).

code_field()->
	<<"code">>.

message_field()->
	<<"message">>.

status_field()->
	<<"status">>.

% "https://sandbox.itunes.apple.com/verifyReceipt" - sandbox url 
% "https://buy.itunes.apple.com/verifyReceipt" - real url

get_url(Dest)->
	case Dest of
			production ->
				"https://buy.itunes.apple.com/verifyReceipt";
			sandbox ->
				"https://sandbox.itunes.apple.com/verifyReceipt"
	end.

verify_iap(Data, Dest)->
	%extract Receipt data
	DataList = binary_to_list(Data),
	ReceiptData = lists:sublist(DataList, 13, length(DataList)),
	DecodedReceiptData = "{\"receipt-data\" : \"" ++ ReceiptData ++ "\"}",
	inets:start(temporary),
	ssl:start(temporary),
	{ok, {{Version, ResponseCode, ReasonPhrase}, Headers, Body}} = httpc:request(post, {get_url(Dest), [], "application/json; charset=utf8", DecodedReceiptData}, [], []),
	{struct, JsonPropList} = mochijson2:decode(Body),
	Status = proplists:get_value(<<"status">>, JsonPropList),
	define_status(Status).

make_response(Code, Message)->
	log_error(Code, Message),
	[{status_field() , [{code_field(), Code}, {message_field(), Message}]}].

log_error(Code, Message)->
	io:format("~n Error (~p): ~p~n", [binary_to_list(Code), binary_to_list(Message)]).

define_status(Status)->
	case Status of
		21000 ->
			Message = <<"The App Store could not read the JSON object you provided.">>,
			Code = <<"21000">>,
			make_response(Code, Message);
		21002 ->
			Message = <<"The data in the receipt-data property was malformed.">>,
			Code = <<"21002">>,
			make_response(Code, Message);
		21003 ->
			Message = <<"The receipt could not be authenticated.">>,
			Code = <<"21003">>,
			make_response(Code, Message);
		21004 ->
			Message = <<"The shared secret you provided does not match the shared secret on file for your account.">>,
			Code = <<"21004">>,
			make_response(Code, Message);
		21005 ->
			Message = <<"The receipt server is not currently available.">>,
			Code = <<"21005">>,
			make_response(Code, Message);			
		21006 ->
			Message = <<"This receipt is valid but the subscription has expired. When this status code is returned to your server, the receipt data is also decoded and returned as part of the response.">>,
			Code = <<"21006">>,
			make_response(Code, Message);	
		21007 ->
			Message = <<"This receipt is a sandbox receipt, but it was sent to the production service for verification.">>,
			Code = <<"21007">>,
			make_response(Code, Message);
		21008 ->
			Message = <<"This receipt is a production receipt, but it was sent to the sandbox service for verification.">>,
			Code = <<"21008">>,
			make_response(Code, Message);
		0 -> 
			Message = <<"This receipt is real.">>,
			Code = <<"0">>,
			make_response(Code, Message);
		_ ->
			Message = <<"Unrecognized error.">>,
			Code = <<"-1">>,
			make_response(Code, Message)
	end.
