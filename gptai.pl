:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).


get_key(Key) :-
    setup_call_cleanup(
        open(key, read, In),
        read_line_to_string(In, Key),
        close(In)).

query_gpt(_{query:Query}, Reply) :-
    get_key(Key),
    Url = 'https://api.openai.com/v1/chat/completions',
    Data = _{ model: 'gpt-4o-mini',
              temperature:0.7,
              messages:[
                  _{ role: user,
                     content: Query
                   }
              ]
            },
    http_post(Url, json(Data), Reply0,
              [ json(Data),
                authorization(bearer(Key)),
                status_code(Status),
                json_object(dict)
              ]),
    Reply = Reply0.put(status, Status).
