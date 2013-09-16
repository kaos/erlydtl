%%%-------------------------------------------------------------------
%%% File:      erlydtl_scanner.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @copyright 2013 Andreas Stenius
%%% @doc 
%%% Template language scanner
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
%%% Copyright (c) 2013 Andreas Stenius
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%% @since 2013-09-12 by Andreas Stenius
%%%-------------------------------------------------------------------
-module(erlydtl_scanner).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').
-author('Andreas Stenius <kaos@astekk.se>').

-export([scan/1, resume/1, next/2]).
-include("erlydtl_ext.hrl").

-compile({no_auto_import, [is_number/1]}).

-ifdef (TEST).
-export([scan_old/1, resume_old/1]).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%====================================================================
%% API
%%====================================================================

scan(Template) ->
    do_scan(
      #scanner_state{ 
         template=Template,
         scope=[],
         tags=tags()
        }).

resume(State) when is_record(State, scanner_state) ->
    do_scan(State).

%%====================================================================
%% Internal functions
%%====================================================================

-record(scope, {
          pos=1 :: integer(),
          until :: Pos::pos_integer(),
          tag :: #tag{},
          tags=[] :: {{Pos::pos_integer(), Prio::any()}, list(#tag{})}
         }).

do_scan(#scanner_state{ template=[] }=State) ->
    #scanner_state{ scanned=Scanned } = scan_token(reverse_string, State),
    {ok, lists:reverse(Scanned)};
do_scan(#scanner_state{ scope=[#scope{ pos=Pos, tags=[{{Pos, _}, Tag}|_] }|_] }=State) ->
    do_scan(open_tag(Tag, State));
do_scan(#scanner_state{ scope=[], tags=Tags }=State) ->
    do_scan(
      open_tag(
        find_open_tag(State, [T || T <- Tags, T#tag.valid_in == undefined]),
        State));
do_scan(#scanner_state{ scope=[#scope{ until=Pos, pos=Pos }|_] }=State) ->
    do_scan(close_tag(State));
do_scan(#scanner_state{ scope=[#scope{ tag=#tag{ on_scan=Scan } }|_] }=State) ->
    do_scan(scan_tag(Scan, State));
do_scan(Err) -> Err.


%%% State processing helpers

find_open_tag(#scanner_state{ template=Template }=State, Tags) ->
    case lists:dropwhile(
           fun(#tag{ open=Prefix }) when is_list(Prefix) ->
                   not lists:prefix(Prefix, Template);
              (#tag{ open=OpenTest }) when is_function(OpenTest, 2) ->
                   not OpenTest(test, State)
           end, Tags)
    of
        [Tag|_] -> Tag;
        [] -> undefined
    end.

open_tag(#tag{ open=Prefix, on_open=Open }=Tag, State) ->
    new_scope(Tag, next(Prefix, scan_token(Open, State))).

new_scope(#tag{ tag=In }=Tag, #scanner_state{ tags=Tags, scope=Scope }=State) ->
    State#scanner_state{
      scope=[update_scope(
               #scope{ tag=Tag,
                       tags=[{{0, 0}, T} || T <- Tags, T#tag.valid_in == In]
                     },
               State)
             |Scope]}.

update_scope(#scope{ pos=Pos, tag=#tag{ close=Close }, tags=Tags }=Scope, State) ->
    case find_tag(Close, {Pos - 1, undefined}, State) of
        Pos -> Scope#scope{ until=Pos };
        Until ->
            Scope#scope{
              until=Until,
              tags=lists:keysort(
                     1, [case Tag of
                             {{TagPos, _}, T}
                               when TagPos < Pos;
                                    TagPos == undefined ->
                                 scope_tag(T, Pos, Until, State);
                             _ -> Tag
                         end || Tag <- Tags])}
    end.

scope_tag(#tag{ open=Open, prio=Prio }=Tag, Pos, Until, State) ->
    {{find_tag(Open, {Pos - 1, Until}, State), Prio}, Tag}.

find_tag(undefined, _, _) -> undefined;
find_tag(Delim, {Offset, undefined}, #scanner_state{ template=Template }=State) ->
    tag_pos(Offset, find_tag(Delim, Template, State));
find_tag(Delim, {Offset, Until}, #scanner_state{ template=Template }=State) ->
    tag_pos(Offset,
            find_tag(Delim,
                     string:substr(Template, 1, Until - Offset),
                     State));
find_tag(Delim, String, _State) when is_list(Delim) ->
    string:str(String, Delim);
find_tag(Delim, String, State) when is_function(Delim, 2) ->
    Delim({find, String}, State).

tag_pos(Offset, Pos) when is_integer(Pos), Pos > 0 -> Offset + Pos;
tag_pos(_Offset, _Pos) -> undefined.

close_tag(#scanner_state{
                scope=[#scope{
                          tag=#tag{ 
                                 close=Prefix,
                                 on_close=Close } }|_] }=State) ->
    State1 = next(Prefix, scan_token(Close, State)),
    [#scope{ pos=Pos }, Scope|Scopes] = State1#scanner_state.scope,
    State1#scanner_state{ scope=[update_scope(move_scope(Pos - 1, Scope), State1)|Scopes] }.

scan_tag(undefined, #scanner_state{ template=[C|_] }=State) -> next(C, State);
scan_tag(OnScan, #scanner_state{ template=[C|_] }=State)
  when is_function(OnScan, 2) -> OnScan(C, State);
scan_tag(OnScan, State) when is_function(OnScan, 1) -> OnScan(State).


%%% Save/process scanned token
scan_token([E|Es], State) ->
    scan_token(Es, scan_token(E, State));
scan_token([], State) -> State;
scan_token(undefined, State) -> State;
scan_token(append_char, #scanner_state{ template=[C|_], scanned=[{Type, Pos, Chars}|Scanned] }=State) ->
    State#scanner_state{ scanned=[{Type, Pos, [C|Chars]}|Scanned] };
scan_token({append_char, C}, #scanner_state{ scanned=[{Type, Pos, Chars}|Scanned] }=State) ->
    State#scanner_state{ scanned=[{Type, Pos, [C|Chars]}|Scanned] };
scan_token(reverse_chars, #scanner_state{ scanned=[{Type, Pos, Chars}|Scanned] }=State) ->
    State#scanner_state{ scanned=[{Type, Pos, lists:reverse(Chars)}|Scanned] };
scan_token(reverse_string, #scanner_state{ scanned=[{string, Pos, Text}|Scanned] }=State) ->
    State#scanner_state{ scanned=[{string, Pos, lists:reverse(Text)}|Scanned] };
scan_token(reverse_string, State) -> State;
scan_token(to_atom, #scanner_state{ scanned=[{Type, Pos, Text}|Scanned] }=State) ->
    State#scanner_state{ scanned=[{Type, Pos, list_to_atom(Text)}|Scanned] };
scan_token({Type, no_value}, #scanner_state{ pos=Pos, scanned=Scanned }=State) ->
    State#scanner_state{ scanned=[{Type, Pos}|Scanned] };
scan_token({Type, Init}, #scanner_state{ pos=Pos, scanned=Scanned }=State) ->
    State#scanner_state{ scanned=[{Type, Pos, Init}|Scanned] };
scan_token(Fun, #scanner_state{ scanned=[Token|Scanned] }=State) when is_function(Fun, 2) ->
    State#scanner_state{ scanned=[Fun(Token, State)|Scanned] };
scan_token(Type, #scanner_state{ template=[C|_], pos=Pos, scanned=Scanned }=State) ->
    State#scanner_state{ scanned=[{Type, Pos, [C]}|Scanned] }.

%%% Move along past processed data in the template
next([C|Cs], State) -> next(Cs, next(C, State));
next([], State) -> State;
next(F, State) when is_function(F, 2) -> F(open, State);
next(undefined, State) -> State;
next(C, #scanner_state{ template=[C|Template], pos=Pos, scope=[Scope|Scopes] }=State) -> 
    State#scanner_state{
      template=Template,
      pos=update_pos(C, Pos),
      scope=[move_scope(1, Scope)|Scopes] };
next(Count, #scanner_state{ template=[C|_] }=State) when Count > 0 ->
    next(Count - 1, next(C, State));
next(0, State) -> State.

%%% Keep track of our position in the template
update_pos($\n, {Row, _Col}) -> {Row + 1, 1};
update_pos(_, {Row, Col}) -> {Row, Col + 1}.

move_scope(Count, #scope{ pos=Pos }=Scope) ->
    Scope#scope{ pos=Pos + Count }.

%%% Add more data to a scanned token
append_char(C, Type, #scanner_state{ scanned=[{Type, Pos, Chars}|Scanned] }=State) ->
    next(C, State#scanner_state{ scanned=[{Type, Pos, [C|Chars]}|Scanned] });
append_char(C, Type, #scanner_state{ pos=Pos, scanned=Scanned }=State) ->
    next(C, State#scanner_state{ scanned=[{Type, Pos, [C]}|Scanned] }).

append_next(Type, #scanner_state{ template=[C|_] }=State) -> append_char(C, Type, State).
append_text(Text, Type, State) ->
    lists:foldl(fun(C, S) -> append_char(C, Type, S) end, State, Text).


%%====================================================================
%% Tag callbacks and helpers
%%====================================================================

%% TEXT
scan_text(State) ->
    append_next(string, State).

%% CODE
scan_code(#scanner_state{ template=Template }=State) ->
    case scan_next_code(Template) of
        undefined ->
            {Row, Col} = State#scanner_state.pos,
            {error, {Row, ?MODULE, lists:concat(["Illegal character in column ", Col])}, State};
        {Cs, skip} -> next(Cs, State);
        Cs -> next(Cs, scan_token({list_to_atom(Cs), no_value}, State))
    end.

scan_next_code(Template) ->
    case lists:dropwhile(
           fun({Prefix, _}) -> not lists:prefix(Prefix, Template);
              (Prefix) -> not lists:prefix(Prefix, Template) 
           end, code_tokens())
    of
        [{_, skip}=Res|_] -> Res;
        [{_, Res}|_] -> Res;
        [Res|_] -> Res;
        [] -> undefined
    end.

code_tokens() ->
    ["==", "!=", ">=", "<=", "<", ">", "(", ")", ",", "|", "=", ":", ".", 
     {"_(", "_"},
     {" ", skip}].

%% STRING
scan_string(State) ->
    append_next(string_literal, State).

scan_escape(State) ->
    close_tag(append_next(string_literal, State)).

%% IDENTIFIER
scan_identifier(C, State) ->
    case is_alpha(C) orelse is_digit(C) of
        true -> append_char(C, identifier, State);
        false -> close_tag(State)
    end.

is_alpha(C) -> ((C >= $a) andalso (C =< $z)) orelse ((C >= $A) andalso (C =< $Z)) orelse (C == $_).
is_digit(C) -> (C >= $0) andalso (C =< $9).

is_identifier({find, String}, _State) -> find_identifier(String, 1);
is_identifier(open, State) -> next(1, State).

is_identifier([$_, $(|_]) -> false;
is_identifier([C|_]) -> is_alpha(C).

find_identifier([], _) -> 0;
find_identifier(String, Pos) ->
    case is_identifier(String) of
        true -> Pos;
        false -> find_identifier(tl(String), Pos + 1)
    end.

process_identifier({identifier, Pos, Name}, #scanner_state{ scanned=[_, PrevToken|_] }) ->
    case lists:keyfind(Name, 2, keywords(element(1, PrevToken))) of
        false -> {identifier, Pos, list_to_atom(Name)};
        {Keyword, _} -> {Keyword, Pos, Name}
    end.

%% NUMBER
scan_number(C, State) ->
    case is_digit(C) of
        true -> append_char(C, number_literal, State);
        false -> close_tag(State)
    end.

is_number({find, String}, _State) -> find_number(String, 1);
is_number(open, State) -> next(1, State).

is_number([C|_]) -> is_digit(C) orelse (C == $-).

find_number([], _) -> 0;
find_number(String, Pos) ->
    case is_number(String) of
        true -> Pos;
        false -> find_number(tl(String), Pos + 1)
    end.

%% VERBATIM
scan_verbatim(#scanner_state{ template=Template,
                              scanned=[{{verbatim, Tag}=Type, Pos, Text}
                                       |Scanned]}=State) ->
    case find_end_verbatim(Tag, Template) of
        {true, Cs} ->
            next(Cs, close_tag(
                       State#scanner_state{
                         scanned=[{string, Pos, Text}|drop_verbatim_tag(Scanned)]
                        }));
        {false, []} ->
            append_text(Template, Type, State);
        {false, Cs} ->
            append_text(Cs, Type, State)
    end.

is_verbatim({find, _}, #scanner_state{ scanned=Scanned }) ->
    case find_verbatim_tag(Scanned) of
        undefined -> 0; _ -> 1
    end;
is_verbatim(open, State) -> State.

on_verbatim({verbatim, Pos, Text}, #scanner_state{ scanned=[_|Scanned] }) ->
    {{verbatim, find_verbatim_tag(Scanned)}, Pos, Text}.

find_verbatim_tag(Scanned) -> element(1, split_verbatim_tag(Scanned)).
drop_verbatim_tag(Scanned) -> element(2, split_verbatim_tag(Scanned)).

split_verbatim_tag([{close_tag, _, _},
                    {identifier, _, verbatim},
                    {open_tag, _, _}|S]) ->
    {'$noname', S};
split_verbatim_tag([{close_tag, _, _},
                    {identifier, _, Tag},
                    {identifier, _, verbatim},
                    {open_tag, _, _}|S]) ->
    {Tag, S};
split_verbatim_tag(Scanned) -> {undefined, Scanned}.

find_end_verbatim('$noname', Template) ->
    find_end_verbatim(["{%", "endverbatim", "%}"], Template, []);
find_end_verbatim(Tag, Template) ->
    find_end_verbatim(["{%", "endverbatim", atom_to_list(Tag), "%}"], Template, []).

find_end_verbatim([], _, Acc) -> {true, lists:flatten(lists:reverse(Acc))};
find_end_verbatim(Tags, " " ++ Template, Acc) ->
    find_end_verbatim(Tags, Template, [" "|Acc]);
find_end_verbatim([Tag|Tags], Template, Acc) ->
    case lists:prefix(Tag, Template) of
        true -> find_end_verbatim(Tags, lists:nthtail(length(Tag), Template), [Tag|Acc]);
        false -> {false, lists:flatten(
                           lists:reverse(
                             [lists:sublist(
                                Template, string:str(tl(Template), "{%"))
                              |Acc]))}
    end.


%% KEYWORDS
keywords(open_tag) ->
    %% The rest must be preceded by an open_tag.
    %% This allows variables to have the same names as tags.
    [{autoescape_keyword, "autoescape"},
     {endautoescape_keyword, "endautoescape"},
     {block_keyword, "block"},
     {endblock_keyword, "endblock"},
     {comment_keyword, "comment"},
     {endcomment_keyword, "endcomment"},
     {cycle_keyword, "cycle"},
     {extends_keyword, "extends"},
     {filter_keyword, "filter"},
     {endfilter_keyword, "endfilter"},
     {firstof_keyword, "firstof"},
     {for_keyword, "for"},
     {empty_keyword, "empty"},
     {endfor_keyword, "endfor"},
     {if_keyword, "if"},
     {elif_keyword, "elif"},
     {else_keyword, "else"},
     {endif_keyword, "endif"},
     {ifchanged_keyword, "ifchanged"},
     {endifchanged_keyword, "endifchanged"},
     {ifequal_keyword, "ifequal"},
     {endifequal_keyword, "endifequal"},
     {ifnotequal_keyword, "ifnotequal"},
     {endifnotequal_keyword, "endifnotequal"},
     {include_keyword, "include"},
     {now_keyword, "now"},
     {regroup_keyword, "regroup"},
     {endregroup_keyword, "endregroup"},
     {spaceless_keyword, "spaceless"},
     {endspaceless_keyword, "endspaceless"},
     {ssi_keyword, "ssi"},
     {templatetag_keyword, "templatetag"},
     {widthratio_keyword, "widthratio"},
     {call_keyword, "call"},
     {endwith_keyword, "endwith"},
     {trans_keyword, "trans"},
     {blocktrans_keyword, "blocktrans"},
     {endblocktrans_keyword, "endblocktrans"}
     |keywords()];
keywords(_) -> keywords().
keywords() ->
    [{in_keyword, "in"},
     {not_keyword, "not"},
     {or_keyword, "or"},
     {and_keyword, "and"},
     {as_keyword, "as"},
     {by_keyword, "by"},
     {with_keyword, "with"},
     %% was: These must be succeeded by a close_tag
     %% but: that's currently not supported here..
     %% todo: figure out how to fix it
     {only_keyword, "only"},
     {parsed_keyword, "parsed"},
     {noop_keyword, "noop"},
     {reversed_keyword, "reversed"},
     {openblock_keyword, "openblock"},
     {closeblock_keyword, "closeblock"},
     {openvariable_keyword, "openvariable"},
     {closevariable_keyword, "closevariable"},
     {openbrace_keyword, "openbrace"},
     {closebrace_keyword, "closebrace"},
     {opencomment_keyword, "opencomment"},
     {closecomment_keyword, "closecomment"}
    ].


%%% Builtin top level block tags and classes..
tags() ->
    [#tag{ tag=text, open="", on_scan=fun scan_text/1 },
     #tag{ valid_in=text, tag=verbatim, prio=1,
           open=fun is_verbatim/2,
           on_open=[{verbatim, []}, fun on_verbatim/2],
           on_scan=fun scan_verbatim/1 },
     #tag{ valid_in=text, tag=comment, open="<!--{#", close="#}-->" },
     #tag{ valid_in=text, tag=comment, open="{#", close="#}" },
     #tag{ valid_in=text, tag=code,
           open="<!--{{", close="}}-->",
           on_open=[reverse_string, {open_var, '<!--{{'}],
           on_close={close_var, '}}-->'},
           on_scan=fun scan_code/1 },
     #tag{ valid_in=text, tag=code,
           open="{{", close="}}",
           on_open=[reverse_string, {open_var, '{{'}],
           on_close={close_var, '}}'},
           on_scan=fun scan_code/1 },
     #tag{ valid_in=text, tag=code,
           open="{%", close="%}",
           on_open=[reverse_string, {open_tag, '{%'}],
           on_close={close_tag, '%}'},
           on_scan=fun scan_code/1 },
     #tag{ valid_in=code, tag=identifier, open=fun is_identifier/2,
           on_open=identifier,
           on_close=[reverse_chars, fun process_identifier/2],
           on_scan=fun scan_identifier/2 },
     #tag{ valid_in=code, tag=number, open=fun is_number/2,
           on_open=number_literal, on_close=reverse_chars,
           on_scan=fun scan_number/2 },
     #tag{ valid_in=code, tag=string,
           open="\"", close="\"",
           on_open=string_literal,
           on_close=[append_char, reverse_chars],
           on_scan=fun scan_string/1 },
     #tag{ valid_in=code, tag=string,
           open="\'", close="\'",
           on_open={string_literal, "\""},
           on_close=[{append_char, $"}, reverse_chars],
           on_scan=fun scan_string/1 },
     #tag{ valid_in=string, tag=escape,
           open="\\", on_open=append_char,
           on_scan=fun scan_escape/1 }
    ].



-ifdef (TEST).

text_test() ->
    T = "foo bar",
    E = {ok, [{string, {1, 1}, "foo bar"}]},
    ?assertEqual(E, scan_old(T)),
    ?assertEqual(E, scan(T)).
    
identifier_test() ->
    T = "foo {{ bar }}",
    E = {ok, [{string, {1, 1}, "foo "},
              {open_var, {1, 5}, '{{'},
              {identifier, {1, 8}, bar},
              {close_var, {1, 12}, '}}'}]},
    ?assertEqual(E, scan_old(T)),
    ?assertEqual(E, scan(T)).

string_literal_test() ->
    T = "{{ \"test\" }}",
    E = {ok, [{open_var, {1, 1}, '{{'},
              {string_literal, {1, 4}, "\"test\""},
              {close_var, {1, 11}, '}}'}]},
    ?assertEqual(E, scan_old(T)),
    ?assertEqual(E, scan(T)).

number_literal_test() ->
    T = "{{ 12345 }}",
    E = {ok, [{open_var, {1, 1}, '{{'},
              {number_literal, {1, 4}, "12345"},
              {close_var, {1, 10}, '}}'}]},
    ?assertEqual(E, scan_old(T)),
    ?assertEqual(E, scan(T)).

attribute_test() ->    
    T = "{{ foo.bar }}",
    E = {ok, [{open_var, {1, 1}, '{{'},
              {identifier, {1, 4}, foo},
              {'.', {1, 7}},
              {identifier, {1, 8}, bar},
              {close_var, {1, 12}, '}}'}]},
    ?assertEqual(E, scan_old(T)),
    ?assertEqual(E, scan(T)).

keyword_test() ->    
    T = "{% if 1 %}",
    E = {ok, [{open_tag, {1, 1}, '{%'},
              {if_keyword, {1, 4}, "if"},
              {number_literal, {1, 7}, "1"},
              {close_tag, {1, 9}, '%}'}]},
    ?assertEqual(E, scan_old(T)),
    ?assertEqual(E, scan(T)).

invalid_character_test() ->    
    T = "{{ ~ }}",
    EMsg = {1, ?MODULE, "Illegal character in column 4"},
    ?assertMatch({error, EMsg, _}, scan_old(T)),
    ?assertMatch({error, EMsg, #scanner_state{}}, scan(T)).

escape_test() ->
    T = "{{ \"\\\" '\" }}",
    E = {ok, [{open_var, {1, 1}, '{{'},
              {string_literal, {1, 4}, "\"\\\" '\""},
              {close_var, {1, 11}, '}}'}]},
    ?assertEqual(E, scan_old(T)),
    ?assertEqual(E, scan(T)).

strings_test() ->    
    T = "{% cycle 'a' 'b' %}",
    E = {ok, [{open_tag, {1, 1}, '{%'},
              {cycle_keyword, {1, 4}, "cycle"},
              {string_literal, {1, 10}, "\"a\""},
              {string_literal, {1, 14}, "\"b\""},
              {close_tag, {1, 18}, '%}'}]},
    ?assertEqual(E, scan_old(T)),
    ?assertEqual(E, scan(T)).
    
verbatim_test() ->
    T = "foo{% verbatim %}bar{% endverbatim %}baz",
    E = {ok, [{string, {1, 1}, "foo"},
              {string, {1, 18}, "barbaz"}]},
    ?assertEqual(E, scan_old(T)),
    ?assertEqual(E, scan(T)).
    
verbatim2_test() ->
    T = "foo{% verbatim %}{% endverbatim %}bar",
    E = {ok, [{string, {1, 1}, "foo"},
              {string, {1, 18}, "bar"}]},
    ?assertEqual(E, scan_old(T)),
    ?assertEqual(E, scan(T)).
    

%%====================================================================
%% The previous scanner
%% Kept as reference during tests for a while..
%%====================================================================

-record(scanner_state_old, {
          template=[],
          scanned=[],
          pos={1,1},
          state=in_text
         }).

%%--------------------------------------------------------------------
%% @spec scan(T::template()) -> {ok, S::tokens()} | {error, Reason}
%% @type template() = string() | binary(). Template to parse
%% @type tokens() = [tuple()].
%% @doc Scan the template string T and return the a token list or
%% an error.
%% @end
%%--------------------------------------------------------------------
scan_old(Template) ->
    scan(Template, [], {1, 1}, in_text).    

resume_old(#scanner_state_old{ template=Template, scanned=Scanned, 
                               pos=Pos, state=State}) ->
    scan(Template, Scanned, Pos, State).

scan([], Scanned, _, in_text) ->
    Tokens = lists:reverse(Scanned),
    FixedTokens = reverse_strings(Tokens),
    MarkedTokens = mark_keywords(FixedTokens),
    AtomizedTokens = atomize_identifiers(MarkedTokens),
    {ok, AtomizedTokens};

scan([], _Scanned, _, {in_comment, _}) ->
    {error, "Reached end of file inside a comment."};

scan([], _Scanned, _, _) ->
    {error, "Reached end of file inside a code block."};

scan("<!--{{" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_var, {Row, Column}, '<!--{{'} | Scanned], {Row, Column + length("<!--{{")}, {in_code, "}}-->"});

scan("{{" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_var, {Row, Column}, '{{'} | Scanned], {Row, Column + length("{{")}, {in_code, "}}"});

scan("<!--{#" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, Scanned, {Row, Column + length("<!--{#")}, {in_comment, "#}-->"});

scan("{#" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, Scanned, {Row, Column + length("{#")}, {in_comment, "#}"});

scan("#}-->" ++ T, Scanned, {Row, Column}, {in_comment, "#}-->"}) ->
    scan(T, Scanned, {Row, Column + length("#}-->")}, in_text);

scan("#}" ++ T, Scanned, {Row, Column}, {in_comment, "#}"}) ->
    scan(T, Scanned, {Row, Column + length("#}")}, in_text);

scan("<!--{%" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_tag, {Row, Column}, '<!--{%'} | Scanned], 
         {Row, Column + length("<!--{%")}, {in_code, "%}-->"});

scan("{%" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, [{open_tag, {Row, Column}, '{%'} | Scanned], 
         {Row, Column + length("{%")}, {in_code, "%}"});

scan([_ | T], Scanned, {Row, Column}, {in_comment, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_comment, Closer});

scan("\n" ++ T, Scanned, {Row, Column}, in_text) ->
    scan(T, append_text_char(Scanned, {Row, Column}, $\n), {Row + 1, 1}, in_text);

scan([H | T], Scanned, {Row, Column}, in_text) ->
    scan(T, append_text_char(Scanned, {Row, Column}, H), {Row, Column + 1}, in_text);

scan("\"" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, "\""} | Scanned], {Row, Column + 1}, {in_double_quote, Closer});

scan("\"" ++ T, Scanned, {Row, Column}, {in_identifier, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, "\""} | Scanned], {Row, Column + 1}, {in_double_quote, Closer});

scan("\'" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, "\""} | Scanned], {Row, Column + 1}, {in_single_quote, Closer});

scan("\'" ++ T, Scanned, {Row, Column}, {in_identifier, Closer}) ->
    scan(T, [{string_literal, {Row, Column}, "\""} | Scanned], {Row, Column + 1}, {in_single_quote, Closer});

scan([$\\ | T], Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_char(Scanned, $\\), {Row, Column + 1}, {in_double_quote_slash, Closer});

scan([H | T], Scanned, {Row, Column}, {in_double_quote_slash, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_double_quote, Closer});

scan([$\\ | T], Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_char(Scanned, $\\), {Row, Column + 1}, {in_single_quote_slash, Closer});

scan([H | T], Scanned, {Row, Column}, {in_single_quote_slash, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_single_quote, Closer});

                                                % end quote
scan("\"" ++ T, Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_char(Scanned, 34), {Row, Column + 1}, {in_code, Closer});

                                                % treat single quotes the same as double quotes
scan("\'" ++ T, Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_char(Scanned, 34), {Row, Column + 1}, {in_code, Closer});

scan([H | T], Scanned, {Row, Column}, {in_double_quote, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_double_quote, Closer});

scan([H | T], Scanned, {Row, Column}, {in_single_quote, Closer}) ->
    scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_single_quote, Closer});


scan("}}-->" ++ T, Scanned, {Row, Column}, {_, "}}-->"}) ->
    scan(T, [{close_var, {Row, Column}, '}}-->'} | Scanned], 
         {Row, Column + length("}}-->")}, in_text);

scan("}}" ++ T, Scanned, {Row, Column}, {_, "}}"}) ->
    scan(T, [{close_var, {Row, Column}, '}}'} | Scanned], {Row, Column + 2}, in_text);

scan("%}-->" ++ T, Scanned, {Row, Column}, {_, "%}-->"}) ->
    scan(T, [{close_tag, {Row, Column}, '%}-->'} | Scanned], 
         {Row, Column + length("%}-->")}, in_text);

scan("%}" ++ T, [{identifier, _, "mitabrev"}, {open_tag, _, '{%'}|Scanned], {Row, Column}, {_, "%}"}) ->
    scan(T, [{string, {Row, Column + 2}, ""}|Scanned], {Row, Column + 2}, {in_verbatim, undefined});

scan("%}" ++ T, [{identifier, _, ReversedTag}, {identifier, _, "mitabrev"}, {open_tag, _, '{%'}|Scanned], 
     {Row, Column}, {_, "%}"}) ->
    scan(T, [{string, {Row, Column + 2}, ""}|Scanned], {Row, Column + 2}, {in_verbatim, ReversedTag});

scan("%}" ++ T, Scanned, {Row, Column}, {_, "%}"}) ->
    scan(T, [{close_tag, {Row, Column}, '%}'} | Scanned], 
         {Row, Column + 2}, in_text);

scan("{%" ++ T, Scanned, {Row, Column}, {in_verbatim, Tag}) ->
    scan(T, Scanned, {Row, Column + 2}, {in_verbatim_code, lists:reverse("{%"), Tag});

scan(" " ++ T, Scanned, {Row, Column}, {in_verbatim_code, BackTrack, Tag}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_verbatim_code, [$\ |BackTrack], Tag});

scan("endverbatim%}" ++ T, Scanned, {Row, Column}, {in_verbatim_code, _BackTrack, undefined}) ->
    scan(T, Scanned, {Row, Column + length("endverbatim%}")}, in_text);

scan("endverbatim " ++ T, Scanned, {Row, Column}, {in_verbatim_code, BackTrack, Tag}) ->
    scan(T, Scanned, {Row, Column + length("endverbatim ")}, 
         {in_endverbatim_code, "", lists:reverse("endverbatim ", BackTrack), Tag});

scan(" " ++ T, Scanned, {Row, Column}, {in_endverbatim_code, "", BackTrack, Tag}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_endverbatim_code, "", [$\ |BackTrack], Tag});

scan([H|T], Scanned, {Row, Column}, {in_endverbatim_code, EndTag, BackTrack, Tag}) when H >= $a, H =< $z; H >= $0, H =< $9; H =:= $_  ->
    scan(T, Scanned, {Row, Column + 1}, {in_endverbatim_code, [H|EndTag], [H|BackTrack], Tag});

scan(" " ++ T, Scanned, {Row, Column}, {in_endverbatim_code, Tag, BackTrack, Tag}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_endverbatim_code, Tag, [$\ |BackTrack], Tag});

scan("%}" ++ T, Scanned, {Row, Column}, {in_endverbatim_code, Tag, _BackTrack, Tag}) ->
    scan(T, Scanned, {Row, Column + 2}, in_text);

scan("%}" ++ T, Scanned, {Row, Column}, {in_endverbatim_code, "", _BackTrack, undefined}) ->
    scan(T, Scanned, {Row, Column + 2}, in_text);

scan([H|T], [{string, Pos, Data}|Scanned], {Row, Column}, {in_endverbatim_code, _, BackTrack, Tag}) ->
    NewPos = case H of $\n -> {Row + 1, 1}; _ -> {Row, Column + 1} end,
    scan(T, [{string, Pos, [H|BackTrack] ++ Data}|Scanned], NewPos, {in_verbatim, Tag});

scan([H|T], [{string, Pos, Data}|Scanned], {Row, Column}, {in_verbatim_code, BackTrack, Tag}) ->
    NewPos = case H of $\n -> {Row + 1, 1}; _ -> {Row, Column + 1} end,
    scan(T, [{string, Pos, [H|BackTrack] ++ Data}|Scanned], NewPos, {in_verbatim, Tag});

scan([H|T], [{string, Pos, Data}|Scanned], {Row, Column}, {in_verbatim, Tag}) ->
    NewPos = case H of $\n -> {Row + 1, 1}; _ -> {Row, Column + 1} end,
    scan(T, [{string, Pos, [H|Data]}|Scanned], NewPos, {in_verbatim, Tag});

scan("==" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'==', {Row, Column}} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan("!=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'!=', {Row, Column}} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan(">=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'>=', {Row, Column}} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan("<=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'<=', {Row, Column}} | Scanned], {Row, Column + 2}, {in_code, Closer});

scan("<" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'<', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan(">" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'>', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("("++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'(', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan(")" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{')', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("," ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{',', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("|" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'|', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("=" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'=', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan(":" ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{':', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("." ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, [{'.', {Row, Column}} | Scanned], {Row, Column + 1}, {in_code, Closer});

scan("_(" ++ T, Scanned, {Row, Column}, {in_code, Closer}) ->
    scan(T, lists:reverse([{'_', {Row, Column}}, {'(', {Row, Column + 1}}], Scanned), {Row, Column + 2}, {in_code, Closer});

scan(" " ++ T, Scanned, {Row, Column}, {_, Closer}) ->
    scan(T, Scanned, {Row, Column + 1}, {in_code, Closer});


scan([H | T], Scanned, {Row, Column}, {in_code, Closer}) ->
    case char_type(H) of
        letter_underscore ->
            scan(T, [{identifier, {Row, Column}, [H]} | Scanned], {Row, Column + 1}, {in_identifier, Closer});
        hyphen_minus ->
            scan(T, [{number_literal, {Row, Column}, [H]} | Scanned], {Row, Column + 1}, {in_number, Closer});
        digit ->
            scan(T, [{number_literal, {Row, Column}, [H]} | Scanned], {Row, Column + 1}, {in_number, Closer});
        _ ->
            {error, {Row, ?MODULE, lists:concat(["Illegal character in column ", Column])},
             #scanner_state_old{ template=[H|T], scanned=Scanned, pos={Row, Column}, state={in_code, Closer}}}
    end;

scan([H | T], Scanned, {Row, Column}, {in_number, Closer}) ->
    case char_type(H) of
        digit ->
            scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_number, Closer});
        _ ->
            {error, {Row, ?MODULE, lists:concat(["Illegal character in column ", Column])},
             #scanner_state_old{ template=[H|T], scanned=Scanned, pos={Row, Column}, state={in_code, Closer}}}
    end;

scan([H | T], Scanned, {Row, Column}, {in_identifier, Closer}) ->
    case char_type(H) of
        letter_underscore ->
            scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_identifier, Closer});
        digit ->
            scan(T, append_char(Scanned, H), {Row, Column + 1}, {in_identifier, Closer});
        _ ->
            {error, {Row, ?MODULE, lists:concat(["Illegal character in column ", Column])},
             #scanner_state_old{ template=[H|T], scanned=Scanned, pos={Row, Column}, state={in_code, Closer}}}
    end.

%%% internal functions

append_char([{Type, Pos, Chars}|Scanned], Char) ->
    [{Type, Pos, [Char | Chars]} | Scanned].

append_text_char([], {Row, Column}, Char) ->
    [{string, {Row, Column}, [Char]}];
append_text_char([{string, StrPos, Chars} |Scanned1], _, Char) ->
    [{string, StrPos, [Char | Chars]} | Scanned1];
append_text_char(Scanned, {Row, Column}, Char) ->
    [{string, {Row, Column}, [Char]} | Scanned].

char_type(C) when ((C >= $a) andalso (C =< $z)) orelse ((C >= $A) andalso (C =< $Z)) orelse (C == $_) ->
    letter_underscore;
char_type(C) when ((C >= $0) andalso (C =< $9)) ->
    digit;
char_type($-) ->
    hyphen_minus;
char_type(_C) ->
    undefined.

reverse_strings(Tokens) ->
    reverse_strings(Tokens, []).

reverse_strings([], Acc) ->
    lists:reverse(Acc);
reverse_strings([{Category, Pos, String}|T], Acc) when Category =:= string; Category =:= identifier;
                                                       Category =:= string_literal; Category =:= number_literal ->
    reverse_strings(T, [{Category, Pos, lists:reverse(String)}|Acc]);
reverse_strings([Other|T], Acc) ->
    reverse_strings(T, [Other|Acc]).

mark_keywords(Tokens) ->
    mark_keywords(Tokens, []).

mark_keywords([], Acc) ->
    lists:reverse(Acc);
mark_keywords([{identifier, Pos, "in" = String}|T], Acc) ->
    mark_keywords(T, [{in_keyword, Pos, String}|Acc]);
mark_keywords([{identifier, Pos, "not" = String}|T], Acc) ->
    mark_keywords(T, [{not_keyword, Pos, String}|Acc]);
mark_keywords([{identifier, Pos, "or" = String}|T], Acc) ->
    mark_keywords(T, [{or_keyword, Pos, String}|Acc]);
mark_keywords([{identifier, Pos, "and" = String}|T], Acc) ->
    mark_keywords(T, [{and_keyword, Pos, String}|Acc]);
mark_keywords([{identifier, Pos, "as" = String}|T], Acc) ->
    mark_keywords(T, [{as_keyword, Pos, String}|Acc]);
mark_keywords([{identifier, Pos, "by" = String}|T], Acc) ->
    mark_keywords(T, [{by_keyword, Pos, String}|Acc]);
mark_keywords([{identifier, Pos, "with" = String}|T], Acc) ->
    mark_keywords(T, [{with_keyword, Pos, String}|Acc]);
%% These must be succeeded by a close_tag
mark_keywords([{identifier, Pos, "only" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{only_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "parsed" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{parsed_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "noop" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{noop_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "reversed" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{reversed_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "openblock" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{openblock_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "closeblock" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{closeblock_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "openvariable" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{openvariable_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "closevariable" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{closevariable_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "openbrace" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{openbrace_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "closebrace" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{closebrace_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "opencomment" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{opencomment_keyword, Pos, String}, CloseTag], Acc));
mark_keywords([{identifier, Pos, "closecomment" = String}, {close_tag, _, _} = CloseTag|T], Acc) ->
    mark_keywords(T, lists:reverse([{closecomment_keyword, Pos, String}, CloseTag], Acc));
%% The rest must be preceded by an open_tag.
%% This allows variables to have the same names as tags.
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "autoescape" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {autoescape_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endautoescape" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endautoescape_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "block" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {block_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endblock" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endblock_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "comment" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {comment_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endcomment" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endcomment_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "cycle" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {cycle_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "extends" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {extends_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "filter" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {filter_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endfilter" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endfilter_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "firstof" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {firstof_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "for" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {for_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "empty" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {empty_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endfor" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endfor_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "if" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {if_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "elif" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {elif_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "else" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {else_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endif" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endif_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "ifchanged" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {ifchanged_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endifchanged" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endifchanged_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "ifequal" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {ifequal_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endifequal" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endifequal_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "ifnotequal" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {ifnotequal_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endifnotequal" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endifnotequal_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "include" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {include_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "now" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {now_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "regroup" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {regroup_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endregroup" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endregroup_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "spaceless" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {spaceless_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endspaceless" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endspaceless_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "ssi" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {ssi_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "templatetag" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {templatetag_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "widthratio" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {widthratio_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "call" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {call_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endwith" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endwith_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "trans" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {trans_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "blocktrans" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {blocktrans_keyword, Pos, String}], Acc));
mark_keywords([{open_tag, _, _} = OpenToken, {identifier, Pos, "endblocktrans" = String}|T], Acc) ->
    mark_keywords(T, lists:reverse([OpenToken, {endblocktrans_keyword, Pos, String}], Acc));
mark_keywords([H|T], Acc) ->
    mark_keywords(T, [H|Acc]).

atomize_identifiers(Tokens) ->
    atomize_identifiers(Tokens, []).

atomize_identifiers([], Acc) ->
    lists:reverse(Acc);
atomize_identifiers([{identifier, Pos, String}|T], Acc) ->
    atomize_identifiers(T, [{identifier, Pos, list_to_atom(String)}|Acc]);
atomize_identifiers([H|T], Acc) ->
    atomize_identifiers(T, [H|Acc]).

-endif. %% TEST
