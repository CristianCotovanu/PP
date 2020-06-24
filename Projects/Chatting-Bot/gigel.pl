:- ensure_loaded('chat.pl').

% Returneaza true dacă regula dată ca argument se potriveste cu
% replica data de utilizator. Replica utilizatorului este
% reprezentata ca o lista de tokens. Are nevoie de
% memoria replicilor utilizatorului pentru a deduce emoția/tag-ul
% conversației.
% match_rule/3
% match_rule(_Tokens, _UserMemory, rule(_, _, _, _, _)) :- fail.
match_rule(Tokens, UserMemory, rule(Expression, _, _, TmpEmotion, TmpTag)) :- Expression = Tokens,
                                    get_emotion(UserMemory, Emotion),
                                    (Emotion = neutru -> true ; [Emotion] = TmpEmotion),
                                    get_tag(UserMemory, Tag),
                                    (Tag = none -> true ; [Tag] = TmpTag).

% Primeste replica utilizatorului (ca lista de tokens) si o lista de
% reguli, iar folosind match_rule le filtrează doar pe cele care se
% potrivesc cu replica dată de utilizator.
% find_matching_rules/4
% find_matching_rules(+Tokens, +Rules, +UserMemory, -MatchingRules)
find_matching_rules(Tokens, Rules, UserMemory, MatchingRules) :- 
    findall(SingleRule,
            (member(SingleRule, Rules), 
            match_rule(Tokens, UserMemory, SingleRule)),
            MatchingRules).

% Intoarce in Answer replica lui Gigel. Selecteaza un set de reguli
% (folosind predicatul rules) pentru care cuvintele cheie se afla in
% replica utilizatorului, in ordine; pe setul de reguli foloseste
% find_matching_rules pentru a obtine un set de raspunsuri posibile.
% Dintre acestea selecteaza pe cea mai putin folosita in conversatie.
%
% Replica utilizatorului este primita in Tokens ca lista de tokens.
% Replica lui Gigel va fi intoarsa tot ca lista de tokens.
%
% UserMemory este memoria cu replicile utilizatorului, folosita pentru
% detectarea emotiei / tag-ului.
% BotMemory este memoria cu replicile lui Gigel și va si folosită pentru
% numararea numarului de utilizari ale unei replici.
%
% In Actions se vor intoarce actiunile de realizat de catre Gigel in
% urma replicii (e.g. exit).
%
% Hint: min_score, ord_subset, find_matching_rules

flatten_one_level([], []).
flatten_one_level([Head | Rest], L) :- is_list(Head), flatten_one_level(Rest, RestV2), !, append(Head, RestV2, L).
flatten_one_level([Head | Rest], [Head | RestV2]) :- flatten_one_level(Rest, RestV2).

delete_last_element([_], []).
delete_last_element([Head, Second | Rest], [Head | RestV2]):- delete_last_element([Second | Rest], RestV2).

% select_answer/5
% select_answer(+Tokens, +UserMemory, +BotMemory, -Answer, -Actions)
select_answer(Tokens, UserMemory, BotMemory, Answer, Actions) :-
    findall(BagRule, (rules(Reply, BagRule), ord_subset(Reply, Tokens)), RulesFound),   % Ia toate regulile care fac match cu replica noastra
    flatten(RulesFound, MatchingRules),                                                 % lista liste cuvinte => lista de cuvinte
    find_matching_rules(Tokens, MatchingRules, UserMemory, RulesToUse),                   % Ia toate regulile care fac match cu replica noastra
    findall(RuleReplies,
        member(rule(Tokens, RuleReplies, _, _, _), RulesToUse),
        RuleMatrix),
    flatten_one_level(RuleMatrix, TempTemp),
    length(TempTemp, Len),
    (Len =\= 1, 
        delete_last_element(TempTemp, InputKeys); 
        InputKeys = TempTemp),
    findall((TempKey, TempVal), 
        (member(WordsKey, InputKeys), 
        unwords(WordsKey, TempKey),
        get_value(BotMemory, TempKey, TempVal)),
        MapList),
    min_element(MapList, MinReply),
    words(MinReply, Answer),
    (member(Tokens, [[pa], [pa, pa], [bye], [la, revedere]]),
    Actions = [exit] ; 
    Actions = []).

% Esuează doar daca valoarea exit se afla in lista Actions.
% Altfel, returnează true.
% handle_actions/1
% handle_actions(+Actions)
handle_actions(Actions) :- \+member(exit, Actions).

% Atribuie un scor pentru fericire (de cate ori au fost folosit cuvinte din predicatul happy(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie fericit.
% get_happy_score/2
% get_happy_score(+UserMemory, -Score)

sum_list_elements([Elem], Elem).
sum_list_elements([Elem1, Elem2 | Rest], Sum) :- sum_list_elements([Elem1 + Elem2 | Rest], Sum).
sum_list_elements(_, 0).

get_happy_score(UserMemory, Score) :- 
    dict_keys(UserMemory, Keys),
    findall(Occurences, 
                (member(WordKey, Keys), 
                words(WordKey, KeyTokens),
                member(SingleToken, KeyTokens), 
                happy(SingleToken),
                get_value(UserMemory, WordKey, Occurences)), 
            HappyWordsCount),
    sum_list_elements(HappyWordsCount, TempScore), !,
    Score is TempScore.

% get_happy_score(memory{'trist': 3, 'fericit': 2, 'vesel':1, 'haha':1 , 'bucuros':2}, X).

% get_happy_score(memory{'sunt trist': 3, 'esti fericit': 2, 'mergi vesel':1, 't haha':1 , 'a bucuros':2}, X).

% Atribuie un scor pentru tristețe (de cate ori au fost folosit cuvinte din predicatul sad(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie trist.
% get_sad_score/2
% get_sad_score(+UserMemory, -Score)
get_sad_score(UserMemory, Score) :- 
    dict_keys(UserMemory, Keys),
    findall(Occurences, 
                (member(WordKey, Keys), 
                words(WordKey, KeyTokens),
                member(SingleToken, KeyTokens), 
                sad(SingleToken),
                get_value(UserMemory, WordKey, Occurences)), 
            SadWordsCount),
    sum_list_elements(SadWordsCount, TempScore), !,
    Score is TempScore.

% Pe baza celor doua scoruri alege emoția utilizatorul: `fericit`/`trist`,
% sau `neutru` daca scorurile sunt egale.
% e.g:
% ?- get_emotion(memory{'sunt trist': 1}, Emotion).
% Emotion = trist.
% get_emotion/2
% get_emotion(+UserMemory, -Emotion)
get_emotion(UserMemory, Emotion) :- 
    get_sad_score(UserMemory, SadScore),
    get_happy_score(UserMemory, HappyScore),
    (==(SadScore, HappyScore), Emotion = neutru; 
    (<(SadScore, HappyScore), Emotion = fericit; 
    >(SadScore, HappyScore), Emotion = trist)).

% Atribuie un scor pentru un Tag (de cate ori au fost folosit cuvinte din lista tag(Tag, Lista))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să vorbească despre acel subiect.
% get_tag_score/3
% get_tag_score(+Tag, +UserMemory, -Score)
get_tag_score(Tag, UserMemory, Score) :- 
    dict_keys(UserMemory, Keys),
    findall(Occurences, 
                (tag(Tag, TagWords),
                member(SingleTagWord, TagWords),
                member(WordKey, Keys),
                words(WordKey, KeyTokens),
                member(SingleToken, KeyTokens),
                SingleTagWord == SingleToken,
                get_value(UserMemory, WordKey, Occurences)), 
            TagWordsCount),
    sum_list_elements(TagWordsCount, TempScore), !,
    Score is TempScore.

% Pentru fiecare tag calculeaza scorul și îl alege pe cel cu scorul maxim.
% Dacă toate scorurile sunt 0 tag-ul va fi none.
% e.g:
% ?- get_tag(memory{'joc fotbal': 2, 'joc box': 3}, Tag).
% Tag = sport.
% get_tag/2
% get_tag(+UserMemory, -Tag)
get_tag(UserMemory, Tag) :- 
    get_tag_score(film, UserMemory, FilmScore),
    get_tag_score(sport, UserMemory, SportScore),
    (==(FilmScore, SportScore), Tag = none; 
    (<(FilmScore, SportScore), Tag = sport; 
    >(FilmScore, SportScore), Tag = film)).
