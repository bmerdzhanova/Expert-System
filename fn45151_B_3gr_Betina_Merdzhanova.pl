main:-
    intro,
    reset_answers,
    find_species(Species),
    find_animal(Animal),
    my_describe(Species),
    my_describe(Animal),nl.

intro:-
    write('Animal Classification Expert System'),nl,
    write('To answer, input the number shown next to each answer, followed by a dot (.)'), nl, nl.

find_species(Species):-
    species(Species),!.

find_animal(Animal):-
    animal(Animal),!.

:- dynamic(progress/2).

reset_answers :-
  retract(progress(_, _)),
  fail.
reset_answers.

species(mammal):-
    has_hair(yes).

species(mammal):-
    gives_milk(yes).

species(carnivore):-
    eats_meat(yes).

species(carnivore):-
    has_pointed_teeth(yes),
    has_claws(yes),
    has_forward_eyes(yes).

species(ungulate):-
    species(mammal),
    chews_cud(yes).

animal(cheetah):-
    species(mammal),
    species(carnivore),
    has_tawny_color(yes),
    has_dark_spots(yes).

animal(tiger):-
    has_tawny_color(yes),
    has_black_stripes(yes),
    species(mammal),
    species(carnivore).

animal(giraffe):-
    has_dark_spots(yes),
    has_long_neck(yes),
    species(mammal),
    species(ungulate).

animal(zebra):-
    has_black_stripes(yes),
    species(ungulate).

question(has_hair):-
    write('Does the animal have hair?'),nl.

question(gives_milk):-
    write('Does the animal give milk?'),nl.

question(eats_meat):-
    write('Does the animal eat meat?'),nl.

question(has_pointed_teeth):-
    write('Does the animal have pointed teeth?'),nl.

question(has_claws):-
    write('Does the animal have claws?'),nl.

question(has_forward_eyes):-
    write('Does the animal have forward eyes?'),nl.

question(chews_cud):-
    write('Does the animal have chew cud?'),nl.

question(has_tawny_color):-
    write('Does the animal have tawny color?'),nl.

question(has_dark_spots):-
    write('Does the animal have dark spots?'),nl.

question(has_long_neck):-
    write('Does the animal have a long neck?'),nl.

question(has_black_stripes):-
    write('Does the animal have black stripes?'),nl.

my_answer(yes):-
    write('Yes').

my_answer(no):-
    write('No').

my_describe(mammal):-
    write('Mammal'), nl,
    write('Mammals have hair and give milk'),nl.

my_describe(carnivore):-
    write('Carnivore'),nl,
    write('Carnivores eat meat, have pointed teeth, have claws and forward eyes.'),nl.

my_describe(ungulate):-
    write('Ungulate'),nl,
    write('Ungulates have hooves and chew out'),nl.

my_describe(cheetah):-
    write('Cheetah'),nl,
    write('Cheetahs are carnivore mammals that have tawny color and dark spots.'),nl.

my_describe(tiger):-
    write('Tiger'),nl,
    write('Tigers are carnivore mammals that have tawny color and black stripes.'),nl.

my_describe(giraffe):-
    write('Giraffe'), nl,
    write('Giraffes are ungulate that have dark spots and long neck.'),nl.

my_describe(zebra):-
    write('Zebra'),nl,
    write('Zebras are ungulate taht have black stripes.'),nl.

has_hair(Answer):-
    progress(has_hair, Answer).

has_hair(Answer):-
    \+ progress(has_hair,_),
    my_ask(has_hair, Answer, [yes, no]).

gives_milk(Answer):-
    progress(gives_milk, Answer).

gives_milk(Answer):-
    \+ progress(gives_milk,_),
    my_ask(gives_milk, Answer, [yes, no]).

eats_meat(Answer):-
    progress(eats_meat, Answer).

eats_meat(Answer):-
    \+ progress(eats_meat,_),
    my_ask(eats_meat, Answer, [yes,no]).

has_pointed_teeth(Answer):-
    progress(has_pointed_teeth, Answer).

has_pointed_teeth(Answer):-
    \+ progress(has_pointed_teeth,_),
   my_ask(has_pointed_teeth, Answer, [yes, no]).

has_claws(Answer):-
    progress(has_claws, Answer).

has_claws(Answer):-
    \+ progress(has_claws,_),
    my_ask(has_claws, Answer, [yes,no]).

has_forward_eyes(Answer):-
    progress(has_forward_eyes, Answer).

has_forward_eyes(Answer):-
    \+ progress(has_forward_eyes,_),
    my_ask(has_forward_eyes, Answer, [yes,no]).

chews_cud(Answer):-
    progress(chews_cud, Answer).

chews_cud(Answer):-
    \+ progress(chews_cud,_),
    my_ask(chews_cud, Answer, [yes, no]).

has_tawny_color(Answer):-
    progress(has_tawny_color, Answer).

has_tawny_color(Answer):-
    \+ progress(has_tawny_color,_),
    my_ask(has_tawny_color, Answer, [yes,no]).

has_dark_spots(Answer):-
    progress(has_dark_spots, Answer).

has_dark_spots(Answer):-
    \+ progress(has_dark_spots,_),
    my_ask(has_dark_spots, Answer, [yes,no]).

has_long_neck(Answer):-
    progress(has_long_neck, Answer).

has_long_neck(Answer):-
    \+ progress(has_long_neck,_),
    my_ask(has_long_neck, Answer, [yes, no]).

has_black_stripes(Answer):-
    progress(has_black_stripes, Answer).

has_black_stripes(Answer):-
    \+ progress(has_black_stripes,_),
    my_ask(has_black_stripes, Answer, [yes,no]).

answers([], _).
answers([First|Rest], Index) :-
  write(Index), write(' '), my_answer(First), nl,
  NextIndex is Index + 1,
  answers(Rest, NextIndex).


parse(0, [First|_], First).
parse(Index, [_|Rest], Response) :-
  Index > 0,
  NextIndex is Index - 1,
  parse(NextIndex, Rest, Response).

my_ask(Question, Answer, Choices) :-
  question(Question),
  answers(Choices, 0),
  read(Index),
  parse(Index, Choices, Response),
  asserta(progress(Question, Response)),
  Response = Answer.
