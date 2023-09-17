	:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(settings)).
:- use_module(library(macros)).

:- setting(rows, positive_integer, 24, 'number of tty rows').
:- setting(columns, positive_integer, 24, 'number of tty columns').
:- setting(cursor_x, positive_integer, 1, 'cursor x position').
:- setting(cursor_y, positive_integer, 1, 'cursor y position').
:- setting(render_x, positive_integer, 1, 'render x position').
:- setting(filename, codes, ``, 'current filename').
:- setting(editor_rows, list, [], 'text rows of the file').
:- setting(editor_render, list, [], 'text rows to display').
:- setting(numrows, nonneg, 0, 'number of text rows').
:- setting(row_offset, nonneg, 0, 'text row offset').
:- setting(column_offset, nonneg, 0, 'text column offset').
:- setting(tabstop, positive_integer, 3, 'tab stop').
:- setting(status_message, codes, ``, 'current status message').
:- setting(status_message_time, float, 0.0, 'current status message timestamp').
:- setting(dirty, boolean, false, 'text file has been modified since last write').

#define(version, `0.0.1`).
#define(esc, 0'\x1b).
#define(ctrl_key(C), Replacement) :-
   Replacement is C /\ 0x1f.
#define(dcg(Goal), Codes) :-
   phrase(Goal, Codes).
#define(backspace, 127).
#define(enter, 0'\r).

identity(Goal) -->
   Goal.

esc -->
   "\x1b[".
esc(Code) -->
   esc, [Code].
esc(Code, Args) -->
   esc, sequence(identity, ";", Args), [Code].
erase_in_line -->
   esc(0'K).
erase_in_display(Arg) -->
   esc(0'J, [integer(Arg)]).
cursor_position -->
   esc(0'H).
cursor_position(Y, X) -->
   esc(0'H, [integer(Y), integer(X)]).
set_mode(Arg) -->
   esc(0'l, ["?", integer(Arg)]).
reset_mode(Arg) -->
   esc(0'h, ["?", integer(Arg)]).
cursor_forward(Arg) -->
   esc(0'C, [integer(Arg)]).
cursor_down(Arg) -->
   esc(0'B, [integer(Arg)]).
device_status_report(Arg) -->
   esc(0'n, [integer(Arg)]).
cursor_position_report(Y, X) -->
   esc(0'R, [integer(Y), integer(X)]).
arrow_up -->
   esc(0'A).
arrow_down -->
   esc(0'B).
arrow_right -->
   esc(0'C).
arrow_left -->
   esc(0'D).
page_up -->
   esc(0'~, [integer(5)]).
page_down -->
   esc(0'~, [integer(6)]).
home -->
   esc, home_.
home_ --> "1~".
home_ --> "7~".
home_ --> "H".
home_ --> "OH".
end -->
   esc, end_.
end_ --> "4~".
end_ --> "8~".
end_ --> "F".
end_ --> "OF".
delete -->
   esc(0'~, [integer(3)]).
select_graphic_rendition -->
   esc(0'm).
select_graphic_rendition(Arg) -->
   esc(0'm, [integer(Arg)]).

peek(C), [C] -->
   [C].
add(C), [C] --> [].

waiting(L, L) :-
   var(L).

read_key -->
   [C],
   {
      maplist(dif(C), [
         #ctrl_key(0'q),
         #ctrl_key(0'b),
         #ctrl_key(0's),
         #esc,
         #enter,
         #backspace,
         #ctrl_key(0'h),
         #ctrl_key(0'l)
      ]),
      insert_char(C)
   },
   main_loop.
read_key  -->
   [#esc],
   (  waiting
   -> []  % pressed escape
   ;  (add(#esc), read_escape)
   -> []
   ;  []
   ),
   main_loop.
read_key -->
   [#enter],
   main_loop.
read_key -->
   [#backspace],
   {
      setting(cursor_x, X),
      (  X =:= 1
      -> true
      ;  NewX is X - 1,
         set_setting(cursor_x, NewX),
         del_char
      )
   },
   main_loop.
read_key -->
   [#ctrl_key(0'h)],
   add(#backspace),
   main_loop.
read_key -->
   [#ctrl_key(0'l)],
   main_loop.
read_key -->
   [#ctrl_key(0's)],
   { save_file },
   main_loop.
read_key -->
   [#ctrl_key(0'b)],
   { prolog },
   main_loop.
read_key -->
   [#ctrl_key(0'q)],
   quit.
quit -->
   { setting(dirty, Dirty) },
   quit_dirty(Dirty).
quit_dirty(true) -->
   { set_status_message("no write since last change") },
   main_loop.
quit_dirty(false) -->
   quit_final.
quit_final -->
   { phrase_to_stream(clear_screen) },
   remainder(_).

read_escape -->
   #dcg(arrow_up),
   {
      setting(cursor_y, Y),
      NextY is max(1, Y - 1),
      set_setting(cursor_y, NextY),
      correct_cursor_x
   }.
read_escape -->
   #dcg(arrow_down),
   {
      setting(cursor_y, Y),
      setting(numrows, Rows),
      NextY is max(1, min(Rows + 1, Y + 1)),
      set_setting(cursor_y, NextY),
      correct_cursor_x
   }.
read_escape -->
   #dcg(arrow_left),
   {
      setting(cursor_x, X),
      (  X == 1, setting(cursor_y, Y), Y > 1
      -> NextY is Y - 1,
         set_setting(cursor_y, NextY),
         correct_cursor_x(99999)
      ;  NextX is max(1, X - 1),
         set_setting(cursor_x, NextX)
      )
   }.
read_escape -->
   #dcg(arrow_right),
   {
      setting(cursor_x, X),
      setting(cursor_y, Y),
      setting(editor_rows, Rows),
      (  nth1(Y, Rows, Row),
         length(Row, RowSize),
         setting(numrows, NumRows),
         X =:= RowSize + 1, Y =< NumRows
      -> NextY is Y + 1,
         set_setting(cursor_y, NextY),
         set_setting(cursor_x, 1)
      ;  correct_cursor_x(1)
      )
   }.
read_escape -->
   #dcg(page_up),
   {
      setting(rows, Rows),
      setting(row_offset, Offset),
      NextY is max(1, Offset - Rows + 1),
      set_setting(cursor_y, NextY)
   }.
read_escape -->
   #dcg(page_down),
   {
      setting(rows, Rows),
      setting(row_offset, Offset),
      setting(numrows, NumRows),
      NextY is min(NumRows + 1, Offset + 2 * Rows - 1),
      set_setting(cursor_y, NextY),
      correct_cursor_x
   }.
read_escape -->
   home,
   { set_setting(cursor_x, 1) }.
read_escape -->
   end,
   { correct_cursor_x(9999) }.
read_escape -->
   #dcg(delete),
   { del_char }.

correct_cursor_x :-
   correct_cursor_x(0).
correct_cursor_x(N) :-
   setting(cursor_y, Y),
   setting(editor_rows, Rows),
   (  nth1(Y, Rows, Row)
   -> length(Row, RowSize)
   ;  RowSize = 0
   ),
   setting(cursor_x, X),
   NextX is max(1, min(RowSize + 1, X+N)),
   set_setting(cursor_x, NextX).


write_bytes(Bytes) :-
   format("~s", [Bytes]).
phrase_to_stream(Goal) :-
   phrase(Goal, L),
   write_bytes(L).


foldl(Goal, List, V0, V) -->
  foldl_(List, Goal, V0, V).

foldl_([H | T], Goal, V0, V) -->
  call(Goal, H, V0, V1),
  foldl_(T, Goal, V1, V).
foldl_([], _, V, V) --> [].

cutof_right(N, Goal) -->
   {
      phrase(Goal, L),
      length(L, Len),
      Size is min(N, Len),
      length(CutOf, Size),
      append(CutOf, _, L)
   },
   CutOf.
cutof_left(N, Goal) -->
   {
      phrase(Goal, L),
      length(L, Len),
      Size is min(N, Len),
      length(CutOf, Size),
      append(_, CutOf, L)
   },
   CutOf.

fill(N, Char) -->
   { compare(Order, N, 0) },
   fill(Order, N, Char).
fill(=, _, _) --> [].
fill(>, N, Char) -->
   [Char],
   { N1 is N - 1 },
   fill(N1, Char).

offset(N, Goal) -->
   {
      phrase(Goal, L),
      length(L, Size),
      RealN is min(Size, N),
      length(Offset, RealN),
      append(Offset, Rest, L)
   },
   Rest.

left_center(N, LeftGoal, CenterGoal, FillChar) -->
   left_center_right(N, LeftGoal, CenterGoal, "", FillChar).
left_right(N, LeftGoal, RightGoal, FillChar) -->
   left_center_right(N, LeftGoal, "", RightGoal, FillChar).

left_center_right(N, LeftGoal, CenterGoal, RightGoal, FillChar) -->
   {
      phrase(LeftGoal, Left),
      phrase(CenterGoal, Center),
      phrase(RightGoal, Right),
      length(Left, LeftSize),
      length(Center, CenterSize),
      length(Right, RightSize),
      TotalFillSize is max(0, N - (LeftSize + CenterSize + RightSize)),
      FillSize is TotalFillSize // 2
   },
   cutof_left(N, (
      Left,
      fill(FillSize, FillChar),
      Center,
      fill(FillSize, FillChar),
      Right
   )).

draw_rows -->
   {
      setting(rows, NumRows),
      numlist(1, NumRows, Indices)
   },
   sequence(draw_row(NumRows), "\n", Indices).

draw_row(NumRows, N) -->
   {
      setting(numrows, NumTextRows),
      setting(row_offset, RowOffset),
      FileRow is N + RowOffset
   },
   (  { FileRow > NumTextRows }
   -> (  { N is NumRows // 3, NumTextRows == 0 }
      -> { setting(columns, NumColumns) },
         left_center(NumColumns, "~", ("Kilogic editor -- version ", #version), 0' )
      ;  "~", #dcg(erase_in_line)
      )
   ;  draw_row(FileRow)
   ).

draw_row(FileRow) -->
   {
      setting(editor_render, EditorRows),
      nth1(FileRow, EditorRows, Row),
      setting(column_offset, ColumnOffset),
      setting(columns, NumColumns)
   },
   cutof_right(NumColumns, (offset(ColumnOffset, Row), #dcg(erase_in_line))).

clear_screen -->
   #dcg(erase_in_display(2)),
   #dcg(cursor_position).

scroll :-
   scroll_axis(row_offset, cursor_y, rows),
   row_cx_to_rx,
   scroll_axis(column_offset, render_x, columns).

row_cx_to_rx :-
   setting(cursor_x, CX),
   setting(cursor_y, Y),
   setting(editor_rows, Rows),
   (  nth1(Y, Rows, Row)
   -> cx_to_rx(Row, 1, CX, 1, RX)
   ;  RX = CX
   ),
   set_setting(render_x, RX).
cx_to_rx(Row, CN, CX, RN, RX) :-
   compare(Order, CN, CX),
   cx_to_rx(Order, Row, CN, CX, RN, RX).
cx_to_rx(=, _, _, _, RX, RX).
cx_to_rx(>, _, _, _, RX, RX).
cx_to_rx(<, [Char | Row], CN, CX, RN, RX) :-
   char_cx_to_rx(Char, Row, CN, CX, RN, RX).
char_cx_to_rx(Char, Row, CN, CX, RN, RX) :-
   dif(Char, 0'\t),
   CN1 is CN + 1,
   RN1 is RN + 1,
   cx_to_rx(Row, CN1, CX, RN1, RX).
char_cx_to_rx(0'\t, Row, CN, CX, RN, RX) :-
   setting(tabstop, TabStop),
   RN1 is RN + (TabStop - (RN mod TabStop)) + 1,
   CN1 is CN + 1,
   cx_to_rx(Row, CN1, CX, RN1, RX).

scroll_axis(OffsetName, CursorName, AxisName) :-
   maplist(setting, [OffsetName, CursorName, AxisName], [Offset, Y, Axis]),
   (  Y =< Offset
   -> NewOffset is Y - 1
   ;  Y > Offset + Axis
   -> NewOffset is Y - Axis
   ;  NewOffset = Offset
   ),
   set_setting(OffsetName, NewOffset).

status_bar_left -->
   { setting(filename, Filename) },
   status_bar_filename(Filename).
status_bar_filename([Char | Chars]) -->
   [Char | Chars], " ",
   { setting(dirty, Dirty) },
   status_bar_dirty(Dirty).
status_bar_filename([]) -->
   "[No Name]".
status_bar_dirty(true) -->
   "+ ", status_bar_numrows.
status_bar_dirty(false) -->
   "- ", status_bar_numrows.
status_bar_numrows -->
   { setting(numrows, NumRows) },
   integer(NumRows).

draw_status_bar -->
   #dcg(select_graphic_rendition(7)),
   {
      setting(columns, Columns),
      setting(numrows, Rows),
      setting(cursor_y, Y)
   },
   left_right(Columns,
              status_bar_left,
              (integer(Y), "/", integer(Rows)), 0' ),
   #dcg(select_graphic_rendition).

draw_message_bar -->
   {
      setting(status_message, Message),
      setting(columns, Columns),
      get_time(CurrentTime),
      setting(status_message_time, MessageTime),
      Elapsed is CurrentTime - MessageTime,
      (  Elapsed > 5
      -> String = ""
      ;  String = Message
      )
   },
   cutof_right(Columns, (String, #dcg(erase_in_line))).

refresh_screen -->
   { scroll },
   #dcg(set_mode(25)),
   #dcg(cursor_position),
   draw_rows, "\n",
   draw_status_bar, "\n",
   draw_message_bar,
   {
      setting(render_x, X),
      setting(cursor_y, Y),
      setting(row_offset, RowOffset),
      RealY is Y - RowOffset,
      setting(column_offset, ColumnOffset),
      RealX is X - ColumnOffset
   },
   cursor_position(RealY, RealX),
   #dcg(reset_mode(25)).

init :-
   tty_size(Rows, Columns),
   RealRows is Rows - 2,
   set_setting(rows, RealRows),
   set_setting(columns, Columns).

insert_char(Char) :-
   setting(cursor_y, Y),
   setting(cursor_x, X),
   setting(editor_rows, Rows),
   setting(editor_render, Render),
   setting(numrows, NumRows),
   (  Y =:= NumRows + 1
   -> append(Rows, [``], AllRows),
      append(Render, [``], AllRender),
      set_setting(numrows, Y)
   ;  AllRows = Rows,
      AllRender = Render
   ),
   nth1(Y, AllRows, Row, RowsWithout),
   nth1(Y, AllRender, _, RenderWithout),
   nth1(X, NewRow, Char, Row),
   phrase(render_line(NewRow, 1), NewRender),
   nth1(Y, RowsWith, NewRow, RowsWithout),
   nth1(Y, RenderWith, NewRender, RenderWithout),
   NewX is X+1,
   set_setting(cursor_x, NewX),
   set_setting(editor_rows, RowsWith),
   set_setting(editor_render, RenderWith),
   set_setting(dirty, true).

del_char :-
   setting(cursor_y, Y),
   setting(cursor_x, X),
   setting(editor_rows, Rows),
   setting(editor_render, Render),
   setting(numrows, NumRows),
   (  Y =:= NumRows + 1
   -> true
   ;  nth1(Y, Rows, Row, RowsWithout),
      (  Row = []
      -> true
      ;  nth1(X, Row, _, NewRow)
      -> nth1(Y, Render, _, RenderWithout),
         phrase(render_line(NewRow, 1), NewRender),
         nth1(Y, RowsWith, NewRow, RowsWithout),
         nth1(Y, RenderWith, NewRender, RenderWithout),
         set_setting(editor_rows, RowsWith),
         set_setting(editor_render, RenderWith),
         set_setting(dirty, true)
      ;  true
      )
   ).



render_lines([Line | Lines]) -->
   { phrase(render_line(Line), Render) },
   [Render],
   render_lines(Lines).
render_lines([]) --> [].

render_line(Line) -->
   render_line(Line, 1).
render_line([], _) --> [].
render_line([Char | Line], N) -->
   render_char(Char, N, N1),
   render_line(Line, N1).
render_char(Char, N, N1) -->
   {
      dif(Char, 0'\t),
      N1 is N + 1
   },
   [Char].
render_char(0'\t, N, N1) -->
   {
      setting(tabstop, TabStop),
      N1 is ceil(N / TabStop) * TabStop + 1,
      TabSize is N1 - N,
      length(Tab, TabSize),
      maplist(=(0' ), Tab)
   },
   Tab.

read_lines(Stream) -->
   { read_line_to_codes(Stream, Line) },
   read_lines(Line, Stream).
read_lines(Line, Stream) -->
   { dif(end_of_file, Line) },
   [Line],
   read_lines(Stream).
read_lines(end_of_file, _) -->
   [].

open_file(File) :-
   open(File, read, Stream),
   phrase(read_lines(Stream), Rows),
   close(Stream),
   phrase(render_lines(Rows), Render),
   length(Rows, NumRows),
   write_to_codes(File, FileCodes),
   set_setting(filename, FileCodes),
   set_setting(numrows, NumRows),
   set_setting(editor_rows, Rows),
   set_setting(editor_render, Render).
reset_file :-
   set_setting(editor_rows, []),
   set_setting(editor_render, []),
   set_setting(filename, ``),
   set_setting(cursor_x, 1),
   set_setting(cursor_y, 1),
   set_setting(numrows, 0).
save_file :-
   setting(filename, Filename),
   save_file(Filename).
save_file([]).
save_file([Char | Chars]) :-
   Filename = [Char | Chars],
   setting(editor_rows, Rows),
   phrase(sequence(identity, "\n", Rows), L),
   open(Filename, write, Stream),
   format(Stream, "~s", [L]),
   close(Stream),
   set_setting(dirty, false),
   set_status_message((Filename, " written to disk")).

set_status_message(Goal) :-
   phrase(Goal, L),
   set_setting(status_message, L),
   get_time(Time),
   set_setting(status_message_time, Time).

editor(File) :-
   open_file(File),
   editor_.
editor :-
   reset_file,
   editor_.
editor_ :-
   init,
   set_status_message(">^.^<"),
   with_tty_raw(phrase_from_stream(main_loop, current_input)).

debug_input, [C] -->
   [C],
   {
      ( code_type(C, cntrl)
      -> debug(main_loop, "~d~n", [C])
      ; debug(main_loop, "~c~n", [C])
      )
   }.

main_loop -->
   { phrase_to_stream(refresh_screen) },
   debug_input,
   read_key.

read_cursor_position(Rows, Columns) -->
   { phrase_to_stream((device_status_report(6), "\n")) },
   read_cursor_postion_(Rows, Columns).
read_cursor_postion_(Rows, Columns) -->
   cursor_position_report(Rows, Columns).
