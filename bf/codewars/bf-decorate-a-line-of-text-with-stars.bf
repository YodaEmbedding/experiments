[
  https://www.codewars.com/kata/bf-decorate-a-line-of-text-with-stars

  Sets up memory tape in the following way:
  **_H*_e*_l*_l*_o*_W*_o*_r*_l*_d*_**\n
  where _ is any non-zero value, i.e. \u0001.

  To print "consecutive" characters, simply skip by 3 (>>>).
  To print "consecutive" stars, simply start at an offset of 1 (>) and then skip by 3 (>>>).
  To print newline, simply skip to end.
]

==== SETUP MEMORY TAPE ====

++++++[>+++++++<-]  make asterisk
>[>+>+>+<<<-]>      copy asterisk
>>>,[
  <[>>+>+<<<-]+>    copy asterisk
  >>>,              read character
]
<[>+>+<<-]+>        copy asterisk
>>++++++++++        make newline

==== PRINT OUTPUT ====

Print ******
[<]>>               reset to offset 1
[.>>>]              echo every 3
<<.                 echo newline

Print *text*
[<]>                reset to offset 0
[.>>>]              echo every 3
<.                  echo newline

Print ******
[<]>>               reset to offset 1
[.>>>]              echo every 3
