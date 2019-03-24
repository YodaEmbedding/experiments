[ Tests to see if two input characters are equal to each other.
  If they are, echo =.
  Otherwise, echo !.
]

Input characters into #1 and #2
@Input first character:\n
> ,.
@\n\n
@Input second character:\n
> ,.
@\n\n
<<

Take difference of two characters
> [->-<] <                                      sub #2 #2 #1 ; mov #1 0

Not equal sign
>>> [-] > [-] <<<<                              mov #3 0 ; mov #4 0
>> [>>+<] > [-<<                                if #2 ne 0 then
    >> +++++ +++++ + [ > +++ < - ] <<           mov #5 '!'
>]< <<                                          end if

Equal sign
>>> [-] > [-] <<<<                              mov #3 0 ; mov #4 0
>> [>>+<] >- [+<                                if #2 eq 0 then
    >> +++++ +++++ [ > +++++ + < - ] >+< <<     mov #5 '='
>>]<< <<                                        end if

Display resulting sign
@Sign result:\n
>>>>> . <<<<<                                   echo #5

@\n\nPROGRAM END\n
?                                               dump final register contents
