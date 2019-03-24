[ Tests to see if two input characters are equal to each other.
  If they are, print =.
]

mov #3 '='
+++++ +++++ [ >>> +++++ + <<< - ] >>> +
<<<

mov #4 '!'
+++++ +++++ + [ >>>> +++ <<<< - ]

@Input first character:\n
> ,.
@\n\n
@Input second character:\n
> ,.
@\n\n
<<

sub #2 #2 #1    ; subtract
mov #1 0        ; destroy #1
> [->-<]
<

eq #1 #2 0      ; check if #2 equals 0
notl #2 #1      ; logical not
> +
> [< - < + >> [-]]
<< [- >> + <<]

@Equality:\n

if #1 "print #3"
> [>> . << [-]]

if #2 "print #4"
> [>> . << [-]]

@\n\nPROGRAM END
?               ; dump final register contents
