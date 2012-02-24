NanoML is a simple eager functional programming language based on [MiniML](http://www.andrej.com/plzoo/html/miniml.html) from [The Programming Language Zoo](http://www.andrej.com/plzoo/)

NanoML written on [_F#_](http://en.wikipedia.org/wiki/F_Sharp_%28programming_language%29). Lexing and parsing done by fslex and fsyacc. Runtime is a simple [SECD virtual machine](http://en.wikipedia.org/wiki/SECD_machine).

NanoML has 4 types _int_, _float_, _bool_, (* -> *). Language hasn't type inference at all. But type checker statically check all types in program. 

Language support functions, values, recursion, closures.

Also language provides with simple interpreter shell.

Below is a classical example recursive factorial function:

	let fact = 
		fun f (n : int) : int =>
			if n = 0 then 1
			else n * f (n - 1)
		end;;



To see more NanoML code check _stdlib.nanoml_ which contains small set of common and useful functions.

To get full language syntax see _NanoMLParser.fsy_.