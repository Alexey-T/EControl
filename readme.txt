This is the base code of EControl Syntax Editor SDK (original site was http://econtrol.ru,
since 2024 the site is free to re-register by anybody).
Reworked and ported from Delphi to Lazarus by Alexey Yu. Torgashin (http://uvviewsoft.com),
now it requires ATSynEdit package for Lazarus.

Code contains:
- syntax parser
- grammar parser
- regex engine (parser requires it, it cannot use FPC RegExpr unit)
- lexer manager (linked list of lexers)
Code does not contain:
- SyntaxMemo control
- ecMemoStrings class
- popup listbox classes
- other visual controls

Code is modified, to work with ATSynEdit
(ecMemoStrings class deleted, replaced with ATStringBuffer class [same methods, much less code]).

LICENSE:

Original author, Michael Zakharov from econtrol.ru (since 2024 site is free to re-register by anybody),
gave permission to use this code (modified for ATSynEdit) only inside **open source** projects.
It's not allowed to use this code in closed source.
For usage in closed source code, you must buy a license from Michael Zakharov.
Michael in 2015 worked at the Yaroslavl State University, seems had an office there, per picture on
econtrol.ru site (deleted in 2024).

Copyright (c) 2004-2015, EControl
Copyright for changed/added parts (c) 2015-2026 Alexey Torgashin, UVviewsoft.com
