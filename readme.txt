This is the base code of EControl Syntax Editor SDK http://econtrol.ru
Ported from Delphi to Lazarus, requires ATSynEdit package.

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

LICENSE
EControl author [Michael Zakharov] gave permission to use this code (modified for ATSynEdit) only inside **open source** projects. It's NOT ALLOWED to use this code in closed source. For usage in closed source, you must buy a license from EControl (for full code).
Copyright (c) 2004-2015, EControl
Copyright for added parts (c) 2015 Alexey Torgashin, UVviewsoft.com
