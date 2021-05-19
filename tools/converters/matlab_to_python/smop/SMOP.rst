.. contents:: 

============================
Lexical analysis and parsing
============================

Mostly resolving ambiguities in Matlab definition.

Reserved words
--------------

Reserved words are not reserved when used as fields.  So
``return=1`` is illegal, but ``foo.return=1`` is fine.

Round, square, and curly brackets
---------------------------------

In Python and in C it's simple -- function are called
using parentheses, and array subscripted with square
brackets.  In Matlab and in Fortran both are done with
parentheses, so there is no visual difference between
function call ``foo(x)`` and indexing array ``foo``.
      
Handling the white space
------------------------

There are four kinds of ws, each recognized by a dedicated rule:
NEWLINE, COMMENT, ELLIPSIS, and SPACES.  Only NEWLINE returns a token,
the three others silently discard their input.  NEWLINE collects
adjacent ``\n`` characters and returns a single SEMI token::

  def t_NEWLINE(t):
      r'\n+'
      t.lexer.lineno += len(t.value)
      if not t.lexer.parens and not t.lexer.braces:
            t.value = ";"
            t.type = "SEMI"
            return t

Comments come in two flavors -- regular and MULTILINE.
The regular comments are discarded, while MULTILINE
become doc strings, and are handled as expr statements.
regular consume everything from % or # up to but not
including \n::

          (%|\#).*

Multiline COMMENT rule drops leading blanks, then eats
everything from ``%`` or ``#`` up to the end of line,
`including` newline character.  COMMENT leaves one
newline character, or else funny things happen::

  @TOKEN(r"(%|\#).*")
  def t_COMMENT(t):
        if not options.do_magic or t.value[-1] != "!":
            t.lexer.lexpos = t.lexer.lexdata.find("\n",t.lexer.lexpos)
      
Multiline comments::

    (^[ \t](%|\#).*\n)+

The pattern for multi-line comments works only if
re.MULTILINE flag is passed to ctor.

Comments starting with ``%!`` have a special meaning TBD

ELLIPSIS is the matlab way for line continuation.  It
discards everything between ``...`` and the newline
character, including the trailing ``\n``::

  def t_ELLIPSIS(t):
        r"\.\.\..*\n"
        t.lexer.lineno += 1
  
SPACES discards one or more horisontal space characters.
Not clear what escape sequence is supposed to do::

    def t_SPACES(t):
       r"(\\\n|[ \t\r])+"
       
Disambiguating quotes
---------------------

a. A quote, immediately following a reserved word is always a
   ``STRING``. Implemented using inclusive state `afterkeyword`::
  
     t.type = reserved.get(t.value,"IDENT")
     if t.type != "IDENT" and t.lexer.lexdata[t.lexer.lexpos]=="'":
         t.lexer.begin("afterkeyword")

b. A quote, immediately following any of: (1) an alphanumeric
   charater, (2) right bracket, parenthesis or brace, or (3)
   another ``TRANSPOSE``, is a ``TRANSPOSE``.  Otherwise, it
   starts a string.  If the quote is separated from the term by
   line continuation (...), matlab starts a string, so these
   rules still hold::

     def t_TRANSPOSE(t):
         r"(?<=\w|\]|\)|\})((\.')|')+"
         # <---context ---><-quotes->
         # We let the parser figure out what that mix of quotes and
         # dot-quotes, which is kept in t.value, really means.
	 return t

Keyword ``end`` -- expression and statement 
-------------------------------------------

Any of: ``endwhile``, etc. are ``END_STMT``.  Otherwise, lonely ``end``
is a keyword ``END_EXPR``.  It is not allowed to be used as a variable,
except if appears inside subscripts, in which case it keeps the upper
bound of the corresponding dimension.  It is frequently used with
the auto-expanding array idiom::

    a(end+1) = b

Optional ``end`` statement as function terminator
-------------------------------------------------

Inconsistency between Matlab and Octave, solved
if the lexer effectively handles the whitespace:: 

         function : FUNCTION
                  | END_STMT SEMI FUNCTION

This usage is consistent with the other cases -- (1) statements start
with a keyword and are terminated by the SEMI token, and (2) the
lexer combines several comments, blanks, and other junk as one
SEMI token.  Compare parse.py rule for RETURN statement.

Semicolon as statement terminator, as column separator in matrices.
Comma, semicolon, and newline are statement terminators.  In
matrix expressiions, whitespace is significant and separates elements
just as comma does.

Matrix state
------------

In matrix state, consume whitespace separating two terms and
return a fake ``COMMA`` token.  This allows parsing ``[1 2 3]`` as
if it was ``[1,2,3]``.  Handle with care: ``[x + y]`` vs ``[x +y]``

Term T is::

  1. a name or a number
  2. literal string enclosed in single or double quotes
  3. (T) or [T] or {T} or T' or +T or -T

Terms end with::

  1. an alphanumeric charater \w
  2. single quote (in octave also double-quote)
  3. right parenthesis, bracket, or brace
  4. a dot (after a number, such as 3. 

The pattern for whitespace accounts for ellipsis as a whitespace, and
for the trailing junk.

Terms start with::

  1. an alphanumeric character
  2. a single or double quote,
  3. left paren, bracket, or brace and finally
  4. a dot before a digit, such as .3  .

TODO: what about curly brackets ???
TODO: what about dot followed by a letter, as in field
  
  [foo  .bar]
          
  t.lexer.lineno += t.value.count("\n")
  t.type = "COMMA"
  return t

================
Run-time support
================

libsmop
-------

Shared library ``libsmop.so`` implements classes ``matlabarray``,
 ``char``, and ``cellarray``, as well as some small functions::

  def abs(a): return numpy.abs(a)

Library ``libsmop.pyx`` is written in Cython, and is built as::
  
  cython libsmop.pyx
  gcc -Wno-cpp -I /usr/include/python2.7 -O2 -shared -o libsmop.so -fPIC libsmop.c

Once built, libsmop is imported::

  from libsmop import *

Matlab arrays differ from numpy arrays in many ways, and class
``matlabarray`` captures these differences.  There are two
natural places to call matlabarray.

First, around numeric constants, (both scalars and arrays),
string and cellarray literals, and upon return from any function
-- either library or user defined.  This looks terrible.

Another possibility is to wrap the function arguments inside the
function

Base-one indexing
-----------------

Following FORTRAN tradition, Matlab starts array indexing with one, not
with zero. Correspondingly, the last element of a N-element array is N,
not N-1.

C and FORTRAN data layout
-------------------------

Matlab matrix elements are ordered in columns-first order, better known
as FORTRAN order.  By default, numpy arrays use C layout.  Instances of
``matlabarray`` use FORTRAN layout, except if created empty, in which
case they use C layout.
    
+-----------------------+--------------------------------------+
| matlab                | numpy                                |
+=======================+======================================+
|::                     |::                                    |
|                       |                                      |
|  > reshape(1:4,[2 2]) |   >>> a=matlabarray([1,2,3,4])       |
|  1 3                  |   >>> reshape(a, [2,2])              |
|  2 4                  |   1 3                                |
|                       |   2 4                                |
+-----------------------+--------------------------------------+

>>> a=matlabarray([1,2,3,4])
>>> a.flags.f_contiguous
True
>>> a.flags.c_contiguous
False

>>> a=matlabarray()
>>> a.flags.c_contiguous
True
>>> a.flags.f_contiguous
False

Auto-expanding arrays
---------------------

Arrays are auto-expanded on out-of-bound assignment. Deprecated,
this feature is widely used in legacy code.  In smop, out-of-bound
assignment is fully supported for row and column vectors, and for
their generalizations having shape
    
    [1 1 ... N ... 1 1 1]

These arrays may be resized along their only non-singular dimension.
For other arrays, new columns can be added to F_CONTIGUOUS arrays, and
new rows can be added to C_CONTIGUOUS arrays.

+----------------------------+----------------------------------+
| matlab                     | numpy                            |
+============================+==================================+
|::                          |::                                |
|                            |                                  |
|  > a=[]                    |   >>> a=matlabarray()            |
|  > a(1)=123                |   >>> a[1]=123                   |
|  > a                       |   >>> a                          |
|  123                       |   123                            |
|                            |                                  |
+----------------------------+----------------------------------+

Create by update
----------------
   
In Matlab, arrays can be created by updating a non-existent array,
as in the following example:

    >>> clear a
    >>> a(17) = 42

This unique feature is not yet supported by smop, but can be
worked around by inserting assignments into the original matlab
code:

    >>> a = []
    >>> a(17) = 42

Assignment as copy
------------------
   
Array data is not shared by copying or slice indexing. Instead
there is copy-on-write.

Everything is a matrix
----------------------
   
There are no zero or one-dimensional arrays. Scalars are
two-dimensional rather than zero-dimensional as in numpy.

Single subscript implies ravel
---------------------------------
   
TBD

Boadcasting rules are different
-------------------------------
   
TBD

Boolean indexing
----------------

TBD

Character string literals and escape sequences
-----------------------------------------------
   
In Matlab, character strings are enclosed in single quotes, like
``'this'``, and escape sequences are not recognized::

        matlab> size('hello\n')
        1   7

There are seven (!) characters in ``'hello\n'``, the last two being
the backslash and the letter ``n``.

Two consecutive quotes are used to put a quote into a string::

        matlab> 'hello''world'
        hello'world

In Octave, there are two kinds of strings: octave-style (enclosed
in double quotes), and matlab-style (enclosed in single quotes).
Octave-style strings do understand escape sequences::

        matlab> size("hello\n")
        1   6

There are six characters in ``"hello\n"``, the last one being
the newline character.

Octave recognizes the same escape sequnces as C:: 

        \"  \a  \b  \f  \r  \t  \0  \v  \n  \\ \nnn \xhh

where n is an octal digit and h is a hexadecimal digit.

Finally, two consecutive double-quote characters become a single
one, like here::

  octave> "hello""world"
  hello"world

----------------------------------------------------------------------

===============
Data structures
===============

Empty vector, empty string, and empty cellarray
-----------------------------------------------

+----------------------------+----------------------------------+
| matlab                     | numpy                            |
+============================+==================================+
| ::                         | ::                               |
|                            |                                  |
|   > size([])               |   >>> matlabarray().shape        |
|   0 0                      |   (0, 0)                         |
|                            |                                  |
|   > size('')               |   >>> char().shape               |
|   0 0                      |   (0, 0)                         |
|                            |                                  |
|   > size({})               |   >>> cellarray().shape          |
|   0 0                      |   (0, 0)                         |
+----------------------------+----------------------------------+
   
    
Scalars are 1x1 matrices
------------------------

+----------------------------+----------------------------------+
| matlab                     | numpy                            |
+============================+==================================+
| ::                         | ::                               |
|                            |                                  |
|   > a=17                   |   >>> a=matlabarray(17)          |
|   > size(a)                |   >>> a.shape                    |
|   1 1                      |   1 1                            |
|                            |                                  |
+----------------------------+----------------------------------+
   
Character string literals
-------------------------

Matlab strings inherit their behavior from Matlab numeric arrays.  This
includes base-1 indexing, Fortran data order, and some unexpected
features, such as auto-expand on out of bound assignment (Matlab strings
are mutable objects).  Unless we know better, Matlab string literals
should be translated to instances of class ``char``, which inherits from
``matlabarray``.

+----------------------------+----------------------------------+
| matlab                     | numpy                            |
+============================+==================================+
| ::                         | ::                               |
|                            |                                  |
|   > s='helloworld'         |   >>> s=char('helloworld')       |
|   > size(s)                |   >>> print size_(s)             |
|   1 10                     |   (1,10)                         |
|   > s(1:5)='HELLO'         |   >>> s[1:5]=char('HELLO')       |
|   > s                      |   >>> print s                    |
|   HELLOworld               |   HELLOworld                     |
|   > resize(s,[2 5])        |   >>> print resize_(s,[2,5])     |
|   HELLO                    |   HELLO                          |
|   world                    |   world                          |
+----------------------------+----------------------------------+

Row vectors
-----------       

Rows are matrices whose size is [1 N].  When concatenated, rows are
joined along the first dimension, so concatenating two row vectors
of length M and N yields a row vector of length M+N.
    
+----------------------------+----------------------------------+
| matlab                     | numpy                            |
+============================+==================================+
| ::                         | ::                               |
|                            |                                  |
|  > s=[1 2 3]               |   >>> s=matlabarray([1,2,3])     |
|  > t=[4 5 6]               |   >>> t=matlabarray([4,5,6])     |
|  > u=[s t]                 |   >>> print concat([s,t])        |
|                            |   1 2 3 4 5 6                    |
+----------------------------+----------------------------------+

String concatenation
--------------------

String concatenation is consistent with row vectors concatenation
because string literals are row vectors
  
+----------------------------+----------------------------------+
| matlab                     | numpy                            |
+============================+==================================+
| ::                         | ::                               |
|                            |                                  |
|  > s='abc'                 |   >>> s = char('abc')            |
|  > t='ABC'                 |   >>> t = char('ABC')            |
|  > [s t]                   |   >>> print concat([s,t])        |
|  abcABC                    |   1 2 3 4 5 6                    |
+----------------------------+----------------------------------+

Column vector
-------------

+----------------------------+----------------------------------+
| matlab                     | numpy                            |
+============================+==================================+
|::                          |::                                |
|                            |                                  |
|  > a=[1;2;3]               |   >>> a=matlabarray([[1],        |
|                            |                      [2],        |
|                            |                      [2]])       |
|  > size(a)                 |   >>> a.shape                    |
|  3 1                       |   (3, 1)                         |
+----------------------------+----------------------------------+

Cell arrays
-----------

Cell arrays subclass matlabarray and inherit the usual matlab
array behaviour -- base-1 indexing, Fortran data order, expand on
out-of-bound assignment, etc. Unlike matlabarray, each element of
cellarray holds a python object.

+----------------------------+----------------------------------+
| matlab                     | numpy                            |
+============================+==================================+
|::                          |::                                |
|                            |                                  |
|  > a = { 'abc', 123 }      |   >>> a=cellarray(['abc',123])   |
|  > a{1}                    |   >>> a[1]                       |
|  abc                       |   abc                            |
+----------------------------+----------------------------------+

Cell arrays of strings
----------------------

In matlab, cellstrings are cell arrays, where each cell contains a
char object.  In numpy, class cellstring derives from matlabarray,
and each cell contains a native python string (not a char
instance).

+----------------------------+----------------------------------+
| matlab                     | numpy                            |
+============================+==================================+
|::                          |::                                |
|                            |                                  |
|  > a = { 'abc', 'hello' }  |   >>> a=cellstring(['abc',       |
|                            |                     'hello'])    |
|  > a{1}                    |   >>> a[1]                       |
|  abc                       |   abc                            |
+----------------------------+----------------------------------+

----------------------------------------------------------------------


Git hacks
---------
::

  git difftool --tool <tool>

where ``tool`` is ``meld`` or ``kdiff3``

Vim hacks
---------
::

    http://learnvimscriptthehardway.stevelosh.com
    https://www.ibm.com/developerworks/library/l-vim-script-1/index.html
    https://devhints.io/vimscript
    http://andrewscala.com/vimscript/                                                                                                                            

Pdf hacks
---------
::

   https://www.geeksforgeeks.org/working-with-pdf-files-in-python/

   
.. vim: tw=70:sw=2
