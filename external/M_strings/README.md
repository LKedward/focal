# M_strings.f90 and associated files

## NAME

   M_strings - Fortran module for processing strings

## DESCRIPTION

This package is a self-contained version of the M_strings library from
the GPF (General Purpose Fortran) package that has been extracted for
those just interested in a library of string-related functions. In the
GPF package this library is intertwined with several other large modules.

    git clone https://github.com/urbanjost/M_strings.git
    cd M_strings/src
    # change Makefile if not using gfortran(1)
    make

This will compile the M_strings module and build all the example programs from
the document pages in the PROGRAMS/ sub-directory.

<?
<blockquote>
<table cellpadding="3">


<tr><td colspan="3"><b>CASE</b></td></tr>
<tr><td><a href="md/lower.3.md">        lower        </a></td><td></td><td> changes a string to lowercase over specified range</td></tr>
<tr><td><a href="md/upper.3.md">        upper        </a></td><td></td><td> changes a string to uppercase</td></tr>
<tr><td><a href="md/upper_quoted.3.md"> upper_quoted </a></td><td></td><td> converts string to majuscule skipping strings quoted per Fortran syntax rules</td></tr>

<tr><td colspan="3"><b>COMPARE</b></td></tr>
<tr><td><a href="md/matchw.3.md">          matchw    </a></td><td></td><td> compare given string for match to pattern which may contain wildcard characters</td></tr>
<tr><td><a href="md/isalnum.3.md">         isalnum , isalpha, isascii, isblank, iscntrl, isdigit, isgraph, islower, isprint, ispunct, isspace, isupper, isxdigit</a></td><td></td><td> test membership in subsets of ASCII set</td></tr>

<tr><td colspan="3"><b>EDITING</b></td></tr>
<tr><td><a href="md/join.3.md">    join                  </a></td><td></td><td> append CHARACTER variable array into a single CHARACTER variable with specified separator</td></tr>
<tr><td><a href="md/change.3.md">          change        </a></td><td></td><td> change old string to new string with a directive like a line editor</td></tr>
<tr><td><a href="md/modif.3.md">           modif         </a></td><td></td><td> emulate the MODIFY command from the line editor XEDIT</td></tr>
<tr><td><a href="md/replace.3.md">         replace       </a></td><td></td><td> function globally replaces one substring for another in string</td></tr>
<tr><td><a href="md/transliterate.3.md">   transliterate </a></td><td></td><td> replace characters from old set with new set</td></tr>
<tr><td><a href="md/reverse.3.md">         reverse       </a></td><td></td><td> Return a string reversed</td></tr>
<tr><td><a href="md/substitute.3.md">      substitute    </a></td><td></td><td> subroutine globally substitutes one substring for another in string</td></tr>

<tr><td colspan="3"><b>LENGTH</b></td></tr>
<tr><td><a href="md/len_white.3.md">       len_white     </a></td><td></td><td> get length of string trimmed of whitespace.</td></tr>
<tr><td><a href="md/merge_str.3.md">       merge_str     </a></td><td></td><td> pads strings to same length and then calls MERGE(3f)</td></tr>
<tr><td><a href="md/atleast.3.md">         atleast       </a></td><td></td><td> return string padded to at least specified length!!</td></tr>
<tr><td><a href="md/lenset.3.md">          lenset        </a></td><td></td><td> return string trimmed or padded to specified length</td></tr>
<tr><td><a href="md/stretch.3.md">         stretch       </a></td><td></td><td> return string padded to at least specified length</td></tr>

<tr><td colspan="3"><b>NONALPHA</b></td></tr>
<tr><td><a href="md/noesc.3.md">           noesc         </a></td><td></td><td> convert non-printable characters to a space.</td></tr>
<tr><td><a href="md/expand.3.md">          expand        </a></td><td></td><td> expand C-like escape sequences</td></tr>
<tr><td><a href="md/notabs.3.md">          notabs        </a></td><td></td><td> expand tab characters</td></tr>
<tr><td><a href="md/visible.3.md">         visible       </a></td><td></td><td> expand a string to control and meta-control representations</td></tr>

<tr><td colspan="3"><b>NUMERIC</b></td></tr>
<tr><td><a href="md/isnumber.3.md">        isnumber  </a></td><td></td><td> determine if a string represents a number</td></tr>
<tr><td><a href="md/listout.3.md">         listout   </a></td><td></td><td> expand a list of numbers where negative numbers denote range ends (1 -10 means 1 thru 10)</td></tr>
<tr><td><a href="md/s2v.3.md">     s2v               </a></td><td></td><td> function returns doubleprecision numeric value from a string</td></tr>
<tr><td><a href="md/s2vs.3.md">    s2vs              </a></td><td></td><td> given a string representing numbers return a numeric array</td></tr>
<tr><td><a href="md/getvals.3.md">         getvals   </a></td><td></td><td> read arbitrary number of REAL values from a character variable up to size of VALUES() array</td></tr>
<tr><td><a href="md/string_to_values.3.md"> string_to_values </a></td><td></td><td> read a string representing numbers into a numeric array</td></tr>
<tr><td><a href="md/v2s.3.md">     v2s               </a></td><td></td><td> return numeric string from a numeric value</td></tr>
<tr><td><a href="md/value_to_string.3.md"> value_to_string  </a></td><td></td><td> return numeric string from a numeric value</td></tr>
<tr><td><a href="md/string_to_value.3.md"> string_to_value  </a></td><td></td><td> subroutine returns numeric value from string</td></tr>

<tr><td colspan="3"><b>BASE</b></td></tr>
<tr><td><a href="md/base.3.md">         base        </a></td><td></td><td> convert whole number string in base [2-36] to string in alternate base [2-36]</td></tr>
<tr><td><a href="md/codebase.3.md">     codebase    </a></td><td></td><td> convert whole number in base 10 to string in base [2-36]</td></tr>
<tr><td><a href="md/decodebase.3.md">   decodebase  </a></td><td></td><td> convert whole number string in base [2-36] to base 10 number</td></tr>

<tr><td colspan="3"><b>QUOTES</b></td></tr>
<tr><td><a href="md/quote.3.md">           quote            </a></td><td></td><td> add quotes to string as if written with list-directed input</td></tr>
<tr><td><a href="md/unquote.3.md">         unquote          </a></td><td></td><td> remove quotes from string as if read with list-directed input</td></tr>

<tr><td colspan="3"><b>TOKENS</b></td></tr>
<tr><td><a href="md/delim.3.md">           delim            </a></td><td></td><td> parse a string and store tokens into an array</td></tr>
<tr><td><a href="md/split.3.md">           split            </a></td><td></td><td> parse string into an array using specified delimiters</td></tr>
<tr><td><a href="md/chomp.3.md">           chomp            </a></td><td></td><td> Tokenize a string, consuming it one token per call</td></tr>
<tr><td><a href="md/fmt.3.md">     fmt              </a></td><td></td><td> Tokenize a string, consuming it one token per call</td></tr>
<tr><td><a href="md/strtok.3.md">          strtok           </a></td><td></td><td> Tokenize a string</td></tr>

<tr><td colspan="3"><b>WHITESPACE</b></td></tr>
<tr><td><a href="md/adjustc.3.md">         adjustc          </a></td><td></td><td> center text</td></tr>
<tr><td><a href="md/compact.3.md">         compact          </a></td><td></td><td> converts contiguous whitespace to a single character (or nothing)</td></tr>
<tr><td><a href="md/indent.3.md">          indent           </a></td><td></td><td> count number of leading spaces in a string</td></tr>
<tr><td><a href="md/nospace.3.md">         nospace          </a></td><td></td><td> remove all whitespace from input string</td></tr>
<tr><td><a href="md/crop.3.md">    crop             </a></td><td></td><td> trim leading blanks and trailing blanks from a string</td></tr>

<tr><td colspan="3"><b>ARRAY and C</b></td></tr>
<tr><td><a href="md/c2s.3.md">     c2s         </a></td><td></td><td> convert C string pointer to Fortran character string</td></tr>
<tr><td><a href="md/s2c.3.md">     s2c         </a></td><td></td><td> convert character variable to array of characters with last element set to null</td></tr>
<tr><td><a href="md/switch.3.md">  switch      </a></td><td></td><td> converts between CHARACTER scalar and array of single characters</td></tr>

<tr><td colspan="3"><b>MISCELLANEOUS</b></td></tr>
<tr><td><a href="md/rotate13.3.md">        rotate13         </a></td><td></td><td> apply trivial ROT13 encryption to a string</td></tr>
<tr><td><a href="md/msg.3.md">     msg              </a></td><td></td><td> converts any standard scalar type to a string</td></tr>
<tr><td><a href="md/describe.3.md">        describe         </a></td><td></td><td> returns a string describing the name of a single character</td></tr>
</table>
</body>
