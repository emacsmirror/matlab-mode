% -*- matlab-ts -*- %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - if conditions don't require a terminator %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% The following produces %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
%    ans = %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
% %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
%         2 %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
% %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
%    here %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>

a=1; %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
b=2; %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

if a'b' %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
     disp('here') %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

% MATLAB does not require a statement terminator after the if-condition, so, for example, you can %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% write: %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
% %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
%  if 1 < 2 a = 3; end %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
% %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
% Thus above is equivalent to: %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>

a=1; %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
b=2; %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

if a' %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    b' %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    disp('here') %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

% Note, the Code Analyzer suggests that a line terminator should be added. %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% Information on tree-sitter handling from %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% https://github.com/acristoffers/tree-sitter-matlab/issues/93 %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>

%{ %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

  Is there a look-ahead concept in tree-sitter? If you have an identifier followed by the single-quote, then the single-quote has to be a transpose, correct? Likewise for other constructs such as m(10:12,20:22)' where m a matrix and we're taking a slice of it, then transposing. Though, I'm not very confident on this observation. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>


  Not really. It's a bit complicated the notion of look-ahead in tree-sitter. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  This is how tree-sitter works (overly-simplified): %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  tree-sitter is both a parser and a lexer. A parser takes chars from a file and produces a series of tokens and a lexer takes a sequence of tokens and produces nodes (the tree you manipulate in Emacs). %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  The parser is written descriptively in grammar.js. You say what makes a token/node and not how to generate it. For example, word: /[a-zA-Z]+/ says that a sequence of letters should be tokenized as a (word), or punctuation: choice('.', ',', ';') to say that the chars ., , and ; should be tokenized as (punctuation). %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  You can then say that a group of tokens make a node, like %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  sentence: seq(repeat(word), punctuation) %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  which would make the input the book. be parsed as (sentence (word "the") (word "book") (punctuation ".")), for example. But then you create %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  exclamation: seq(repeat(word), "!") %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  and now the book! would be parsed as (exclamation (word "the") (word "book") "!"). In these cases, the parser would go as this (I'm going word-by-word, but the parser works char-by-char): %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  read "The", emit a (word) token; %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  read " ", ignore; %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  read "book", emit a (word) token; %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  read either "." or "!", and emit the appropriate token; %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  It has enough tokens to satisfy both sentence and exclamation, so it will backtrack and emit the full corresponding node, consuming the nodes it emitted so far before continuing with the parsing. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  The file src/parser.c is automatically generated from grammar.js, implementing a state-machine that does all parsing and lexing. But since there are some limitations with what that implementation can do, tree-sitter also offers the possibility of writing a src/scanner.c to complement (not replace) it. parser.c is then the internal scanner and scanner.c the external scanner. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  Code written in scanner.c does have the liberty of looking ahead, but there are still some constraints: tree-sitter will invoke the external scanner to produce a token, and if it does not produce one, it will invoke the internal scanner to produce one. When tokens are produced, it will try to backtrack and generate nodes (that is, the scanner has no say on the lexing at all, it can only tokenize). %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  When invoking the external scanner, not much information is provided about what is going on. The scanner is given a list of valid nodes it can produce at this point (which is the only way I have to somewhat contextualize the decision) and a saved state generated by the previous, successful attempt to tokenize right before this point. Unsuccessful attempts do not generate nor modify such saved state. The scanner can only see one char at time and can only move forward, but it can mark the start and end of the token it generates, and its type (what token it is/the token itself). %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  Let's now use as example something we fixed recently: %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  [0xDEADBEEFu64] %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  This is the entire file, and this is how the parsing goes: %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  The external scanner is called, and the following nodes are in the list of valid tokens: %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  COMMENT %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  LINE_CONTINUATION %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  COMMAND_NAME %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  SINGLE_QUOTE_STRING_START %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  DOUBLE_QUOTE_STRING_START %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  MULTIOUTPUT_VARIABLE_START %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  IDENTIFIER %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  And the following are marked as not valid: %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  - `ENTRY_DELIMITER` %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  - `FORMATTING_SEQUENCE` %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  - `ESCAPE_SEQUENCE` %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  - `STRING_CONTENT` %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  - `SINGLE_QUOTE_STRING_END` %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  - `DOUBLE_QUOTE_STRING_END` %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  - `CATCH_IDENTIFIER` %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  - `COMMAND_ARGUMENT` %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  The external scanner will see [ and will mark this as the token MULTIOUTPUT_VARIABLE_START, but will not emit the token yet: it will look ahead to find ]=. Since it's not there, it will not emit any token at all. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  The internal scanner kicks in and matches on the [ as the opening of a matrix and will emit the unnamed [ token. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  The external scanner will be called, but will not produce any token again, because the char it sees is a number. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  The internal scanner has the number rule that matches on hexadecimals, so it consumes 0xDEADBEEF and emits an unnamed token for it. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  The external scanner is called and emits an IDENTIFIER token for u64. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  The external scanner is called and produces nothing for ]. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  The internal scanner sees ] and produces an unnamed token. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  The lexer kicks in and tries to reduce ("[" "0xDEADBEEF" (identifier) "]") and fails, the matrix node requires [ seq(expression, ENTRY_DELIMITER) ] and it fould [ (expression) (expression) ] (no ENTRY_DELIMITER). That's why it was breaking inside matrices and cells but not anywhere else. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
  To fix it, we go back to 5. and parse the entire number (u64 included) as a single number node. I modified the number regex for that. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  For the transpose, the external scanner is necessary to look ahead and see if we have a transpose or a string. It works by a combination of 1) only having the string token as a valid one in certain scenarios, 2) by the requirement that a single-quoted string has to end on the same line and 3) that I have token to separate entries in many contexts. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  The place it used to break is, again, inside matrices and cells, where we can have a sequence of not-delimited expressions, so [A' B'] can be either (matrix (transpose (identifier)) (transpose (identifier))) or (matrix (identifer) (string)) (as humans is easy to see which one it is, but when all you see is ' without any context, that's hard). That's why I used the ENTRY_DELIMITER token, to use the scanner to say if that's a string or not. Now, the external scanner can see the ' and, if we had a space before it, it's a string, if not, it's a transpose. Again, not all tokes are valid at all points, and that matters a lot too in this identification. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  The problem if the if statement itself is that it sees if a < b c end and tokenizes as "if" (identifier) "<" (identifier) (identifier) (end-keyword), but then the lexer is doing: (if (condition (identifier)) (ERROR "<")), because it takes as little as it can and turns into the (condition) node, so it leaves the "<" token there and no rule will match it, as no expression or statement can start with that. It does not happen in multiple lines because then I force (condition) to be everything between "if" and "\n", so the new-line is what makes it lex correctly. I could not yet find a way to tell it to try to group as much as possible in the condition, and I cannot use the scanner for that either, since I would have to rewrite the entire grammar in the scanner to know when to finish the condition. %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  So, that's my not 100% accurate but hopefully comprehensible enough explanation of how it works. I hope I made things clearer instead of more confusing :) %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

%} %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
