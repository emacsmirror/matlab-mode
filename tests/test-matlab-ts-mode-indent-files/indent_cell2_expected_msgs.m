% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

var_types = { %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
              'bool'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'int8'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'uint8'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'int16'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'uint16'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'int32'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'uint32'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'int64'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'uint64'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'int128'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'uint128'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'single'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'double'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'matrix(int32,2)'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'matrix(bool,2)'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'matrix(int32,3)'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'matrix(bool,3)'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'matrix(int32,100)'; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
            }; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

typesToVerify = { %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                  % numeric, logical, and char scalars %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  logical(1), %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  int8(2), %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  int16(3), %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  double(2+2j), %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  'c', %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>

                  % numeric, logical, and char matrix %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  logical(ones(3,3,3)), %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  int8(ones(3,3,3)), %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  uint8(ones(3,3,3)), %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  double(ones(3,3,3)+2j), %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>

                  % char row vector. %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
                  'char', %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>

                  % cell array %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  { 'c', 'e', 'l', 'l' }, %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>

                  % function handle %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  @() disp('function handle'), %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>

                  % String %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  string(missing), %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  "my scalar string", %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
                  strings(1,2), %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>

                  % comment here %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  dictionary("a","b"), %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  dictionary(0,1), %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                  dictionary %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                }; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

supportedRhsArgs = ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
    { ... %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
      struct('foo', 1), ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
      1, ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
    }; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
