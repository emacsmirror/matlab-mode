% -*- matlab-ts -*-

var_types = {
              'bool';
              'int8';
              'uint8';
              'int16';
              'uint16';
              'int32';
              'uint32';
              'int64';
              'uint64';
              'int128';
              'uint128';
              'single';
              'double';
              'matrix(int32,2)';
              'matrix(bool,2)';
              'matrix(int32,3)';
              'matrix(bool,3)';
              'matrix(int32,100)';
            };

typesToVerify = {
                  % numeric, logical, and char scalars
                  logical(1),
                  int8(2),
                  int16(3),
                  double(2+2j),
                  'c',

                  % numeric, logical, and char matrix
                  logical(ones(3,3,3)),
                  int8(ones(3,3,3)),
                  uint8(ones(3,3,3)),
                  double(ones(3,3,3)+2j),

                  % char row vector.
                  'char',

                  % cell array
                  { 'c', 'e', 'l', 'l' },

                  % function handle
                  @() disp('function handle'),

                  % String
                  string(missing),
                  "my scalar string",
                  strings(1,2),

                  % comment here
                  dictionary("a","b"),
                  dictionary(0,1),
                  dictionary
                };

supportedRhsArgs = ...
    { ...
      struct('foo', 1), ...
      1, ...
    };
