% -*- matlab-ts -*-

classdef indent_fcn_call_in_fcn_call_cont
    properties (Constant)
        tabularDs = tabularTextDatastore(some.namespace.someFunction( ...
                                             'argument1', 'argument2', 'argument3'), ...
                                         "TreatAsMissing", "NA", "MissingValue", 0, ...
                                         "SelectedVariableNames", {'ArrTime', 'DepDelay'});
    end
end
