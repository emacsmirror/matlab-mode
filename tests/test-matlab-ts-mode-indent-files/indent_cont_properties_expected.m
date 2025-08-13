% -*- matlab-ts -*-

classdef indent_cont_properties

    properties
        p1 = {
               1 2;
               3 4
             }
    end

    methods
        function f1(obj, a)
            arguments
                obj    %#ok<INUSA>
                a ...
                    { ...
                      mustBeReal ...
                    }
            end
            disp(a)
        end

        function f2(p1, p2)
            arguments
                p1 string {mustBeScalarOrEmpty}
                p2 double {...
                            mustBeReal ...
                          } = 0
            end
            disp(p1)
            disp(p2)
        end
    end
end
