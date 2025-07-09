% -*- matlab-ts -*-
classdef indent_keywords
    properties
        % comment here
        p1 = 0
        p2 = ...
            0

        % comment here

        %{
          foo
        %}
    end
    methods
        function method1(in)

            arguments
                in (1,1) double
            end

            global gVar1 gVar2
            global pVar1 pVar2

            try


                switch in
                  case 10
                    disp('10');


                    disp('11');
                  otherwise

                    disp('~10');

                end
            catch me
                rethrow(me)
            end

            j = 0;
            for n = 1:in
                if mod(n, 5)
                    x = 1;
                    continue
                elseif mod(n, 7)
                    continue
                else
                    j = j + 1;
                end
                disp(['Divisible by 5 or 7 : ' num2str(n)])
            end

            x = 0
            while x < 10
                x = x + 1;
            end
        end

        function method2()
            n = 200;
            A = 500;
            a = zeros(1,n);
            parfor i = 1:n
                a(i) = max(abs(eig(rand(A))));
            end

            return
        end
    end

    events
        e1
        e2
        % comment
    end

    enumeration
        one
        two
        % comment
    end
end
