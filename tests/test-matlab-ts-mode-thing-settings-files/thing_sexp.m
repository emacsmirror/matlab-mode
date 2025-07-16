% -*- matlab-ts -*-

% (t-utils-xr "C-a" "C-n" "C-M-f")
classdef thing_sexp

    % (t-utils-xr "C-a" "C-n" "M-m" "C-M-f" "C-M-f" "C-M-f" "C-M-f")
    properties
        % (t-utils-xr "C-a" "C-n" "M-m" "C-M-f" "C-M-f")
        p1 = 0
        p2 = ...
            0

        % comment here

        %{
          foo
        %}
    end

    methods

        % (t-utils-xr "C-a" "C-n" "M-m" "C-M-f" "C-M-f")
        function method1(in1, in2)

            % (t-utils-xr "C-a" "C-n" "M-m" "C-M-f" "C-M-f" "C-M-f" "C-M-f" "C-M-f" "C-M-f" "C-M-f" "C-M-f"  "C-M-f")
            arguments
                in1 (1,1) double
                in2 (1,2) double
            end

            global gVar1 gVar2
            global pVar1 pVar2

            % (t-utils-xr "C-a" "C-n" "M-m" "C-M-f" "C-M-f" "C-M-f" "C-M-f" "C-M-f" "C-M-f")
            try
                % (t-utils-xr "C-a" "C-n" "M-m" "C-M-f")
                switch in
                  % (t-utils-xr "C-a" "C-n" "M-m" "C-M-f" "C-M-f" "C-M-f" "C-M-f")
                  case 10
                    disp('10');

                  case 11
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

            c = {'1', {2, 3}};

            x = 0
            while x < 10
                x = x + 1;
            end
            % (t-utils-xr "C-a" "C-p" "C-e" "C-M-b" "C-M-b" "C-M-b" "C-M-b" "C-M-b" "C-M-b" "C-M-b" "C-M-b"  "C-M-b")
        end

        function method2()

            % (t-utils-xr "C-a" "C-n" "M-m" "C-M-f" "C-M-f" "C-M-f" "C-M-f" "C-M-f")
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
        % (t-utils-xr "C-a" "C-n" "M-m" "C-M-f" "C-M-f")
        e1
        e2
        % comment
    end

    enumeration
        % (t-utils-xr "C-a" "C-n" "M-m" "C-M-f" "C-M-f")
        one
        two
        % comment
    end
end
